import pandas as pd
import numpy as np
import os
import pickle
from sklearn.model_selection import StratifiedKFold,train_test_split
from sklearn.preprocessing import MinMaxScaler,StandardScaler,OneHotEncoder
from xgboost.sklearn import XGBClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics
from sklearn.linear_model import LogisticRegression
from itertools import product
from scipy.spatial import distance
import math
import operator
import re
import warnings 
import sys


warnings.filterwarnings('ignore')

### dummie transformation
def dummie_transformation(df,cat_list):
    df_dummies=pd.get_dummies(df,columns=cat_list,prefix=cat_list,drop_first=True)
    #df_dummies.drop(cat_list,axis=1,inplace=True)
    return df_dummies

### categorize session hour

def cat_session_hour(x):
    if 8<=x<16:
        return 'Early Session'
    elif 16<=x<20:
        return 'Prime Session'
    else:
        return 'Late Session'

### truncate interval

def truncate_interval(x):
    if x >144:
        return 144.0
    elif x< 0 :
        return 0.0
    else:
        return x 

### transform categorical veriable

def cat_preprocessing(df):
    cat_list=['session_length','session_local_day','gender_request','session_period']
    drop_list=['interval_created_to_session_time','session_local_hour']
    df_trans=df.dropna(axis=0,how='any')
    df_trans['interval']=df_trans.interval_created_to_session_time.apply(truncate_interval)
    df_trans['session_period']=df_trans.session_local_hour.apply(cat_session_hour)
    df_trans_dummies=dummie_transformation(df=df_trans,cat_list=cat_list)
    df_trans_dummies.drop(drop_list,axis=1,inplace=True)
    
    return df_trans_dummies


### generate geo features

def geodata_generate(x_train,x_test):
    latitude=list(x_train.latitude.quantile([0.25,0.5,0.75]).values)
    longitude=list(x_train.longitude.quantile([0.25,0.5,0.75]).values)
    for n,(lat,long) in zip(range(9),list(product(latitude,longitude))):
        x_train['geo_'+str(n)]=euclidean_dist(x_train.latitude,x_train.longitude,lat,long)
        x_test['geo_'+str(n)]=euclidean_dist(x_test.latitude,x_test.longitude,lat,long)
        
    trainset=x_train.drop(['city_id','city','latitude','longitude'],axis=1)
    testset=x_test.drop(['city_id','city','latitude','longitude'],axis=1)
    return trainset,testset
     

### calculate the distance

def euclidean_dist(x1,y1,x2,y2):
    return round(np.sqrt(pow((x1-x2),2)+pow((y1-y2),2)),6)
    
### scale training and testing data

def scaler(x_train,x_test):
    g=x_train.columns.to_series().groupby(x_train.dtypes).groups
    df_type={k.name: v for k,v in g.items()}
    scale_list=list(df_type['int64'])+list(df_type['float64'])
    #print (scale_list)
    sc=MinMaxScaler()
    x_train_scaled=x_train.loc[:,[s for s in x_train.columns if s not in scale_list]]
    x_test_scaled=x_test.loc[:,[s for s in x_test.columns if s not in scale_list]]
    for column in scale_list:
        x_train_scaled[column]=sc.fit_transform(x_train.loc[:,column])
        x_test_scaled[column]=sc.transform(x_test.loc[:,column])
    return x_train_scaled,x_test_scaled

### mathematical transformation

def math_transform(df):
    g=df.columns.to_series().groupby(df.dtypes).groups
    df_type={k.name: v for k,v in g.items()}
    trans_list=list(df_type['int64'])+list(df_type['float64'])
    trans_list=[s for s in trans_list if s
                not in ['unfilled','is_couple','special_request']]
    #print (trans_list)
    df_trans=df.loc[:,[c for c in df.columns if c not in trans_list]]
    adjust=1e-10
    for column in trans_list: 
        df_trans[column+'_log']=np.log(df[column]+adjust)
        df_trans[column+'_inv']=1/(df[column]+adjust)
        df_trans[column+'_sqrt']=np.sqrt(df[column])
        df_trans[column+'_pow']=pow(df[column],2)
    return df_trans

### train the model use stratified cross validation

def auc_cross_validation(model,X_train,y_train,city_id,city_dict):
    X_train=np.array(X_train)
    y_train=np.array(y_train)
    skf=StratifiedKFold(n_splits=5,random_state=123,shuffle=False)
    score=[]
    for train_ind,test_ind in skf.split(X_train,y_train):
        train_data_x,test_data_x=X_train[train_ind],X_train[test_ind]
        train_data_y,test_data_y=y_train[train_ind],y_train[test_ind]
        
        model.fit(train_data_x,train_data_y)
        pred=model.predict_proba(test_data_x)[:,1]
        score.append(metrics.roc_auc_score(test_data_y,pred))
    #print ("Model for %s is: %0.5f"%(city_dict[city_id],np.mean(score)))
    return np.mean(score)
        
    
def stringfy(model):
    model_name=model.__class__.__name__
    return re.sub(r'[a-z]','',model_name)

### save city level model

def save_city_model(model,city_id):
    model_name=stringfy(model)
    #file_name=model_name+'_'+city_dict[city_id]+'.pkl'
    file_name=model_name+'_'+re.sub(r'/','_',city_dict[city_id])+'.pkl'
    save_path=os.getcwd()+'/'+file_name
    #print(save_path)
    if os.path.exists(save_path):
        os.remove(save_path)
        print('Old model is removed')
    try:
        outfile=open(save_path,'wb')
        pickle.dump(model,outfile)
        #print(1)
        outfile.close()
        print('Saved the Model as ',file_name)
    except:
        print('Failed to Save the Model')

### show the model in the directory

def show_current_model(**path):
    if path:
        pass
    else:
        path=os.getcwd()
    try:
        for file in os.listdir(path):
            if file.endswith('.pkl'):
                print(file)
    except:
        print('Invalid Path')

### load model

def load_model(city_id,**path):
    if path:
        pass
    else:
        path=os.getcwd()
    #file_end='_'+city_dict[city_id]+'.pkl'
    file_end='_'+re.sub(r'/','_',city_dict[city_id])+'.pkl'
    #print(path)
    #print(file_end)
    try:
        for file in os.listdir(path):
            if file.endswith(file_end):
                file_path=path+'/'+file
                infile=open(file_path,'rb')
                model=pickle.load(infile)
                infile.close()
                print ('%s Loaded'%file)
                return model
        if not model:
            print('Model Not Exist.')
    except:
        print('Invalid Path')

### save threshold in one file
        
def save_threshold(threshold_dict,**path):
    if path:
        save_path=path
    else:
        save_path=os.getcwd()+'/threshold.pkl'
    if os.path.exists(save_path):
        os.remove(save_path)
        print('Old threshold file removed.')
    try:    
        outfile=open(save_path,'wb')
        pickle.dump(threshold_dict,outfile)
        outfile.close()
        print('Successfully Saved Threshold.')
    except:
        print('Failed to Save Threshold.')

### load threshold
        
def load_threshold(city_id,**path):
    if path:
        save_path=path
    else:
        save_path=os.getcwd()+'/threshold.pkl'
    try:
        infile=open(save_path,'rb')
        thres_dict=pickle.load(infile)
        infile.close()
        print ('Threshold for %s has been loaded.'%city_dict[city_id])
        return thres_dict[city_id]
        
    except:
        print('Failed to load threshold')
        
### select the best threshold

def best_threshold_cv(model,X_train,y_train):
    X_train=np.array(X_train)
    y_train=np.array(y_train)
    skf=StratifiedKFold(n_splits=3,random_state=1234,shuffle=False)
    best_f1_score=-1
    best_recall=0
    best_threshold=0
    best_accuracy=0
    
    for threshold in np.arange(0.001,0.999,0.001):
        f1_score=[]
        accuracy_score=[]
        recall_score=[]
        for train_ind,test_ind in skf.split(X_train,y_train):
            test_data_x=X_train[test_ind]
            test_data_y=y_train[test_ind]
            p1=np.where(model.predict_proba(test_data_x)[:,1]>=threshold,1,0)
            f1_score.append(metrics.f1_score(test_data_y,p1))
            recall_score.append(metrics.recall_score(test_data_y,p1))
            accuracy_score.append(metrics.accuracy_score(test_data_y,p1))
        current_f1_score=np.mean(f1_score)
        current_recall_score=np.mean(recall_score)
        current_accuracy_score=np.mean(accuracy_score)
        if (current_f1_score>best_f1_score):
            best_recall=current_recall_score
            best_f1_score=current_f1_score
            best_accuracy=current_accuracy_score
            best_threshold=threshold
    #print('Best Threshold:%.6f, Best Accuracy:%.6f, Best F1 Score:%.6f, Best Recall: %.6f'%(best_threshold,best_accuracy,best_f1_score,best_recall))
    return best_threshold,best_accuracy,best_f1_score,best_recall





### main

if __name__=='__main__':
    train_filename='fill_rate_data_train.csv'
    test_filename='fill_rate_data_test.csv'

    ### preprocessing training data
    data_fill_train=pd.read_csv(train_filename)
    #data_fill_train=data_fill_train.dropna(axis=0,how='any')
    #data_fill_train['interval']=data_fill_train.interval_created_to_session_time.apply(truncate_interval)
    #data_fill_train['session_period']=data_fill_train.session_local_hour.apply(cat_session_hour)

    ##cat list
    #cat_list=['session_length','session_local_day','gender_request','session_period']
    #data_fill_dummies_train=dummie_transformation(df=data_fill_train,cat_list=cat_list)
    ## drop list
    #drop_list=['interval_created_to_session_time','session_local_hour']
    #data_fill_dummies_train.drop(drop_list,axis=1,inplace=True)

    data_fill_dummies_train=cat_preprocessing(data_fill_train)


    ### init city info
    city_dict=dict(list(set(zip(data_fill_train.city_id,data_fill_train.city))))
    city_list=list(city_dict.keys())[:1]
    model_list=[LogisticRegression(),XGBClassifier()]
    model_list_abb=dict(zip([stringfy(n) for n in model_list ],model_list))
    failed_city=[]
    threshold_dict=dict()
    n=0
    args=sys.argv[1:]
    for city_id in city_list:
        #data_fill_dummies_add_geo=geodata_generate(data_fill_dummies[data_fill_dummies.city_id==city_id])
        #data_fill_dummies_add_geo=math_transform(data_fill_dummies_add_geo)
        data_fill_dummies_city_train=data_fill_dummies_train[data_fill_dummies_train.city_id==city_id]
        predictors=[s for s in data_fill_dummies_city_train if s!='unfilled']
        X_Train=data_fill_dummies_city_train.loc[:,predictors]
        y_Train=data_fill_dummies_city_train.loc[:,'unfilled']
        X_train,X_test,y_train,y_test=train_test_split(X_Train,y_Train,test_size=0.2,random_state=123)
        X_train_add_geo,X_test_add_geo=geodata_generate(X_train,X_test)
        X_train_add_geo_trans=math_transform(X_train_add_geo)
        X_test_add_geo_trans=math_transform(X_test_add_geo)
        X_train_scaled,X_test_scaled=scaler(X_train_add_geo_trans,X_test_add_geo_trans)   
        auc_dict=dict()
        try:
            for model in model_list:
                auc_dict[stringfy(model)]=auc_cross_validation(model,X_train_scaled,y_train,city_id,city_dict)
                
            print ('Best Model for %s: '%city_dict.get(city_id),sorted(auc_dict.items(),key=operator.itemgetter(1),reverse=True)[0][0],'| Best Auc:',sorted(auc_dict.items(),key=operator.itemgetter(1),reverse=True)[0][1] )
            best_model=model_list_abb.get(sorted(auc_dict.items(),key=operator.itemgetter(1),reverse=True)[0][0],None)
            best_model.fit(np.array(X_train_scaled),np.array(y_train))
            best_threshold,best_accuracy,best_f1_score,best_recall=best_threshold_cv(best_model,X_train_scaled,y_train)
            threshold_dict[city_id]={'best_threshold':best_threshold,'best_accuracy':best_accuracy,'best_f1_score':best_f1_score,'best_recall':best_recall}
            save_city_model(best_model,city_id)
        except:
                print('Failed to build model for %s Sample size too small.'%city_dict[city_id])
                failed_city.append(city_dict[city_id])
        

        n+=1
        if n==len(city_list):
            
            print('''
            ###############################
                  Training Finished
            ###############################
            
            ''')
            save_threshold(threshold_dict)









    

