import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pylab as plt
from sklearn.preprocessing import StandardScaler,MinMaxScaler
import warnings
from sklearn import metrics
from sklearn.model_selection import train_test_split
from xgboost.sklearn import XGBClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier,ExtraTreesClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import GridSearchCV
import os
import pickle
import sys
from sklearn.preprocessing import OneHotEncoder


def data_preprocession(filename,Train):
    try :
        data=pd.read_csv(os.getcwd()+"/"+filename)
    except:
        print('Invalid Path')
    
    ### categorize the variable
    def cat_booking_hour(x):
        if x<=3:
            result='On_Demand'
        else:
            result='Adv_Booked'
        return result
    
    def cat_session_length(x):
        if x in [90,120]:
            result='long_session'
        else:
            result='short_session'
        return result
    def cat_client_rank(x):
        if x in ['AAA','AA','A']:
            result=1
        else:
            result=0
        return result

    def cat_city_maturity(x):
        if x in [2,5,3,4,13,12,9,1,14,15,17,18,10,16]:
            result='Grandfather_Market'
        elif x in [20,22,24,25,21,23,27,29,30,31,32,34,53,35]:
            result='Mature_Market'
        elif x in [57,56,58,38,28,33,36,40,45,37,41,52,46,43]:
            result='Medium_Age_Market'
        else:
            result='New_Market'
        return result

    def signtobook(x):
        if x <=0 :
            return 0
        elif x>=90:
            return 90
        else:
            return x
    ### categorical transformation 
    if Train:
        data['client_rank_label']=data['status'].apply(cat_client_rank)
    data['booking_hour_ahead']=data['first_hours_ahead'].apply(cat_booking_hour)
    data['market_maturity']=data['city_id'].apply(cat_city_maturity)
    data['signup_to_book_interval']=data['first_days_since_signup'].apply(signtobook)
    data['session_length_type']=data['session_length'].apply(cat_session_length)
    data['income_level']=data.median_income
    ### drop columns
    column_drop=['user_id','first_days_since_signup','first_hours_ahead','first_zip','city_id','median_income','session_length','status']
    data_merge=data.drop(column_drop,axis=1)
    ### create dummy variable
    column_dummy=['booking_hour_ahead','market_maturity','session_length_type']
    data_train_dummies=pd.get_dummies(data_merge,columns=column_dummy)
    ### make categorical variable n-1
    baseline=['booking_hour_ahead_Adv_Booked','market_maturity_New_Market','session_length_type_short_session']
    data_train_dummies_remove_base=data_train_dummies.drop(baseline,axis=1)
    return data_train_dummies_remove_base,data['user_id']

### data scaling
def min_max_scale(train_data,test_data):
    sc=MinMaxScaler()
    scale_list=['first_MT_quality','credit_amount','gift_amount','signup_to_book_interval','income_level']
    data_train_dummies_remove_base_scaled_train=train_data.loc[:,[s for s in data_train_dummies_remove_base.columns if s not in scale_list]]
    data_train_dummies_remove_base_scaled_test=test_data.loc[:,[s for s in data_train_dummies_remove_base.columns if s not in scale_list]]
    for candidate_column in scale_list:
        data_train_dummies_remove_base_scaled_train[candidate_column]=sc.fit_transform(train_data.loc[:,candidate_column])  
        data_train_dummies_remove_base_scaled_test[candidate_column]=sc.transform(test_data.loc[:,candidate_column])
    return data_train_dummies_remove_base_scaled_train,data_train_dummies_remove_base_scaled_test

### model fit

def get_params(model,param_list):
    model_name=model.__class__.__name__
    params=param_list.get(model_name,{})
    return params


def model_fit(model,init,param_list,X_train,y_train):
    X_train=np.array(X_train)
    y_train=np.array(y_train)    
    if init:
        params=get_params(model,param_list)
        model.set_params(**params)
    model.fit(X_train,y_train)
    return model

def auc_cross_validate(model,X_train,y_train):
    #score=cross_val_score(model,X_train,y_train,cv=5,random_state=123,scoring='roc_auc')
    X_train=np.array(X_train)
    y_train=np.array(y_train)
    skf=StratifiedKFold(n_splits=5,random_state=1234,shuffle=False)
    score=[]
    for train_index,test_index in skf.split(X_train,y_train):
        train_data_x,test_data_x=X_train[train_index],X_train[test_index]
        train_data_y,test_data_y=y_train[train_index],y_train[test_index]
        #model.fit(train_data_x,train_data_y)
        pred=model.predict_proba(test_data_x)[:,1]
        score.append(metrics.roc_auc_score(test_data_y,pred))
    return (np.mean(score))
    
def create_new_features(model,X_train,X_test):
    one=OneHotEncoder()
    X_train=np.array(X_train)
    X_test=np.array(X_test)
    if model.__class__.__name__!='XGBClassifier':
        print ('Choose XGBClassifier')
        return 0
    else:
        train_new_feature=one.fit_transform(model.apply(X_train)).toarray()
        test_new_feature=one.transform(model.apply(X_test)).toarray()
        X_train_new=np.hstack([X_train,train_new_feature])
        X_test_new=np.hstack([X_test,test_new_feature])
    return X_train_new,X_test_new

def find_best_params(model,X_train,y_train,PARAM_GRID,cv,scoring,verbose):
    X_train=np.array(X_train)
    y_train=np.array(y_train) 
    gv=GridSearchCV(model,PARAM_GRID[model.__class__.__name__],cv=cv,scoring=scoring,verbose=verbose)
    gv.fit(X_train,y_train)
    params=model.get_params()
    params.update(gv.best_params_)
    model.set_params(**params)
    #model.fit(X_train,y_train)
    auc=auc_cross_validate(model,X_train,y_train)
    return params,auc 

### save and load model

def save_model(model):
    model_name=model.__class__.__name__
    save_path=os.getcwd()+'/'+model_name+'.pkl'
    if os.path.exists(save_path):
        os.remove(save_path)
        print('Old File Removed.')
    outfile=open(save_path,'wb')
    try:
        pickle.dump(model,outfile)
        outfile.close()
        print ('Saved %s Successfully as %s'%(model_name,model_name+'.pkl'))
    except:
        print('Failed to Save the Model.')
        
def current_model(**save_path):
    try:
        if save_path:
            pass
        else:
            save_path=os.getcwd()
        for file in os.listdir(save_path):
            if file.endswith('.pkl'):
                print (file)
    except:
        print ('Invaid Path')
        
def load_model(model_name):
    try:
        load_path=os.getcwd()+'/'+model_name+'.pkl'
        infile=open(load_path,'rb')
        model=pickle.load(infile)
        infile.close()
        print('Model Loaded Succesfully.')
        return model
    except:
        print('Invalid Path')

def save_threshold(threshold_dict):
    #model_name=model.__class__.__name__
    save_path=os.getcwd()+'/threshold_dict.pkl'
    if os.path.exists(save_path):
        os.remove(save_path)
        print('Old File Removed.')
    
    try:
        outfile=open(save_path,'wb')
        pickle.dump(threshold_dict,outfile)
        outfile.close()
        print ('Saved Threshold Dict Successfully.')
    except:
        print('Failed to Save the Threshold Dict.')

def load_threshold():
    try:
        load_path=os.getcwd()+'/threshold_dict.pkl'
        infile=open(load_path,'rb')
        threshold_dict=pickle.load(infile)
        infile.close()
        print('Threshold Dict Loaded Succesfully.')
        return threshold_dict
    except:
        print('Invalid Path')


### select best threshold

def best_threshold_cv(model,X_train,y_train):
    best_threshold=0
    best_f1_score=-1
    best_recall=0
    best_accuracy_score=0
    skf=StratifiedKFold(n_splits=3,random_state=1234,shuffle=False)

    for threshold in np.arange(0.001,0.999,0.001):
    #print(threshold)
        f1_score=[]
        recall_score=[]
        accuracy_score=[]
        for train_index,test_index in skf.split(X_train,y_train):
            test_data_x=X_train[test_index]
            test_data_y=y_train[test_index]
            p1=np.where(model.predict_proba(test_data_x)[:,1]>=threshold,1,0)
            f1_score.append(metrics.f1_score(test_data_y,p1))
            recall_score.append(metrics.recall_score(test_data_y,p1))
            accuracy_score.append(metrics.accuracy_score(test_data_y,p1))
        current_score=np.mean(f1_score)
        current_recall=np.mean(recall_score)
        current_accuracy_score=np.mean(accuracy_score)
        if (current_score>=best_f1_score) & (current_recall>=0.38):
            best_f1_score=current_score
            best_threshold=threshold
            best_recall=current_recall
            best_accuracy_score=current_accuracy_score
            #a=accuracy_score
    #best_accuracy_score=metrics.accuracy_score(y_train,np.where(model.predict_proba(X_train)[:,1]>=best_threshold,1,0))
    return best_threshold,best_accuracy_score,best_f1_score,best_recall


def best_threshold_test(model,X_test,y_test):
    best_threshold=0
    best_f1_score=-1
    best_recall=0
    best_accuracy_score=0


    for threshold in np.arange(0.001,0.999,0.001):
    #print(threshold)
        current_score=0
        current_recall=0
        current_accuracy_score=0
        p1=np.where(model.predict_proba(X_test)[:,1]>=threshold,1,0)
        current_score=metrics.f1_score(y_test,p1)
        current_recall=metrics.recall_score(y_test,p1)
        current_accuracy_score=metrics.accuracy_score(y_test,p1)

        if (current_score>=best_f1_score):
            best_f1_score=current_score
            best_threshold=threshold
            best_recall=current_recall
            best_accuracy_score=current_accuracy_score
            #a=accuracy_score
    #best_accuracy_score=metrics.accuracy_score(y_train,np.where(model.predict_proba(X_train)[:,1]>=best_threshold,1,0))
    return best_threshold,best_accuracy_score,best_f1_score,best_recall

#### main

if __name__=="__main__":
    ### init params
    train_filename='clientdata.csv'
    test_filename='clienttest.csv'
    N_Rounds=100
    Random_state=12345
    INIT_GRID={'LogisticRegression':{'C':0.1,'penalty':'l1','class_weight': 'balanced'},
           'RandomForestClassifier':{'n_estimators':500,'max_depth':10,'max_features':.1,'min_samples_leaf':2,'random_state':Random_state},
            'XGBClassifier':{'base_score': 0.5,'colsample_bylevel': 1,'colsample_bytree': 1,'gamma': 0,'learning_rate': 0.1,'max_delta_step': 0,'max_depth': 3,'min_child_weight': 1,'missing': None,'n_estimators': 10,'nthread': -1,
            'objective': 'binary:logistic','reg_alpha': 0,'reg_lambda': 1,'scale_pos_weight': 1,'seed': 0,'silent': True,'subsample': 1},
            'ExtraTreesClassifier':{'n_estimators':500,'max_depth ':10,'max_features':.1,'min_samples_leaf':6,'random_state':Random_state}
           }

    PARAM_GRID={'LogisticRegression':{'C':[0.0001,0.001,0.01,0.1,2,],'class_weight': ['balanced',None],'penalty':['l1','l2']},
           'RandomForestClassifier':{'max_depth':[3,5,10],'max_features':['auto','sqrt'],'min_samples_leaf':[2,4,6,8]},
            'XGBClassifier':{'max_depth':[3,5,10],'learning_rate':[0.01,0.1,0.5,1],'min_child_weight':range(1,6,2),'gamma':[i/10.0 for i in range(0,5)]},
            'ExtraTreesClassifier':{'max_depth':[3,5,10],'max_features':['auto','sqrt'],'min_samples_leaf':[2,4,6,8]}
           }

    ### preprocess training dataset
    data_train_dummies_remove_base,data_train_user_id=data_preprocession(filename=train_filename,Train=True)
    #data_train_dummies_remove_base_scaled=min_max_scale(data_train_dummies_remove_base=data_train_dummies_remove_base)
    X_Train_not_scaled=data_train_dummies_remove_base.loc[:,data_train_dummies_remove_base.columns!='client_rank_label']
    y_Train=data_train_dummies_remove_base.loc[:,'client_rank_label']
    #y_Train=data_train_dummies_remove_base_scaled.loc[:,'client_rank_label']
    X_train_not_scaled, X_test_not_scale, y_train, y_test=train_test_split(X_Train_not_scaled,y_Train,test_size=0.2,random_state=Random_state)
    X_train,X_test=min_max_scale(train_data=X_train_not_scaled,test_data=X_test_not_scale)


    ### preprocess test dataset
    data_train_dummies_remove_base,data_test_user_id=data_preprocession(filename=test_filename,Train=False)
    #data_train_dummies_remove_base_scaled=min_max_scale(data_train_dummies_remove_base=data_train_dummies_remove_base)
    X_Test_not_scale=data_train_dummies_remove_base.loc[:,data_train_dummies_remove_base.columns!='client_rank_label']
    X_Train,X_Test=min_max_scale(train_data=X_Train_not_scaled,test_data=X_Test_not_scale)
    #y_Test=data_train_dummies_remove_base_scaled.loc[:,'client_rank_label']
    #X_train, X_test, y_train, y_test=train_test_split(X,y,test_size=0.2,random_state=Random_state)


    ### model fit
    args=sys.argv[1:]
    if args[0]=='--trainbasemodel':
        clf_xgb=model_fit(XGBClassifier(),init=True,param_list=INIT_GRID,X_train=X_train,y_train=y_train)
        skf1=StratifiedKFold(n_splits=5,random_state=1234,shuffle=False)
        best_params_xgb,best_auc_xgb=find_best_params(clf_xgb,X_train=X_train,y_train=y_train,PARAM_GRID=PARAM_GRID,cv=skf1,scoring='roc_auc',verbose=1)
        print('Find the XGB Best Params: ',best_params_xgb)
        print('The Best AUC for XGB is: ',best_auc_xgb)
        clf_xgb.set_params(**best_params_xgb)
        save_model(clf_xgb)
        del args[0]
    elif args[0]=='--loadbasemodel':
        clf_xgb=load_model(args[1])
        del args[0:2]

    else:
        print('Usage: python3 file [--trainbasemodel/loadbasemodel] [modelname] [--trainmodel/loadmodel] [modelname] [--trainthreshold/loadthreshold]')
        current_model()
        sys.exit(1)
    ### create new features
    clf_xgb.fit(np.array(X_train),np.array(y_train))
    X_train_new,X_test_new=create_new_features(clf_xgb,X_train=X_train,X_test=X_test)
    if args[0]=='--trainmodel':
        clf_LR=model_fit(LogisticRegression(),init=True,param_list=INIT_GRID,X_train=X_train_new,y_train=y_train)
        skf2=StratifiedKFold(n_splits=3,random_state=1234,shuffle=False)
        best_params_LR,best_auc_LR=find_best_params(clf_LR,X_train=X_train_new,y_train=y_train,PARAM_GRID=PARAM_GRID,cv=skf2,scoring='roc_auc',verbose=3)
        print('Find the LR Best Params: ',best_params_LR)
        print('The Best AUC for LR is: ',best_auc_LR)
        clf_LR.set_params(**best_params_LR)
        save_model(clf_LR)
        del args[0]
    elif args[0]=='--loadmodel':
        clf_LR=load_model(args[1])
        del args[0:2]
    else:

        print('Usage: python3 file [--trainbasemodel/loadbasemodel] [modelname] [--trainmodel/loadmodel] [modelname] [--trainthreshold/loadthreshold] [--predict]')
        current_model()
        sys.exit(1)
    ### select Threshold
    if args[0]=='--trainthreshold':
        clf_LR.fit(np.array(X_train_new),np.array(y_train))
        best_threshold,best_accuracy_score,best_f1_score,best_recall=best_threshold_cv(clf_LR,X_train=np.array(X_train_new),y_train=np.array(y_train))
        print("Best Threshold: ",best_threshold,' Best Accuracy Score',best_accuracy_score,' Best F1 Score',best_f1_score,' Best Recall',best_recall)
        threshold_dict={'Best_Threshold':best_threshold,'Best_Accuracy_Score':best_accuracy_score,'Best_F1_Score':best_f1_score,'Best_Recall':best_recall}
        save_threshold(threshold_dict)
        del args[0]
    elif args[0]=='--loadthreshold':
        threshold_dict=load_threshold()
        del args[0]
    else :
        print('Usage: python3 file [--trainbasemodel/loadbasemodel] [modelname] [--trainmodel/loadmodel] [modelname] [--trainthreshold/loadthreshold] [--predict]')
        sys.exit(1)

    ### predict high value client
    if args[0]=='--predict':
        clf_xgb.fit(np.array(X_Train),np.array(y_Train))
        X_Train_new,X_Test_new=create_new_features(clf_xgb,X_Train,X_Test)
        clf_LR.fit(np.array(X_Train_new),np.array(y_Train))
        prob=clf_LR.predict_proba(X_Test_new)[:,1]
        pred=np.where(prob>=threshold_dict['Best_Threshold'],1,0)
        output=pd.DataFrame({'User_id':data_test_user_id.values,'HVC_Probability':prob,'Over_the_Threshold':pred})
        if os.path.exists(os.getcwd()+'/prediction.csv'):
            os.remove(os.getcwd()+'/prediction.csv')
        output.to_csv('prediction.csv',index=False)
        print ('Prediction Finished')
    else:
        print('Usage: python3 file [--trainbasemodel/loadbasemodel] [modelname] [--trainmodel/loadmodel] [modelname] [--trainthreshold/loadthreshold] [--predict]')
        sys.exit(1)




