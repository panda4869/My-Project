import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler,MinMaxScaler
import warnings
from sklearn import metrics
from sklearn.model_selection import train_test_split
from xgboost.sklearn import XGBClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier,ExtraTreesClassifier,AdaBoostClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import GridSearchCV
from sklearn.tree import DecisionTreeClassifier
import os
import pdb
import pickle
import sys
#sys.path.append("../..")
import operator
import argparse
from sklearn.preprocessing import OneHotEncoder
import warnings 
import scipy as sp
import time
warnings.filterwarnings('ignore')

def data_preprocessing(filename,Train):
    data=pd.read_csv(os.path.join(os.getcwd(),filename))
    
    ### categorize the variable
    def cat_booking_hour(x):
        return 'On_Demand' if x <= 3 else "Adv_Booked"
    
    def booking_interval(x):
        if x<0:
            return 0
        elif x<=180:
            return x
        else:
            return 180
    

    def cat_client_rank(x):
        return 1 if x in ["AAA", "AA", "A"] else 0

    def cat_city_maturity(x):
        if x in [2,5,3,4,13,12,9,1,14,15,17,18,10,16]:
            return 'Grandfather_Market'
        elif x in [20,22,24,25,21,23,27,29,30,31,32,34,53,35]:
            return 'Mature_Market'
        elif x in [57,56,58,38,28,33,36,40,45,37,41,52,46,43]:
            return 'Medium_Age_Market'
        else:
            return 'New_Market'

    def signtobook(x):
        if x <=0 :
            return 0
        elif x>=90:
            return 90
        else:
            return x
    ### categorical transformation 

    if Train:
        data['client_rank_label'] = data['rank'].apply(cat_client_rank)

    data['booking_hour_ahead1'] = data['hours_ahead1'].apply(cat_booking_hour)
    data['booking_hour_ahead2'] = data['hours_ahead2'].apply(cat_booking_hour)
    data['market_maturity'] = data['city_id'].apply(cat_city_maturity)
    data['signup_to_book_interval'] = data['sign_up_to_book'].apply(signtobook)
    data['booking_interval']=data['interval_between_bookings'].apply(booking_interval)
    #data['session_length_type'] = data['session_length'].apply(cat_session_length)
    data['income_level'] = data.median_income
    ### drop columns
    column_drop = ['user_id', 'sign_up_to_book', 'hours_ahead1','hours_ahead2',  'city_id', 'median_income',\
                   'rank','interval_between_bookings']
    data_merge = data.drop(column_drop, axis = 1)
    ### create dummy variable
    column_dummy = ['booking_hour_ahead1','booking_hour_ahead2', 'market_maturity']
    data_train_dummies = pd.get_dummies(data_merge, columns = column_dummy,drop_first=True)
    ### make categorical variable n-1
    #baseline = ['booking_hour_ahead_Adv_Booked','market_maturity_New_Market','session_length_type_short_session']
    #data_train_dummies_remove_base = data_train_dummies.drop(baseline, axis = 1)
    return data_train_dummies,data['user_id']

### data scaling
def min_max_scale(train_data, test_data):
    sc = MinMaxScaler()
    scale_list = ['signup_to_book_interval','booking_interval','quality_score1','quality_score2','quality_score_diff',\
                  'revenue1','revenue2',\
                  'income_level','num_hvc']
    data_train_dummies_remove_base_scaled_train = train_data.loc[:,[s for s in train_data.columns if s not in scale_list]]
    data_train_dummies_remove_base_scaled_test = test_data.loc[:,[s for s in test_data.columns if s not in scale_list]]
    for candidate_column in scale_list:
        data_train_dummies_remove_base_scaled_train[candidate_column] = sc.fit_transform(train_data.loc[:,candidate_column])  
        data_train_dummies_remove_base_scaled_test[candidate_column] = sc.transform(test_data.loc[:,candidate_column])
    return data_train_dummies_remove_base_scaled_train,data_train_dummies_remove_base_scaled_test

### model fit

def get_params(model, param_list):
    model_name = model.__class__.__name__
    params = param_list.get(model_name, {})
    return params


def model_fit(model, init, param_list, X_train, y_train):
    X_train = np.array(X_train)
    y_train = np.array(y_train).reshape(-1)
    if init:
        params = get_params(model,param_list)
        model.set_params(**params)
    model.fit(X_train,y_train)
    return model

def auc_cross_validate(model,X_train,y_train):
    X_train = np.array(X_train)
    y_train = np.array(y_train).reshape(-1)
    skf = StratifiedKFold(n_splits = 3, random_state = 1234, shuffle = False)
    score = []
    for train_index,test_index in skf.split(X_train,y_train):
        train_data_x,test_data_x = X_train[train_index],X_train[test_index]
        train_data_y,test_data_y = y_train[train_index],y_train[test_index]
        model.fit(train_data_x,train_data_y)
        pred = model.predict_proba(test_data_x)[:,1]
        score.append(metrics.roc_auc_score(test_data_y,pred))
    return (np.mean(score))
    


def find_best_params(model,X_train,y_train,PARAM_GRID,cv,scoring,verbose):
    X_train = np.array(X_train)
    y_train = np.array(y_train).reshape(-1)
    gv = GridSearchCV(model, PARAM_GRID[model.__class__.__name__], cv = cv, scoring = scoring, verbose = verbose)
    gv.fit(X_train,y_train)
    params = model.get_params()
    params.update(gv.best_params_)
    model.set_params(**params)
    auc = auc_cross_validate(model,X_train,y_train)
    return params,auc

### save and load model

def save_model(model):
    model_name = model.__class__.__name__
    save_path = os.getcwd()+'/'+model_name+'.pkl'
    if os.path.exists(save_path):
        os.remove(save_path)
    outfile = open(save_path,'wb')
    try:
        pickle.dump(model,outfile)
        outfile.close()
    except Exception as e:
        print("Error in save_model")
        print(e)
        #newrelic.agent.record_exception(e)

    #s3_connection.upload_file(file_name = model_name+".pkl")

def current_model(**save_path):
    try:
        if save_path:
            pass
        else:
            save_path = os.getcwd()
        for file in os.listdir(save_path):
            if file.endswith('.pkl'):
                print (file)
    except:
        print ('Invaid Path')

def load_model(model_name):
    try:
        #s3_connection.download_file(model_name+".pkl")
        load_path = os.getcwd()+'/'+model_name+'.pkl'
        infile = open(load_path,'rb')
        model = pickle.load(infile)
        infile.close()
        print('Model Loaded Succesfully.')
        return model
    except Exception as e:
        print("Error in load_model")
        print(e)
        #newrelic.agent.record_exception(e)

def save_threshold(threshold_dict):
    model_name = "threshold_dict.pkl"
    save_path = os.getcwd()+'/' + model_name
    if os.path.exists(save_path):
        os.remove(save_path)
        print('Old File Removed.')
    
    try:
        outfile = open(save_path,'wb')
        pickle.dump(threshold_dict,outfile)
        outfile.close()
        print ('Saved Threshold Dict Successfully.')
        #s3_connection.upload_file(file_name = model_name)
    except:
        print('Failed to Save the Threshold Dict.')

def load_threshold():
    pickle_file = "threshold_dict.pkl"
    try:
        #s3_connection.download_file(pickle_file)
        load_path = os.getcwd()+'/' + pickle_file
        infile = open(load_path,'rb')
        threshold_dict = pickle.load(infile)
        infile.close()
        print('Threshold Dict Loaded Succesfully.')
        return threshold_dict
    except Exception as e:
        print("Error in load_threshold")
        print(e)


### select best threshold

def best_threshold_cv(model,X_train,y_train):
    best_threshold = 0
    best_f1_score = -1
    best_recall = 0
    best_accuracy_score = 0
    skf = StratifiedKFold(n_splits=3,random_state=1234,shuffle=False)
    X_train = np.array(X_train)
    y_train = np.array(y_train).reshape(-1)    

    for threshold in np.arange(0.001,0.999,0.001):
        f1_score = []
        recall_score = []
        accuracy_score = []
        for train_index,test_index in skf.split(X_train,y_train):
            test_data_x = X_train[test_index]
            test_data_y = y_train[test_index]
            p1 = np.where(model.predict_proba(test_data_x)[:,1]>=threshold,1,0)
            f1_score.append(metrics.f1_score(test_data_y,p1))
            recall_score.append(metrics.recall_score(test_data_y,p1))
            accuracy_score.append(metrics.accuracy_score(test_data_y,p1))
        current_score = np.mean(f1_score)
        current_recall = np.mean(recall_score)
        current_accuracy_score = np.mean(accuracy_score)
        if (current_score >= best_f1_score) & (current_recall >= 0.60):
            best_f1_score = current_score
            best_threshold = threshold
            best_recall = current_recall
            best_accuracy_score = current_accuracy_score

    return best_threshold,best_accuracy_score,best_f1_score,best_recall


#prepare stacking training set
def get_model_cv_preds(model,X_train,y_train,random_state):
    X_train=np.array(X_train)
    y_train=np.array(y_train).reshape(-1)
    model_name=model.__class__.__name__
    clf=load_model(model_name)
    skf=StratifiedKFold(n_splits=3,random_state=random_state,shuffle=False)
    stack_preds=list()
    cv_index=list()
    for train_index,test_index in skf.split(X_train,y_train):
        train_data_x,test_data_x=X_train[train_index],X_train[test_index]
        train_data_y,test_data_y=y_train[train_index],y_train[test_index]
        clf.fit(train_data_x,train_data_y)
        stack_preds.extend(list(clf.predict_proba(test_data_x)[:,1]))
        #stack_preds.extend(list(test_data_y))
        cv_index.extend(test_index)
    stack_preds=np.array(stack_preds)[sp.argsort(cv_index)]
    return stack_preds

#stakcing 
def stacking(models,X_train,y_train,X_test,test_user_id,random_state,train,prediction,PARAM_GRID):
    if train:
        generalizer=LogisticRegression()
        stage0_train=list()
        #stage0_predict=list()
        for model in models:
            #stage0_predict.append(model.fit(np.array(X_train),np.array(y_train).reshape(-1)))
            stage0_train.append(get_model_cv_preds(model,X_train,y_train,random_state))
        skf=StratifiedKFold(n_splits=3,shuffle=False,random_state=Random_State)
        best_g_params,best_g_auc=find_best_params(model=generalizer,cv=skf,PARAM_GRID=PARAM_GRID,scoring='roc_auc',verbose=1,X_train=np.array(stage0_train).T,y_train=y_train)
        print('Best Generalizer Parms: ',best_g_params)
        print('Best Generalizer AUC: ',best_g_auc)
        generalizer.set_params(**best_g_params)
        generalizer.fit(np.array(stage0_train).T,np.array(y_train).reshape(-1))
        save_model(generalizer)
        print('''
        ###################################
        Finished Training the Stacked Model
        ###################################''')
        
        best_threshold,best_accuracy_score,best_f1_score,best_recall=best_threshold_cv(generalizer,X_train=np.array(stage0_train).T,y_train=np.array(y_train).reshape(-1))
        threshold_dict={'Best_Threshold':best_threshold,'Best_Accuracy_Score':best_accuracy_score,'Best_F1_Score':best_f1_score,'Best_Recall':best_recall}
        print('Best Threshold: ',best_threshold,'Best Accuarcy Score: ',best_accuracy_score,'Best F1 Score: ',best_f1_score,'Best Recall: ',best_recall)
        save_threshold(threshold_dict)
        print('''
        ###############################
        Finished Training the Threshold
        ###############################''')
    if prediction:
        generalizer=load_model('LogisticRegression')
        threshold_dict=load_threshold()
        print('Best Threshold: ', threshold_dict['Best_Threshold'])
        stage1_test=list()
        for model in models:
            model_pred=load_model(model.__class__.__name__)
            model_pred.fit(np.array(X_train),np.array(y_train).reshape(-1))
            stage1_test.append(list(model_pred.predict_proba(np.array(X_test))[:,1]))
        
        #return generalizer.predict_proba(np.array(stage1_test).T)[1:]
        print (np.array(stage1_test).shape)
        prob=generalizer.predict_proba(np.array(stage1_test).T)[:,1]
        preds=np.where(prob>=threshold_dict['Best_Threshold'],1,0)
        output=pd.DataFrame({'user_id':test_user_id,'score':prob,'status':preds},columns=['user_id','score','status'])
        if os.path.exists('predict.csv'):
            os.remove('predict.csv')
        output.to_csv('predict.csv',index=False)
        print('''
        ########################
        Done running prediction
        ########################''')
    
        
        


if __name__=='__main__':

    train_filename='traindata.csv'
    test_filename='testdata.csv'
    Random_State=12345
    # initiate model 
    INIT_GRID={'LogisticRegression':{'C':0.1,'penalty':'l1','class_weight': 'balanced'},
           'RandomForestClassifier':{'n_estimators':500,'max_depth':10,'max_features':.1,'min_samples_leaf':2,'random_state':Random_State},
            'XGBClassifier':{'base_score': 0.5,'colsample_bylevel': 1,'colsample_bytree': 1,'gamma': 0,'learning_rate': 0.1,'max_delta_step': 0,'max_depth': 3,'min_child_weight': 1,'missing': None,'n_estimators': 10,'nthread': -1,
            'objective': 'binary:logistic','reg_alpha': 0,'reg_lambda': 1,'scale_pos_weight': 1,'seed': 0,'silent': True,'subsample': 1},
            'ExtraTreesClassifier':{'n_estimators':500,'max_depth':10,'max_features':.1,'min_samples_leaf':10,'random_state':Random_State},
            'DecisionTreeClassifier': {'max_features':0.8,'max_depth':10,'min_samples_leaf':10,'random_state':Random_State},
            'AdaBoostClassifier':{'learning_rate':0.1,'random_state':Random_State}
           }

    PARAM_GRID={'LogisticRegression':{'C':[0.0001,0.001,0.01,0.1],'class_weight': ['balanced',None],'penalty':['l1','l2']},
           'RandomForestClassifier':{'max_depth':[3,5,10],'max_features':['auto','sqrt'],'min_samples_leaf':[2,4,6,8]},
            'XGBClassifier':{'max_depth':[3,5,10],'learning_rate':[0.01,0.1,0.5,1],'min_child_weight':range(1,6,2),'gamma':[i/10.0 for i in range(0,5)]},
            'ExtraTreesClassifier':{'max_depth':[3,5,10],'max_features':['auto','sqrt'],'min_samples_leaf':[2,4,6,8]},
            'DecisionTreeClassifier': {'max_features':['auto','sqrt'],'max_depth':[3,5,10],'min_samples_leaf':[2,4,6,8]},   
            'AdaBoostClassifier':{'learning_rate':[0.01,0.1,1.],'n_estimators':[10,50,100]}
           }

    # preprocess the training set
    train_data_preprocess,train_data_user_id=data_preprocessing(train_filename,Train=True)
    x_Train_not_scaled=train_data_preprocess.loc[:,train_data_preprocess.columns!='client_rank_label']
    y_Train=train_data_preprocess.loc[:,train_data_preprocess.columns=='client_rank_label']
    x_train_not_scaled,x_test_not_scaled,y_train,y_test=train_test_split(x_Train_not_scaled,y_Train,test_size=0.2,random_state=Random_State)
    x_train,x_test=min_max_scale(x_train_not_scaled,x_test_not_scaled)
    # preprocess the testing set
    test_data_preprocess,test_data_user_id=data_preprocessing(test_filename,Train=False)
    x_Test_not_scaled=test_data_preprocess.loc[:,test_data_preprocess.columns!='client_rank_label']
    x_Train,x_Test=min_max_scale(train_data=x_Train_not_scaled,test_data=x_Test_not_scaled)






    parser = argparse.ArgumentParser()
    parser.add_argument("--trainmodels", action="store_true")
    #parser.add_argument("--loadstacking", action='store_true')
    parser.add_argument("--predict", action="store_true")
    args = parser.parse_args()

    # basemodel to use
    models=[RandomForestClassifier(),XGBClassifier(),ExtraTreesClassifier(),DecisionTreeClassifier(),AdaBoostClassifier()]

    if args.trainmodels:
        start_time=time.time()
        aucs=dict()
        for model in models:
            clf_temp=model_fit(model,init=True,param_list=INIT_GRID,X_train=x_train,y_train=y_train)
            skf_temp=StratifiedKFold(n_splits=3,random_state=Random_State,shuffle=False)
            best_params_temp,best_auc_temp=find_best_params(model=clf_temp,X_train=x_train,y_train=y_train,PARAM_GRID=PARAM_GRID,cv=skf_temp,scoring='roc_auc',verbose=1)
            print('Model Name: ',clf_temp.__class__.__name__)
            print('Find best Params: ',best_params_temp)
            print('The best AUC: ',best_auc_temp)
            model_name=model.__class__.__name__
            if model_name not in aucs:
                aucs[model_name]=best_params_temp
            clf_temp.set_params(**best_params_temp)
            #clf_temp.fit(np.array(x_train),np.array(y_train).reshape(-1))
            save_model(clf_temp)
        end_time=time.time()
        print("Training Time: %.2f Seconds"%(end_time-start_time))
            
        #best_model_name=sorted(aucs.items(),key=operator.itemgetter(1),reverse=True)[0][0]
        #best_model=load_model(best_model_name)
        #best_model.fit(np.array(x_train),np.array(y_train).reshape(-1))

    if args.predict:
        start_time=time.time()
        stacking(models=models,X_train=x_train,y_train=y_train,X_test=x_Test,test_user_id=test_data_user_id,PARAM_GRID=PARAM_GRID,train=True,prediction=True,random_state=Random_State)
        end_time=time.time()
        print("Predict Time: %.2f Seconds"%(end_time-start_time))       
















