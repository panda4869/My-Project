from lifetimes.utils import summary_data_from_transaction_data
import pandas as pd
import numpy as np
from lifetimes import BetaGeoFitter
from lifetimes.plotting import plot_history_alive
import matplotlib.pyplot as plt
from lifetimes.plotting import plot_probability_alive_matrix
from datetime import datetime
import os
import pickle
import dill
import argparse


def save_model(model):
    try:
        model_name=model.__class__.__name__
        save_path=model_name+'.pkl'
        #print(save_model)
        if os.path.exists(save_path):
            os.remove(save_path)
        output_file=open(save_path,'wb')
        dill.dump(model,output_file)
        output_file.close()
        print('{} saved successfully.'.format(model_name))

    except Exception as e:

        print('Error in model')
        print (e)


        
def load_model():
    try:
        model_name='BetaGeoFitter'
        load_path=model_name+'.pkl'
        infile=open(load_path,'rb')
        model=dill.load(infile)
        return model
    except Exception as e:
        print('Failled to load the model')
        print(e)
    
    

if __name__=='__main__':
    
    ## add parser
    parser=argparse.ArgumentParser()
    parser.add_argument('--loadmodel',action='store_true')
    parser.add_argument('--trainmodel',action='store_true')
    parser.add_argument('--predict',action='store_true')
    args=parser.parse_args()
    
    ##init value
    trans_summary=pd.read_csv('transaction_summary.csv',index_col='user_id',usecols=['user_id','frequency','recency','T'])



    if args.trainmodel:
        #train model
        bgf=BetaGeoFitter(penalizer_coef=0.0)
        bgf.fit(trans_summary['frequency'],trans_summary['recency'],trans_summary['T'])
        save_model(bgf)
    if args.loadmodel:
        bgf=load_model()
    if args.predict:
        client_palive=trans_summary.copy()
        client_palive['palive']=client_palive.apply(lambda row: bgf.conditional_probability_alive(row['frequency'],row['recency'],row['T']) if row['frequency']>0 else bgf.conditional_probability_alive(row['frequency']+1,row['recency'],row['T'])  , axis=1)
        file_path='./client_palive.csv'
        if os.path.exists(file_path):
            os.remove(file_path)
        
        client_palive.to_csv(file_path)
        print('Prediction Finished')
            
    
    
    
    