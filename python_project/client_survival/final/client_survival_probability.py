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
import s3_connection
import query_reader
from global_variables import *
from mysql_connection import MysqlConnection
from models.user_score import UserScore

# Use
# csp = ClientSurvivalProbability.new
# csp.train_model()
# csp.predict()

class ClientSurvivalProbability:
    def __init__(self):
        self.read_connection = MysqlConnection(MYSQL_READ_LOCATION).getConnection()
        self.transaction_summary = query_reader.getDataframeFromQuery("scripts/client_survival/transaction_summary.sql", self.read_connection, "user_id")
        self.beta_geo_fitter = None

    def save_model(self, model):
        try:
            model_name = model.__class__.__name__
            save_path = model_name+'.pkl'
            if os.path.exists(save_path):
                os.remove(save_path)
            output_file = open(save_path,'wb')
            dill.dump(model,output_file)
            output_file.close()
            print('{} saved successfully.'.format(model_name))

        except Exception as e:
            print('Error in model')
            print (e)

    '''
    Unused until for future use
    '''
    def load_model(self):
        try:
            model_name = 'BetaGeoFitter'
            load_path = model_name+'.pkl'
            infile = open(load_path,'rb')
            model = dill.load(infile)
            return model
        except Exception as e:
            print('Failled to load the model')
            print(e)

    def predict(self):
        client_palive = self.transaction_summary.copy()
        client_palive['score'] = client_palive.apply(
                lambda row: self.beta_geo_fitter.conditional_probability_alive(row['frequency'],row['recency'],row['T']) if row['frequency']>0 else self.beta_geo_fitter.conditional_probability_alive(row['frequency']+1,row['recency'],row['T']), axis = 1)
        user_score = UserScore()
        user_score.write("customer_retention_probability", client_palive, ['user_id', 'score'])

        print('Prediction Finished')

    def train_model(self):
        self.beta_geo_fitter = BetaGeoFitter(penalizer_coef = 0.0)
        self.beta_geo_fitter.fit(self.transaction_summary['frequency'],self.transaction_summary['recency'],self.transaction_summary['T'])
        '''
        Pan says it's not needed to save
        '''
        # self.save_model(self.beta_geo_fitter)

    def mark_old_values_for_clearing(self):
        user_score = UserScore()
        user_score.mark_deletion("customer_retention_probability")

    def clear_old_values(self):
        user_score = UserScore()
        user_score.delete_marked("customer_retention_probability")

    def run(self):
        self.train_model()
        self.mark_old_values_for_clearing()
        self.predict()
        self.clear_old_values()
