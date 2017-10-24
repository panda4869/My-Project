from sklearn.base import BaseEstimator
import sklearn.metrics
import numpy as np
from sklearn.linear_model import LogisticRegression as LR 
from sklearn.datasets.mldata import fetch_mldata
from sklearn.linear_model.stochastic_gradient import SGDClassifier
import sklearn.svm
import random


class SelfLearningModel(BaseEstimator):
    def __init__(self,basemodel,max_iter=200,prob_threshold=0.8):
        self.model=basemodel
        self.max_iter=max_iter
        self.prob_threshold=prob_threshold

    def fit(self,X,y):
        labelx=X[y!=-1,:]
        labely=y[y!=-1]
        ulabelx=X[y==-1,]
        ulabely=y[y==-1]
        ulabel_old=[]
        self.model.fit(labelx,labely)
        i=0
        while (i<self.max_iter and np.any(ulabel_old!=labely) or len(ulabel_old)==0):
            ulabel_old=ulabely.copy()
            
            nidx=np.where((self.predict_proba(ulabelx)[0]>self.prob_threshold)| (self.predict_proba(ulabelx)[1]>self.prob_threshold))[0]
            labelx=np.vstack((labelx,ulabelx[nidx, :]))
            labely=np.hstack((labely,self.model.predict(ulabelx[nidx, :])))
            self.model.fit(labelx,labely)
            ulabely=self.predict(labelx)
            i+=1

        if not getattr(self.model,'predict_proba',None):
            self.plattlr=LR()
            preds=self.model.predict(labelx)
            self.plattlr.fit(preds.reshape(-1,1),labely)


        return self


    def predict_proba(self,X):
        if getattr(self.model,'predict_proba',None):
            return self.model.predict_proba(X)
        else:
            preds=self.model.predict(X)
            return self.plattlr.predict_proba(preds.reshape(-1,1))




    def predict(self,X):
        return self.model.predict(X)


    def score(self,X,y,sample_weight=None):

        return sklearn.metrics.accuracy_score(y,self.predict(X),sample_weight=sample_weight)


if __name__=='__main__':
    heart=fetch_mldata('heart')
    X=heart.data
    ytrue=heart.target
    ytrue[ytrue==-1]=0
    ys=np.array([-1]*len(ytrue))
    basemodel=SGDClassifier(loss='log',penalty='l1')
    label_N=2
    random_labeled_points=random.sample(list(np.where(ytrue==0)[0]),int(label_N/2))+random.sample(list(np.where(ytrue==1)[0]),int(label_N/2))
    ys[random_labeled_points]=ytrue[random_labeled_points]
    #print(X[random_labeled_points,:])
    basemodel.fit(X[random_labeled_points,:],ytrue[random_labeled_points])
    print('Supervised Accurcy Score: ',basemodel.score(X,ytrue))
    ssmodel=SelfLearningModel(basemodel)
    ssmodel.fit(X,ys)
    print("Semi Supervised Accuracy Score: ",ssmodel.score(X,ytrue))

