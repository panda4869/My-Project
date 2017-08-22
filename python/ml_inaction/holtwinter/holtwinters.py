# Holt-Winters algorithms to forecasting
# Coded in Python 2 by: Andre Queiroz
# Description: This module contains three exponential smoothing algorithms. They are Holt's linear trend method and Holt-Winters seasonal methods (additive and multiplicative).
# References:
#  Hyndman, R. J.; Athanasopoulos, G. (2013) Forecasting: principles and practice. http://otexts.com/fpp/. Accessed on 07/03/2013.
#  Byrd, R. H.; Lu, P.; Nocedal, J. A Limited Memory Algorithm for Bound Constrained Optimization, (1995), SIAM Journal on Scientific and Statistical Computing, 16, 5, pp. 1190-1208.

from sys import exit
from math import sqrt
from numpy import array
from scipy.optimize import fmin_l_bfgs_b

def RMSE(params, *args):

    Y = args[0]
    type = args[1]
    rmse = 0

    if type == 'linear':

        alpha, beta = params
        a = [Y[0]]
        b = [Y[1] - Y[0]]
        y = [a[0] + b[0]]

        for i in range(len(Y)):

            a.append(alpha * Y[i] + (1 - alpha) * (a[i] + b[i]))
            b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
            y.append(a[i + 1] + b[i + 1])

    else:

        alpha, beta, gamma = params
        m = args[2]     
        a = [sum(Y[0:m]) / float(m)]
        b = [(sum(Y[m:2 * m]) - sum(Y[0:m])) / m ** 2]

        if type == 'additive':

            s = [Y[i] - a[0] for i in range(m)]
            y = [a[0] + b[0] + s[0]]

            for i in range(len(Y)):

                a.append(alpha * (Y[i] - s[i]) + (1 - alpha) * (a[i] + b[i]))
                b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
                s.append(gamma * (Y[i] - a[i] - b[i]) + (1 - gamma) * s[i])
                y.append(a[i + 1] + b[i + 1] + s[i + 1])

        elif type == 'multiplicative':

            s = [Y[i] / a[0] for i in range(m)]
            y = [(a[0] + b[0]) * s[0]]

            for i in range(len(Y)):

                a.append(alpha * (Y[i] / s[i]) + (1 - alpha) * (a[i] + b[i]))
                b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
                s.append(gamma * (Y[i] / (a[i] + b[i])) + (1 - gamma) * s[i])
                y.append(a[i + 1] + b[i + 1] + s[i + 1])

        else:

            exit('Type must be either linear, additive or multiplicative')
        
    rmse = sqrt(sum([(m - n) ** 2 for m, n in zip(Y, y[:-1])]) / len(Y))

    return rmse

def linear(x, fc, alpha = None, beta = None):

    Y = x[:]

    if (alpha == None or beta == None):

        initial_values = array([0.3, 0.1])
        boundaries = [(0, 1), (0, 1)]
        type = 'linear'

        parameters = fmin_l_bfgs_b(RMSE, x0 = initial_values, args = (Y, type), bounds = boundaries, approx_grad = True)
        alpha, beta = parameters[0]

    a = [Y[0]]
    b = [Y[1] - Y[0]]
    y = [a[0] + b[0]]
    rmse = 0

    for i in range(len(Y) + fc):

        if i == len(Y):
            Y.append(a[-1] + b[-1])

        a.append(alpha * Y[i] + (1 - alpha) * (a[i] + b[i]))
        b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
        y.append(a[i + 1] + b[i + 1])

    rmse = sqrt(sum([(m - n) ** 2 for m, n in zip(Y[:-fc], y[:-fc - 1])]) / len(Y[:-fc]))

    return Y[-fc:], alpha, beta, rmse

def additive(x, m, fc, alpha = None, beta = None, gamma = None):

    Y = x[:]

    if (alpha == None or beta == None or gamma == None):

        initial_values = array([0.3, 0.1, 0.1])
        boundaries = [(0, 1), (0, 1), (0, 1)]
        type = 'additive'

        parameters = fmin_l_bfgs_b(RMSE, x0 = initial_values, args = (Y, type, m), bounds = boundaries, approx_grad = True)
        alpha, beta, gamma = parameters[0]

    a = [sum(Y[0:m]) / float(m)]
    b = [(sum(Y[m:2 * m]) - sum(Y[0:m])) / m ** 2]
    s = [Y[i] - a[0] for i in range(m)]
    y = [a[0] + b[0] + s[0]]
    rmse = 0

    for i in range(len(Y) + fc):

        if i == len(Y):
            Y.append(a[-1] + b[-1] + s[-m])

        a.append(alpha * (Y[i] - s[i]) + (1 - alpha) * (a[i] + b[i]))
        b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
        s.append(gamma * (Y[i] - a[i] - b[i]) + (1 - gamma) * s[i])
        y.append(a[i + 1] + b[i + 1] + s[i + 1])

    rmse = sqrt(sum([(m - n) ** 2 for m, n in zip(Y[:-fc], y[:-fc - 1])]) / len(Y[:-fc]))

    return Y[-fc:], alpha, beta, gamma, rmse

def multiplicative(x, m, fc, alpha = None, beta = None, gamma = None):

    Y = x[:]

    if (alpha == None or beta == None or gamma == None):

        initial_values = array([0.0, 1.0, 0.0])
        boundaries = [(0, 1), (0, 1), (0, 1)]
        type = 'multiplicative'

        parameters = fmin_l_bfgs_b(RMSE, x0 = initial_values, args = (Y, type, m), bounds = boundaries, approx_grad = True)
        alpha, beta, gamma = parameters[0]

    a = [sum(Y[0:m]) / float(m)]
    b = [(sum(Y[m:2 * m]) - sum(Y[0:m])) / m ** 2]
    s = [Y[i] / a[0] for i in range(m)]
    y = [(a[0] + b[0]) * s[0]]
    rmse = 0

    for i in range(len(Y) + fc):

        if i == len(Y):
            Y.append((a[-1] + b[-1]) * s[-m])

        a.append(alpha * (Y[i] / s[i]) + (1 - alpha) * (a[i] + b[i]))
        b.append(beta * (a[i + 1] - a[i]) + (1 - beta) * b[i])
        s.append(gamma * (Y[i] / (a[i] + b[i])) + (1 - gamma) * s[i])
        y.append((a[i + 1] + b[i + 1]) * s[i + 1])

    rmse = sqrt(sum([(m - n) ** 2 for m, n in zip(Y[:-fc], y[:-fc - 1])]) / len(Y[:-fc]))

    return Y[-fc:], alpha, beta, gamma, rmse


if __name__=='__main__':
    import pandas as pd
    import matplotlib.pyplot as plt
    import numpy as np
    hw_alpha = 0.26      #  Based on robust optimization in Gelper 2007,
    hw_beta  = 0.19      #  for Gaussian, fat tail, and outlier data.
    hw_gamma = 0.5
    df=pd.read_csv('data.csv',index_col=0)
    df.index=pd.to_datetime(df.index)
    x=df['leads'].tolist()
    #print(len(x))
    forecast=12
    season_len=12
    Y,alpha,beta,gamma,rmse,a,b,s=additive(x,m=12,fc=forecast,alpha=hw_alpha,beta=hw_beta,gamma=hw_gamma)
    newY=[np.nan]*(len(x)+forecast)
    j=1
    for i in range(len(x)-1,len(x)+forecast-2):
        newY[i]=Y[j]
        j+=1

    print(alpha,beta,gamma)
    #print (Y)
    #ix=pd.DatetimeIndex(start=df.index[0],periods=len(df)+forcast,freq='M')
    #x=pd.DataFrame(df['leads'])
    plt.plot(x,label='Original')
    plt.plot(newY,label='Prediction')
    plt.legend(loc='best')
    plt.show()
    #print(newY)




    