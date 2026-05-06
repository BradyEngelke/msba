#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 13 13:15:47 2019

@author: bradyengelke
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

av_df = pd.read_csv('aviationData.txt', header = None, sep = '|', names = np.arange(0, 32, 1))

amateurs_df = av_df[av_df[16] == ' Yes ']

def yr(x):
    return x[7:11]

amateurs_df['year'] = amateurs_df[3].map(yr)

acc_s = amateurs_df.groupby('year').count()[0]


acc_s.plot(title = 'Amateur-built Accidents by Year')




df.applymap(lambda x: x.strip())













