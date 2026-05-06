#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 16 12:11:04 2019

@author: bradyengelke
"""

import pandas as pd
import numpy as np

def most_delayed_flights(file, thresh):
    
    df = pd.read_csv(file)
    df_1 = df[['CARRIER', 'FL_NUM', 'DAY_WEEK', 'Flight Status']]
    df_2 = df_1.replace({'ontime': 0, 'delayed': 1})    
    df_3 = df_2.groupby(['CARRIER', 'FL_NUM']).agg({'DAY_WEEK': 'nunique', 'Flight Status': 'mean'})
    reg_flights_s = df_3[df_3['DAY_WEEK'] >= 3]['Flight Status']
    thresh_flights = reg_flights_s[reg_flights_s > thresh]

    return thresh_flights.sort_values(ascending = False).squeeze()

