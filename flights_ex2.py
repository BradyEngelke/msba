#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 17 08:09:03 2019

@author: bradyengelke
"""

import pandas as pd
import numpy as np

def most_delayed_flights(file):
    
    df = pd.read_csv(file)
    df_1 = df[['CARRIER', 'FL_NUM', 'DAY_WEEK','Flight Status']]
    df_2 = df_1.replace({'ontime': 0, 'delayed': 1})  
    
    # calculate carrier delay rates
    carrier_drs = df_2.groupby('CARRIER').mean()
    carrier_drs = carrier_drs.rename(columns = {'FL_NUM': 'z', 
                                                'DAY_WEEK': 'z2', 
                                                'Flight Status': 'Carrier dr'})
    # calculate flight delay rates
    flight_drs = df_2.groupby('FL_NUM').mean()
    flight_drs = flight_drs.rename(columns = {'Flight Status': 'flight dr',
                                              'DAY_WEEK': 'y2'})
    
    # calculate which flights are regular flights
    reg_flights = df_2.groupby('FL_NUM').agg({'DAY_WEEK':'nunique'})
    reg_flights = reg_flights[reg_flights >= 3].dropna()
    reg_flights = reg_flights.rename(columns = {'DAY_WEEK': 'op_days'})
    
    # merge fearures into original df
    merged_df = pd.merge(df_2, reg_flights, on = 'FL_NUM')
    merged_df = merged_df.drop(['DAY_WEEK', 'op_days', 'Flight Status'], axis = 1)
    merged_df2 = pd.merge(merged_df, carrier_drs, on = 'CARRIER')
    merged_df2 = merged_df2.join(flight_drs, on = 'FL_NUM') # could potentially be the bug
    merged_df2 = merged_df2.drop(['z', 'z2', 'y2'], axis = 1)
    
    # filter out flights below Carrier delay rate
    merged_df2['dr_dif'] = merged_df2['flight dr'] - merged_df2['Carrier dr']
    merged_df2 = merged_df2[merged_df2['dr_dif'] > 0]
    merged_df2 = merged_df2.drop_duplicates()
    
    # organize dataframe for output
    df_3 = merged_df2.set_index(['CARRIER', 'FL_NUM']).sort_index()

    return df_3['flight dr']