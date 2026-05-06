#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 22 11:57:26 2019

@author: bradyengelke
"""

import pandas as pd
import numpy as np
        

def f(row):
    count = 0
    for i in range(len(row.misspelled_word)):
        try:
            if row.misspelled_word[i] != row.corrected_word[i]:
                count += 1
        except:
            count = count
            
    return count

def avg_char_diff(file, n):
    
    df = pd.read_csv(file)
    df = df.rename(columns = {'misspelled word': 'misspelled_word',
                              'corrected word': 'corrected_word'})
    df['diff'] = df.apply(f, axis = 1)
    df1 = df.groupby('corrected_word').agg({'diff':'mean'})
    df2 = df1[df1['diff'] >= n]
    df2 = df2.sort_index()

    
    return df2.squeeze()