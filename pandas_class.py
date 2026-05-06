#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 10:56:40 2019

@author: bradyengelke
"""
import numpy as np
from pandas import Series, DataFrame
import pandas as pd
import xlrd # excel file I/O package

df = DataFrame({'int_col' : [1,2,6,8,-1], 
                'float_col' : [0.1, 0.2,0.2,10.1,None], 
                'str_col' : ['a','b',None,'c','a']})

#indexing based on column name
df[['float_col','int_col']]

# boolean indexing
df[df['float_col'] > 0.15]

#advanced boolean indexing 
df[(df['float_col'] > 0.1) & (df['int_col']>2)]
df[(df['float_col'] > 0.1) | (df['int_col']>2)]
df[~(df['float_col'] > 0.1)]

#renaming cols
df2 = df.rename(columns={'int_col' : 'some_other_name'})

#drop or fill rows with missing values
df2.dropna()

df3 = df.copy()
mean = df3['float_col'].mean()
df3['float_col'].fillna(mean)

# apply method -> applies a fcn along axis of a df; .applymap() applies a fcn to all elements in a df
df[['int_col','float_col']].apply(np.sqrt)

#vectorized computation
df = pd.DataFrame(data={"A":[1,2], "B":[1.2,1.3]})
df["C"] = df["A"]+df["B"]
df["D"] = df["A"]*3
df["E"] = np.sqrt(df["A"])

df = pd.DataFrame(data={"A":[1,2], "B":[1.2,1.3], "Z":["a","b"]})
df["F"] = df.Z.str.upper()

# groupby() method
grouped = df['float_col'].groupby(df['str_col'])
grouped.mean()

# mutating in pandas
def two_three_strings(x):
    return x*2, x*3

df4 = df.copy()
df4['twice'],df4['thrice'] = zip(*df4['int_col'].map(two_three_strings))

# stats methods
df.describe()
df.cov()
df.corr()

### classwork with Ken ###

train = pd.read_csv('train.csv')

#isolate a column
train.IsHoliday #won't work if there is a space in the column name
train['IsHoliday']

#count # of records containing a holiday
train['IsHoliday'].sum()

# Count # of records not a holiday
(train['IsHoliday'] == False).sum()
(~train['IsHoliday']).sum()

# of records for each store
train['Store'].value_counts()

# sorted by store #
train['Store'].value_counts().sort_index()

# sorted by record count
train['Store'].value_counts().sort_values(ascending = False)

# Add a column for avg daily sales
train['Daily_Sales'] = train['Weekly_Sales'] / 7

# Rank the records by sales
train['Sales_Rank'] = train['Weekly_Sales'].rank(ascending = False, method = 'min')
train.sort_values('Sales_Rank')

# Select the record with Weekly_Sales of 9755.56
train['Weekly_Sales'] == 9755.56
(train['Weekly_Sales'] == 9755.56).sum()
train[train['Weekly_Sales'] == 9755.56]

# Total sales dept 14 of store 20
train[(train['Dept'] == 14) & (train['Store'] == 20)]
train[(train['Dept'] == 14) & (train['Store'] == 20)]['Weekly_Sales']
train[(train['Dept'] == 14) & (train['Store'] == 20)]['Weekly_Sales'].sum()

# Max weekly sales
train[train['Weekly_Sales'] == train['Weekly_Sales'].max()]

# Min Weekly sales on holiday weeks
train[train['IsHoliday']]['Weekly_Sales'].min()
train[(train['Weekly_Sales'] == train[train['IsHoliday']]['Weekly_Sales'].min()) & train['IsHoliday']]


#ex 12 insurance
distance_matrix = pd.read_csv(dist_matrix, header = None)
cities = pd.Series(['Chicago', 'Los Angeles', 'New York', 'Philadelphia'])
distance_df = pd.DataFrame(distance_matrix.values, index = cities.values, 
                                                       columns = cities.values)

# sales figures by postal code
df = pd.read_csv('sales_june.csv')
ny_df = df[(df['postal_code'] >= 10001) & (df['postal_code'] <= 14975)]
ohio_df = df[(df['postal_code'] >= 43001) & (df['postal_code'] <= 45999)]
mn_df = df[(df['postal_code'] >= 55001) & (df['postal_code'] <= 56763)]
ny_df['sales'] = pd.to_numeric(ny_df['sales'], errors='coerce')
ohio_df['sales'] = pd.to_numeric(ohio_df['sales'], errors='coerce')
mn_df['sales'] = pd.to_numeric(mn_df['sales'], errors='coerce')
ny_total = ny_df['sales'].sum()
ohio_total = ohio_df['sales'].sum()
mn_total = mn_df['sales'].sum()
print(ny_total, ohio_total, mn_total)

# rewritten ex 7 from lab 5
def pct_change(file):
    df = pd.read_csv(file, header = None, names = ['Degree', 'old', 'new'])
    df['pct_change'] = ((df['new']- df['old']) / df['old']) * 100
    del df['new']
    del df['old']
    
    return df.sort_values(by = 'pct_change', ascending = False)







