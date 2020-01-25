# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np

master = pd.read_csv('full_1120.csv', parse_dates=['calendar_date'])
air_info = pd.read_csv('air_store_info_with_nearest_active_station.csv')

final = pd.DataFrame()

for i in range(len(air_info)):
    store_id = air_info['air_store_id'][i]
    station_id = air_info['station_id'][i]
    weather = pd.read_csv(str(station_id) + '.csv',
                          parse_dates=['calendar_date'])
    weather['air_store_id'] = store_id
    temp = master.merge(weather, on=['air_store_id', 'calendar_date'],
                        how='inner')
    final = final.append(temp)

final.to_csv('final.csv', index=False)
