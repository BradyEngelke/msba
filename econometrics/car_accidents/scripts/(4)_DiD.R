# Author: Brady Engelke

library(tidyverse)
library(plm)
library(readxl)
setwd("~/MSBA/spring/Econometrics/project")

# load data
crashes <- read.csv('crash_data/crashes.csv', header = TRUE)
revenues <- read_excel('data/rev_expenses.xlsx', col_names = TRUE)
alcohol <- read_excel('data/alcohol.xlsx', col_names = TRUE)
pop <-read_excel('data/pop.xlsx', col_names = TRUE)

# data engineering
crashes <- crashes %>% mutate(month_num = rep(1:48, 6))
revenues <- revenues %>% select(State, Year, Highways_Expense) %>% rename(state = State, year = Year)
alcohol <- alcohol %>% select(State, Year, All_Beverages) %>% rename(state = State, year = Year)
alcohol <- alcohol %>% filter(state %in% c('Maine', 'Minnesota', 'Texas', 'Nevada') & year >= 2015)
pop$State <- str_sub(pop$State, start = 2)
pop2 <- pop %>% gather(key = 'year', value = 'population', `2015`,`2016`, `2017`, `2018`)
pop2 <- pop2 %>% mutate(pop_density = population / Total_Area)
pop2 <- pop2 %>% select(State, year, pop_density, Total_Area) %>% rename(state = State)

crashes <- merge(crashes, revenues, by = c('state', 'year'))
crashes <- merge(crashes, pop2, by = c('state', 'year'))
# only have data on alcohol thru 2017
# crashes <- merge(crashes, alcohol, by = c('state', 'year'))

crashes <- crashes %>% 
  mutate(hw_expense_ratio = Highways_Expense / Total_Area) %>% 
  select(-Highways_Expense, -Total_Area)

crashes$year <- as.factor(crashes$year)
crashes$month <- as.factor(crashes$month)
crashes$treatment <- as.factor(crashes$treatment)
crashes$after <- as.factor(crashes$after)

crashes <- crashes %>% filter(state %in% c('Maine', 'Minnesota', 'Texas', 'Nevada'))

# exploratory plots
ggplot(crashes) + geom_line(aes(x = month_num, y = fatal_crashes, color = state)) + geom_vline(xintercept = 25, color = 'red') +
  ylab('Fatal Crashes') + xlab('Month') + theme_minimal() 

crashes %>% group_by(treatment, month_num) %>%
  summarize(f_crashes = mean(fatal_crashes)) %>%
  ggplot() + geom_line(aes(x = month_num, y = f_crashes, color = treatment)) + 
  geom_smooth(aes(x = month_num, y = f_crashes, color = treatment)) + geom_vline(xintercept = 25) +
  ylab('Fatal Crashes') + xlab('Month') + theme_minimal() 

crashes %>% group_by(treatment, month_num) %>%
  summarize(f_crashes = sum(fatal_crashes)) %>%
  rename(legality = treatment) %>%
  ggplot() + geom_line(aes(x = month_num, y = f_crashes, color = legality), size = 0.3) + 
  geom_vline(xintercept = 25, color = 'red') +
  geom_smooth(aes(x = month_num, y = f_crashes, color = legality), size = 0.4) +
  ylab('Sum Fatal Crashes') + xlab('Month') + theme_minimal() 

crashes %>% group_by(state) %>%
  summarize(highways = mean(hw_expense_ratio),
            f_crashes = mean(fatal_crashes)) %>%
  ggplot() + geom_col(aes(x = state, y = highways)) 

crashes %>% group_by(state) %>%
  summarize(highways = mean(hw_expense_ratio),
            f_crashes = mean(fatal_crashes)) %>%
  ggplot() + geom_col(aes(x = state, y = f_crashes)) 


# dropping texas to see if results improve
crashes2 <- crashes %>% filter(state %in% c('Minnesota', 'Maine'))

# adding weather data
weather <- read.csv('clean_data/me_mn_weather.csv', header = TRUE)
weather <- weather %>% select(State, year, month, avg_temp, avg_visi)
weather <- weather[3:39, ]
me <- weather[2:13,]
mn <- weather[22:33,]
weather <- rbind(me, mn)
weather <- weather %>% mutate(state = ifelse(State == 'ME', 'Maine', 'Minnesota')) %>% select(-State)
weather <- weather %>% mutate(M = ifelse(month == 1, 'January',
                                  ifelse(month == 2, 'February', 
                                  ifelse(month == 3, 'March',
                                  ifelse(month == 4, 'April',
                                  ifelse(month == 5, 'May',
                                  ifelse(month == 6, 'June',
                                  ifelse(month == 7, 'July',
                                  ifelse(month == 8, 'August', 
                                  ifelse(month == 9, 'September',
                                  ifelse(month == 10, 'October',
                                  ifelse(month == 11, 'November',
                                  ifelse(month == 12, 'December', NA))))))))))))) %>% select(-month)

weather <- weather %>% rename(month = M)
crashes2 <- merge(crashes, weather, by = c('state', 'year', 'month'))
crashes2 <- crashes2 %>% arrange(state, month_num)

# DiD with no weather
# naive regression
summary(lm(fatal_crashes ~ after + treatment + hw_expense_ratio + after * treatment, data = crashes))

# sfe
did_sfe <- plm(fatal_crashes ~ after + treatment + hw_expense_ratio + after * treatment, 
               data = crashes, effect = 'individual', index = 'state', model = 'within')

summary(did_sfe)

# DiD with weather
# naive regression
summary(lm(fatal_crashes ~ after + treatment + 
             hw_expense_ratio + avg_temp + avg_visi + after * treatment, data = crashes2))

# sfe
did_sfe2<- plm(fatal_crashes ~ after + treatment + hw_expense_ratio + after * treatment + avg_visi + avg_temp, 
               data = crashes2, effect = 'individual', index = 'state', model = 'within')

summary(did_sfe2)


