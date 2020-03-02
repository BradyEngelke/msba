# Author: Brady Engelke

library(tidyverse)
library(readxl)
library(stringr)
setwd("~/MSBA/spring/Econometrics/project")

legalization <- read_excel('data/legality.xlsx', col_names = TRUE)
# (0 = Fully-illegal, 1 = medical, 2 = Legal)
population <- read_excel('data/pop.xlsx',col_names = TRUE)
population$State <- str_sub(population$State, start = 2)
age <- read.csv('data/age_gender.csv', header = TRUE) 
# 0 - total, 1 - male, 2 - female
gender <- read.csv('data/gender.csv', header = TRUE)
politics <- read_excel('data/politicalparty.xlsx', col_names = TRUE)
# R = 1, D = 2, Neutral= 0
alcohol <- read_excel('data/alcohol.xlsx', col_names = TRUE)
rev_expenses <- read_excel('data/rev_expenses.xlsx', col_names = TRUE)
crime <- read.csv('data/crime.csv', header = TRUE)

age <- age %>%
  filter(SEX == 0) %>%
  filter(AGE != 999) %>%
  filter(State != 'District of Columbia')

age2 <- age %>%
  select(State, AGE, X2014:X2016) %>%
  mutate(age_total_2014 = AGE * X2014, 
         age_total_2015 = AGE * X2015,
         age_total_2016 = AGE * X2016) %>%
  group_by(State) %>%
  summarize(mean_age_2014 = sum(age_total_2014) / sum(X2014), 
            mean_age_2015 = sum(age_total_2015) / sum(X2015),
            mean_age_2016 = sum(age_total_2016) / sum(X2016))

politics2 <- politics %>%
  mutate(pp_2014 = ifelse(X2014 == 0, 'Neutral',
                   ifelse(X2014 == 1, 'Republican', 'Democrat')),
         pp_2015 = ifelse(X2015 == 0, 'Neutral',
                          ifelse(X2015 == 1, 'Republican', 'Democrat')),
         pp_2016 = ifelse(X2016 == 0, 'Neutral',
                          ifelse(X2016 == 1, 'Republican', 'Democrat'))) %>%
  select(STATE, pp_2014:pp_2016) %>%
  rename(State = STATE)

gender <- gender %>%
  filter(State != 'United States')

male <- gender %>% filter(Sex == 1)
tot_ppl <- gender %>% filter(Sex == 0)
gender2 <- as.data.frame(unique(gender$State))
gender2$prop_male_2014 <- male$X2014 / tot_ppl$X2014
gender2$prop_male_2015 <- male$X2015 / tot_ppl$X2015
gender2$prop_male_2016 <- male$X2016 / tot_ppl$X2016
gender2 <- gender2 %>%
  rename(State = `unique(gender$State)`)

crime <- crime %>%
  mutate(prop_pop_prisoners = prisoner_count / state_population) 

crime2 <- crime %>%
  select(State, Year, prisoner_count, prop_pop_prisoners)

crime_ps <- spread(crime2, key = Year, prisoner_count)
crime_prop <- spread(crime2, key = Year, value = prop_pop_prisoners)

crime_ps$prisoners_2014 <- crime_ps$`2014`
crime_ps$prisoners_2015 <- crime_ps$`2015`
crime_ps$prisoners_2016 <- crime_ps$`2016`
crime_ps$prisoners_2014[is.na(crime_ps$prisoners_2014)] <- 0
crime_ps$prisoners_2015[is.na(crime_ps$prisoners_2015)] <- 0
crime_ps$prisoners_2016[is.na(crime_ps$prisoners_2016)] <- 0

crime_prop$prop_pop_prisoners_2014 <- crime_prop$`2014`
crime_prop$prop_pop_prisoners_2015 <- crime_prop$`2015`
crime_prop$prop_pop_prisoners_2016 <- crime_prop$`2016`
crime_prop$prop_pop_prisoners_2014[is.na(crime_prop$prop_pop_prisoners_2014)] <- 0
crime_prop$prop_pop_prisoners_2015[is.na(crime_prop$prop_pop_prisoners_2015)] <- 0
crime_prop$prop_pop_prisoners_2016[is.na(crime_prop$prop_pop_prisoners_2016)] <- 0

crime_ps2 <- crime_ps %>%
  group_by(State) %>%
  summarize(ps_2014 = sum(prisoners_2014),
            ps_2015 = sum(prisoners_2015),
            ps_2016 = sum(prisoners_2016))

crime_prop2 <- crime_prop %>%
  group_by(State) %>%
  summarize(pop_prop_ps_2014 = sum(prop_pop_prisoners_2014),
            pop_prop_ps_2015 = sum(prop_pop_prisoners_2015),
            pop_prop_ps_2016 = sum(prop_pop_prisoners_2016))

master <- age2
master <- merge(master, politics2, by = 'State')
master <- merge(master, gender2, by = 'State')
master <- merge(master, crime_ps2, by = 'State')
master <- merge(master, crime_prop2, by = 'State')

data <- read.csv('data/data_aggregation_minya.csv') 
data <- data %>%
  select(-X)

master <- merge(master, data, by = 'State')

write.csv(master, 
  '/Users/bradyengelke/MSBA/spring/Econometrics/project/data/master.csv')
