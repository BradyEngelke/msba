# Author: Brady Engelke

library(tidyverse)
library("Hmisc")
library(MatchIt)
library(readxl)
library(ggpubr)
setwd("~/MSBA/spring/Econometrics/project")
# load data
master <- read.csv('clean_data/normalized_m.csv', header = TRUE)
# perform necessary transformations
master <- master %>% 
  select(-X, -X.1, -legalization, -PScore) %>%
  filter(State != 'Colorado' & State != 'Washington' &  State != 'Alaska' & State != 'Oregon') %>%
  mutate(legality = ifelse(State %in% c('Nevada', 'California', 'Massachusetts', 'Maine'), 1, 0)) 

master_pre <- master %>% select(-State)
master$legality <- as.factor(master$legality)
master_pre$legality <- as.factor(master_pre$legality) 
# correlation matrix
controls <- master_pre %>% select(-legality, -pp)
rcorr(as.matrix(controls))
# psm
ps_model <- glm(legality ~ age + pp + prop_male + ps + rev_ratio + beverage + population_density,
                           data = master_pre, family = 'binomial')
summary(ps_model)

ps_model <- glm(legality ~ age + pp + prop_male + ps + rev_ratio + beverage + population_density,
                data = master_pre, family = binomial(link = 'probit'))
summary(ps_model)
# check results
master$propensity_score <- ps_model$fitted.values
ggplot(master) + geom_density(aes(x = propensity_score, color = legality))
master <- master %>% select(State, legality, PS, age:population_density)

# run MatchIt algo
match_output <- matchit(legality ~ age + pp + prop_male + ps + rev_ratio + beverage + population_density, 
                        data = master_pre, method = "nearest", distance = "logit", 
                        caliper = 0.01, replace = FALSE, ratio = 1)

summary(match_output)

data_matched <- match.data(match_output)
data_matched$state <- c('Maine', 'Minnesota', 'Nevada', 'Texas')
pscores <- master %>% 
  filter(State %in% c('Maine', 'Minnesota', 'Nevada', 'Texas')) %>% 
  rename(state = State) %>%
  select(state, propensity_score)
data_matched <- merge(data_matched, pscores, by ='state')
data_matched <- data_matched %>% select(state, legality, propensity_score, age:population_density)

# visualizations
ggplot(data_matched) + geom_col(aes(x = state, y = propensity_score, fill = legality)) +
  labs(color = 'Search Engine') +
  ylab('Propensity Score') + xlab('State') + theme_minimal()

master$states <- c(1:46)
master <- master %>% filter(State != 'California')
master$state_symb <- c('', '', '', '', '', '', '', '', '' , '', '',
                       '', '', '', '', 'ME', '', '', '', 'MN', '' , '', '',
                       '', 'NV', '', '', '', '', '', '', '', '' , '', '',
                       '', '', '', 'TX', '', '', '', '', '', '' )

ggplot(master, aes(x = states, y = propensity_score, color = legality)) + geom_point() +
  geom_text(aes(label= state_symb), hjust = -0.15, vjust = -0.15) +
  labs(shape = 'Legality') +
  ylab('Propensity Score') + xlab('State') + theme_minimal() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

            