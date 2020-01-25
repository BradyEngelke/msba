library(tidyverse)
library(lubridate)
library(ggpubr)

df <- read.csv('data/perf.csv')
lgb <- read.csv('data/Brady_y_pred.csv')
rnn <- read.csv('data/RNN_output.csv')

df$lgbr_preds <- lgb$X0
df$rnn_preds <- rnn$visitors
glimpse(df)

df$date <- date(df$date)

grouped1 <- df %>%
  group_by(date) %>%
  summarize_all('sum')

grouped2 <- df %>%
  group_by(date) %>%
  summarize_all('mean')

grouped2 <- grouped2 %>%
  gather(key = 'prediction', value = 'visitors', -date)

grouped1 <- grouped1 %>%
  gather(key = 'prediction', value = 'visitors', -date)

grouped1$prediction <- as.factor(grouped1$prediction)
grouped2$prediction <- as.factor(grouped2$prediction)
grouped1$date <- date(grouped1$date)
grouped2$date <- date(grouped2$date)
glimpse(grouped1)
glimpse(grouped2)

grouped1 %>%
  filter(prediction %in% c('actuals', 'lin_reg_preds', 'knn_preds', 'lgbr_preds')) %>%
  ggplot() + geom_line(aes(x = date, y = visitors, color = prediction)) + 
  labs(title = 'visitors sum') + theme_minimal()

grouped2 %>%
  filter(prediction %in% c('actuals', 'lin_reg_preds', 'knn_preds', 'lgbr_preds')) %>%
  ggplot() + geom_line(aes(x = date, y = visitors, color = prediction)) + 
  labs(title = 'visitors mean') + theme_minimal()


deviations <- df %>%
  transmute(date = date,
            lin_reg = abs(actuals - lin_reg_preds),
            ridge_reg = abs(actuals - ridge_reg_preds),
            knn = abs(actuals - knn_preds),
            lgbr = abs(actuals - lgbr_preds),
            rnn = abs(actuals - rnn_preds))


dev_grouped1 <- deviations %>%
  group_by(date) %>%
  summarize_all('sum') %>%
  gather(key = 'prediction', value = 'abs_sum_of_residuals', -date)

dev_grouped2 <- deviations %>%
  group_by(date) %>%
  summarize_all('mean') %>%
  gather(key = 'prediction', value = 'abs_sum_of_residuals', -date)

dev_grouped1$date <- date(dev_grouped1$date)
dev_grouped2$date <- date(dev_grouped2$date)

dev_grouped1 %>%
  ggplot() + geom_line(aes(x = date, y = abs_sum_of_residuals, color = prediction)) + 
  labs(title = 'residuals sum') + theme_minimal()

dev_grouped2 %>%
  rename(algorithm = prediction) %>%
  filter(algorithm %in% c('lin_reg', 'knn', 'lgbr', 'rnn')) %>%
  ggplot() + geom_line(aes(x = date, y = abs_sum_of_residuals, color = algorithm)) +
  labs(title = 'Most Accurate Model: LGBR', x = element_blank(), y = 'Mean Absolute Error') + theme_minimal()

mae = dev_grouped2 %>%
  group_by(prediction) %>%
  summarize_all('mean')






