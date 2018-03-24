source('config.R')
source('utils.R')
source('utils_validation.R')
library(randomForestSRC)

subscriptions <- read_rds('../data/subscriptions.rds')
str(subscriptions)

subscriptions_with_continuation <- subscriptions %>%
  filter(firstpaidmonth >= '2015-01-01') %>%
  arrange(customerid, startmonth) %>%
  mutate(lag_customerid = lag(customerid),
         lag_months = lag(months),
         lag2_customerid = lag(customerid, 2),
         lag2_months = lag(months, 2)) %>%
  filter(customerid == lag_customerid) %>%
  mutate(subscription_evolution = as.factor(case_when(
    lag2_customerid != customerid ~ '2nd_sub',
    lag2_months > lag_months ~ 'falls',
    lag2_months < lag_months ~ 'grows',
    T ~ 'steady'
  ))) %>%
  select(-c(lag_customerid, lag2_customerid)) %>%
  # restrict to a recent expiry window
  filter(endmonth >= begin_train_window & endmonth < end_window) %>%
  mutate(set_type = as.factor(if_else(endmonth >= begin_validation_window, 'validation', 'training')),
         months_fac = factor(months, levels = c(1, 3, 12, 24))) %>%
  as.data.frame()

subscriptions_with_continuation %>%
  mutate(first = if_else(subscription_evolution == '2nd_sub', lag_months, lag2_months),
         second = if_else(subscription_evolution == '2nd_sub', months, lag_months),
         third = if_else(subscription_evolution == '2nd_sub', -1, months)
  ) %>%
  group_by(first, second, third) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(id = row_number(),
         first_2 = first) %>%
  gather(sequence, length, c(first, second, third)) %>%
  mutate(length = factor(length, levels = c(-1, 1, 3, 12, 24))) %>%
  ggplot(aes(sequence, length, color = as.factor(first_2), group = id, size = cnt)) +
  geom_line()

continuation_model <- rfsrc(months_fac~lag_months + num_previous_subs + model31224 + paymentperiodchosenatstart + subscription_evolution + siteverkey_cat2 + gdppercapita, 
                            data=subscriptions_with_continuation %>%
                              filter(set_type == 'training') %>%
                              sample_n(10^4))

subscriptions_with_continuation_prediction <- predict(continuation_model, newdata = subscriptions_with_continuation)

rf_summary <- subscriptions_with_continuation %>%
  select(lag_months, set_type) %>%
  bind_cols(subscriptions_with_continuation_prediction$predicted %>%
              as.data.frame()) %>%
  filter(set_type == 'validation') %>%
  gather(months_fac, prediction, -c(lag_months, set_type)) %>%
  group_by(lag_months, months_fac) %>%
  summarise(predicted_rf = mean(prediction)) %>%
  mutate(months_fac = factor(months_fac, levels = c(1, 3, 12, 24)))

subscriptions_with_continuation %>%
  group_by(lag_months, months_fac, set_type) %>%
  summarise(cnt = n()) %>%
  group_by(lag_months, set_type) %>%
  mutate(prob = cnt/sum(cnt)) %>%
  select(-cnt) %>%
  spread(set_type, prob) %>%
  mutate(err = (training - validation)/validation) %>%
  rename(predicted = training,
         real = validation) %>%
  inner_join(rf_summary) %>%
  gather(key, val, c(real, predicted, predicted_rf)) %>%
  ggplot(aes(months_fac, val, fill = key)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('Probability of next subsrciption length') +
  xlab('Next subscription') + ylab('Probability') +
  facet_wrap(~lag_months, ncol = 1) +
  theme_bw()
  
subscriptions %>%
  group_by(isfreemium, model31224) %>%
  summarise(cnt = n()) %>%
  spread(isfreemium, cnt) %>%
  View
  
subscriptions %>%
  filter(firstpaidmonth >= '2017-01-01' & num_previous_subs == 1) %>%
  mutate(set_type = as.factor(if_else(endmonth >= begin_validation_window, 'validation', 'training'))) %>%
  group_by(num_previous_months, months, set_type) %>%
  summarise(cnt = n()) %>%
  group_by(num_previous_months, set_type) %>%
  mutate(prob = cnt/sum(cnt)) %>%
  select(-cnt) %>%
  spread(set_type, prob) %>%
  mutate(err = (training - validation)/validation) %>%
  rename(predicted = training,
         real = validation) %>%
  gather(key, val, c(real, predicted)) %>%
  ggplot(aes(months, val, fill = key)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~num_previous_months, ncol = 1) +
  theme_bw()

iris_model <- randomForestSRC::rfsrc(Species~., data=iris)
aaa <- predict(iris_model, newdata = iris[1:4])
aaa$predicted
