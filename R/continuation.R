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
         lag_months = lag(months)) %>%
  filter(customerid == lag_customerid) %>%
  select(-lag_customerid) %>%
  # restrict to a recent expiry window
  filter(endmonth >= begin_train_window & endmonth < end_window) %>%
  mutate(set_type = as.factor(if_else(endmonth >= begin_validation_window, 'validation', 'training')),
         months = as.factor(months)) %>%
  as.data.frame()

continuation_model <- rfsrc(months~lag_months + num_previous_subs + model31224 + paymentperiodchosenatstart, 
                            data=subscriptions_with_continuation %>%
                              filter(set_type == 'training') %>%
                              sample_n(10^4))

subscriptions_with_continuation_prediction <- predict(continuation_model, newdata = subscriptions_with_continuation)

rf_summary <- subscriptions_with_continuation %>%
  select(lag_months, set_type) %>%
  bind_cols(subscriptions_with_continuation_prediction$predicted %>%
              as.data.frame()) %>%
  filter(set_type == 'validation') %>%
  gather(months, prediction, -c(lag_months, set_type)) %>%
  group_by(lag_months, months) %>%
  summarise(predicted_rf = mean(prediction))

subscriptions_with_continuation %>%
  group_by(lag_months, months, set_type) %>%
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
  ggplot(aes(months, val, fill = key)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~lag_months, ncol = 1) +
  theme_bw()
  

  
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
