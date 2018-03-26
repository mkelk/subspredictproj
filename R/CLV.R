source('config.R')
source('utils.R')
source('utils_validation.R')

subscriptions <- read_rds('../data/subscriptions.rds')
churn_model <- read_rds('../data/models/churn_model.rds')
continuation_model <- read_rds('../data/models/continuation_model.rds')

test_customer <- subscriptions %>% 
  sample_n(1) %>%
  initializeCustomer()

prediction_object = list(
  customer = test_customer,
  churn_probability = predictChurn(test_customer, churn_model),
  
  continuation = predictContinuation(test_customer, continuation_model, churn_model, max_depth = 10, max_months = 24)
)
