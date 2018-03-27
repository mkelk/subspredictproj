source('config.R')
source('utils.R')
source('utils_validation.R')

subscriptions <- read_rds('../data/subscriptions.rds')
churn_model <- read_rds('../data/models/churn_model.rds')
continuation_model <- read_rds('../data/models/continuation_model.rds')

test_customer <- subscriptions %>% 
  sample_n(1) %>%
  generateLifetimeTree(churn_model, continuation_model, max_depth = 2, max_months = 24)
  

prediction_object = list(
  probability = 1,
  customer = test_customer,
  churn_probability = predictChurn(test_customer, churn_model),
  
  continuation = predictContinuation(test_customer, continuation_model, churn_model, max_depth = 1, max_months = 24)
)

expectedValue(test_customer, type = 'lifetime')
expectedValue(test_customer, type = 'revenue')

generateLifetimeTree <- function(customer, churn_model, continuation_model, max_depth, max_months) {
  customer <- as.list(customer)
  
  list(
    probability = 1,
    customer = customer,
    churn_probability = predictChurn(customer, churn_model),
    
    continuation = predictContinuation(customer, continuation_model, churn_model, max_depth = max_depth, max_months = max_months)
  )
}
