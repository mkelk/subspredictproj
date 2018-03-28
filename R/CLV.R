source('config.R')
source('utils.R')
source('utils_clv.R')

subscriptions <- read_rds('../data/subscriptions.rds')
churn_model <- read_rds('../data/models/churn_model.rds')
continuation_model <- read_rds('../data/models/continuation_model.rds')

test_customer <- subscriptions %>% 
  sample_n(1)
  
clv_tree <- generateLifetimeTree(test_customer, churn_model, continuation_model, max_depth = 10, max_months = 24)
  
expectedValue(clv_tree, type = 'lifetime')
expectedValue(clv_tree, type = 'revenue')


test_customer_2 <- initializeCustomer(marketname = 'US', paymentperiodchosenatstart = 3, months = 1, num_previous_months = 0, siteverkey = 'US')

clv_tree <- generateLifetimeTree(test_customer_2, churn_model, continuation_model = NULL, min_branch_probability = 0.001, default_length = 3, revenue = F)
expectedValue(clv_tree, type = 'lifetime')/12
