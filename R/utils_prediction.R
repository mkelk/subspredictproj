



test_customer <- subscriptions_with_target %>% sample_n(1) %>% as.data.frame()

prediction_object = list(
  initial_customer = test_customer,
  churn_probability = predictChurn(test_customer, churn_model),
  
  continuation = predictContinuation(test_customer, continuation_model)
)

##---PREDICTION
predictContinuation <- function(customer, model, default_length = 1){
  probabilities <- tryCatch(predict(model, newdata = customer), error = function(e) {tmp <- data.frame(1); colnames(tmp) <- default_length})
  
  probabilities %>%
    imap(~{
      new_months <- as.numeric(.y)
      
      
      
    })
}

mutateCustomer <- function(customer, new_months, num_previous_months_breaks = c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38), full = TRUE){
  customer$periodend <- customer$periodend %m+% months(new_months)
  customer$startmonth <- customer$startmonth %m+% months(new_months)
  customer$endmonth <- customer$endmonth %m+% months(new_months)
  customer$num_previous_months <- customer$num_previous_months + customer$months
  customer$num_previous_subs <- customer$num_previous_subs + 1
  customer$num_previous_months_binned <- as.numeric(as.character(cut(customer$num_previous_months, 
                                                                     breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                     labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1))))
  customer$siteverkey_cat2
  customer$months <- new_months
  
  if(full){
    plus_vat <- customer$revenuecurrinclvat / customer$revenuecurr
    customer$revenuecurr <- revenueCurr(new_months)
    customer$revenuecurrinclvat <- customer$revenuecurr * plus_vat
    
  }
}

churn_model <- read_rds('../data/models/churn_model.rds')
class(churn_model) <- c('log_month_churn', class(churn_model))
predict.log_month_churn <- function(model, newdata = NULL){
  if(is.null(newdata)) newdata <- model$data
  
  return(1 - (1 - exp( pmin( predict.glm(model, newdata = newdata), 0) )) ^ newdata$months)
}



predictChurn <- function(customer, model, default_prob = 1){
  tryCatch(predict(model, newdata = customer), error = function(e) default_prob)
}


continuation_model <- read_rds('../data/models/continuation_model.rds')
predict.continuation <- function(model, newdata = NULL){
  if(is.null(newdata)) newdata <- model$data
  
  Vectorize(function(x) model$model_lookup[as.character(x)])(newdata$months) %>% map_dfr(~.)
  
}

##-------------------














churn_model <- read_rds('../data/models/churn_model.rds')
churn_model$formula

subscription_summary_no_market = sprintf("ssc-%s_ac-%d_m-%d_ccsl-%s", siteverkey_cat2, num_previous_months_binned, months, chosen_subs_length)
mutate(siteverkey_cat2 = if_else(num_previous_months_binned <= age_to_join_sitevers, siteverkey_cat, 'MUT'),
       chosen_subs_length = if_else( (siteverkey_cat == "SS" & num_previous_months == 0) | (siteverkey_cat != "SS" & num_previous_months == 1), 
                                     as.character(paymentperiodchosenatstart),
                                     'gen'))


processSubscriptions <- function(subscriptions, 
                                 valid_subscription_lengths = c(1, 3, 12, 24), 
                                 num_previous_months_breaks = c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38)) {
  subscriptions %>% 
    # Mortencomment: Need periodend for threetoone processing
    #select(-c(periodstart, periodend)) %>%
    select(-c(periodstart)) %>%
    
    # Removing customers not in the customers set
    filter(customerid %in% customers$customerid) %>%
    
    mutate_at(vars(startmonth, endmonth, periodend), as.Date) %>%
    mutate(months = (year(endmonth) -  year(startmonth)) * 12 + month(endmonth) -  month(startmonth)) %>%
    
    group_by(customerid) %>%
    # Removing customers with subscription period other than 1,3,12,24
    filter(min(months %in% valid_subscription_lengths) == 1) %>%
    # Removing customers with startmonth after '2018-03-01'
    filter(max(startmonth) <= last_valid_month) %>%
    
    arrange(startmonth) %>%
    mutate(status = if_else(endmonth == max(endmonth) & endmonth <  last_valid_month, 'churn', 'active'),
           num_previous_months = cumsum(months) - months,
           num_previous_subs = row_number() - 1,
           num_previous_months_binned = as.numeric(as.character(cut(num_previous_months, 
                                                                    breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                    labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1))))) %>%
    ungroup()
  
}