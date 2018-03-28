# Predicts specific to models
predict.log_month_churn <- function(model, newdata = NULL){
  if(is.null(newdata)) newdata <- model$data
  
  return(1 - (1 - exp( pmin( predict.glm(model, newdata = newdata), 0) )) ^ newdata$months)
}

predict.continuation <- function(model, newdata = NULL){
  if(is.null(newdata)) newdata <- model$data
  
  Vectorize(function(x) model$model_lookup[as.character(x)])(newdata$months) %>% map_dfr(~.)
  
}

# Churn prediction
predictChurn <- function(customer, model, default_prob = 1){
  tryCatch(predict(model, newdata = customer), error = function(e) default_prob)
}

# Continuation prediction
predictContinuation <- function(customer, continuation_model, churn_model, min_branch_probability, max_depth, max_months, default_length = 1, default_prob = 1, revenue = T){
  probabilities <- tryCatch(predict(continuation_model, newdata = customer), error = function(e) {tmp <- data.frame(1); colnames(tmp) <- default_length; return(tmp)})
  
  probabilities %>%
    imap(~{
      new_months <- as.numeric(.y)
      
      clv_tree <- list(
        probability = .x,
        customer = mutateCustomer(customer, new_months, revenue=revenue, full=F)
      )
      clv_tree$churn_probability <- predictChurn(clv_tree$customer, churn_model, default_prob = default_prob)[[1]]
      
      if(1 < max_depth & 
         0 < max_months - clv_tree$customer$months & 
         min_branch_probability/(clv_tree$probability * (1-clv_tree$churn_probability)) <= 1 ) {
        clv_tree$continuation <- predictContinuation(
          clv_tree$customer, 
          continuation_model = continuation_model, 
          churn_model = churn_model,
          min_branch_probability = min_branch_probability/(clv_tree$probability * (1-clv_tree$churn_probability)),
          max_depth = max_depth - 1, 
          max_months = max_months - clv_tree$customer$months,
          default_length = default_length,
          default_prob = default_prob,
          revenue=revenue)
      }
      
      return(clv_tree)
    })
}


generateLifetimeTree <- function(customer, churn_model, continuation_model, min_branch_probability = 0.001, max_depth = Inf, max_months = Inf, default_length = 1, default_prob = 1, revenue = TRUE) {
  customer <- as.list(customer)
  
  clv_tree <- list(
    probability = 1,
    customer = customer,
    churn_probability = predictChurn(customer, churn_model,  default_prob = default_prob)[[1]]
  )
  if(1 < max_depth & 
     0 < max_months - clv_tree$customer$months & 
     min_branch_probability/(clv_tree$probability * (1-clv_tree$churn_probability)) <= 1 ) {
    
    clv_tree$continuation = predictContinuation(
      clv_tree$customer, 
      continuation_model = continuation_model, 
      churn_model = churn_model,
      min_branch_probability = min_branch_probability/(clv_tree$probability * (1-clv_tree$churn_probability)),
      max_depth = max_depth - 1, 
      max_months = max_months - clv_tree$customer$months,
      default_length = default_length,
      default_prob = default_prob,
      revenue=revenue)
  }
  
  return(clv_tree)
}

# Calculate CLV for customer 
expectedValue <-  function(customer_simulation, type = 'revenue', default_val = 0){
  if(!type %in% c('revenue', 'lifetime', 'subs')) stop('Wrong type argument! Possible values are revenue lifetime')
  if(type == 'revenue') value <- customer_simulation$customer$revenuecurr
  if(type == 'lifetime') value <- customer_simulation$customer$months
  if(type == 'subs') value <- 1
  
  if(!is.null(customer_simulation$continuation)) {
    tail_value <- customer_simulation$continuation %>%
      map_dbl(~{
        .$probability * expectedValue(., default_val = default_val, type = type)
      }) %>%
      reduce(`+`)
  } else {
    tail_value <- default_val
  }
  
  return(value + (tail_value * (1 - customer_simulation$churn_probability)))
}


# Customer operations
revenueCurr <- function(months) months * 10

initializeCustomer <- function(
  marketname = 'US', 
  paymentperiodchosenatstart = 3, 
  months = 1, 
  num_previous_months = 0, 
  siteverkey = 'US',
  num_previous_months_breaks = c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38), 
  age_to_join_sitevers = 5
) {
  
  customer <- list(
    marketname = marketname,  
    paymentperiodchosenatstart = paymentperiodchosenatstart, 
    months = months,
    num_previous_months = num_previous_months, 
    siteverkey = siteverkey
  )
  
  customer$market_category = budgetMarket(customer$marketname)
  customer$num_previous_months_binned <- as.numeric(as.character(cut(customer$num_previous_months, 
                                                                     breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                     labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1))))
  customer$siteverkey_cat = ifelse(customer$siteverkey == "US", "SS", "ORG")
  customer$siteverkey_cat2 <- if_else(customer$num_previous_months_binned <= age_to_join_sitevers, as.character(customer$siteverkey_cat), 'MUT')
  customer$chosen_subs_length <- ifelse((customer$siteverkey_cat == "SS" & customer$num_previous_months == 0) |
                                          (customer$siteverkey_cat != "SS" & customer$num_previous_months == 1), 
                                        as.character(customer$paymentperiodchosenatstart),
                                        'gen')
  
  customer$subscription_summary_no_market <- sprintf("ssc-%s_ac-%d_m-%d_ccsl-%s", 
                                                     customer$siteverkey_cat2, 
                                                     customer$num_previous_months_binned, 
                                                     customer$months, 
                                                     customer$chosen_subs_length)
  
  return(customer)
}


mutateCustomer <- function(
  customer, 
  new_months, 
  num_previous_months_breaks = c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38), 
  age_to_join_sitevers = 5,
  revenue = TRUE,
  full = TRUE
) {
  customer$num_previous_months <- customer$num_previous_months + customer$months
  customer$num_previous_months_binned <- as.numeric(as.character(cut(customer$num_previous_months, 
                                                                     breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                     labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1))))
  customer$siteverkey_cat2 <- if_else(customer$num_previous_months_binned <= age_to_join_sitevers, as.character(customer$siteverkey_cat), 'MUT')
  customer$chosen_subs_length <- ifelse((customer$siteverkey_cat == "SS" & customer$num_previous_months == 0) |
                                          (customer$siteverkey_cat != "SS" & customer$num_previous_months == 1), 
                                        as.character(customer$paymentperiodchosenatstart),
                                        'gen')
  customer$months <- new_months
  
  customer$subscription_summary_no_market <- sprintf("ssc-%s_ac-%d_m-%d_ccsl-%s", 
                                                     customer$siteverkey_cat2, 
                                                     customer$num_previous_months_binned, 
                                                     customer$months, 
                                                     customer$chosen_subs_length)
  
  if(revenue){
    plus_vat <- customer$revenuecurrinclvat / customer$revenuecurr
    customer$revenuecurr <- revenueCurr(new_months)
    customer$revenuecurrinclvat <- customer$revenuecurr * plus_vat
  }
  
  if(full){
    customer$periodend <- customer$periodend %m+% months(new_months)
    customer$startmonth <- customer$startmonth %m+% months(new_months)
    customer$endmonth <- customer$endmonth %m+% months(new_months)
    customer$num_previous_subs <- customer$num_previous_subs + 1
    
    customer$subscription_summary <- sprintf("mc-%s_ssc-%s_ac-%d_m-%d_ccsl-%s", 
                                    customer$market_category,
                                    customer$siteverkey_cat2, 
                                    customer$num_previous_months_binned, 
                                    customer$months, 
                                    customer$chosen_subs_length) 
      
  }
  
  return(customer)
}
