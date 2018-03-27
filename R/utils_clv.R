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
predictContinuation <- function(customer, continuation_model, churn_model, default_length = 1, max_depth = 10, max_months = 36){
  probabilities <- tryCatch(predict(continuation_model, newdata = customer), error = function(e) {tmp <- data.frame(1); colnames(tmp) <- default_length})
  
  probabilities %>%
    imap(~{
      new_months <- as.numeric(.y)
      tmp <- list(customer = mutateCustomer(customer, new_months, revenue=T, full=F))
      tmp$churn_probability <- predictChurn(tmp$customer, churn_model)
      if(tmp$customer$simulation_step < max_depth & tmp$customer$simulation_month < max_months) {
        tmp$continuation <- predictContinuation(tmp$customer, continuation_model, max_depth = max_depth, max_months = max_months)
      }
      
      return(list(probability = .x, leaf = tmp))
    })
}

# Calculate CLV for customer 
sumCLV <-  function(customer_simulation, default_val = 0){
  if(!is.null(customer_simulation$continuation)) {
    clv <- customer_simulation$continuation %>%
      map_dbl(~{
        .$probability * sumCLV(.$leaf, default_val = default_val)
      }) %>%
      reduce(`+`)
  } else {
    clv <- default_val
  }
  
  return(customer_simulation$customer$revenuecurr + (clv * (1 - customer_simulation$churn_probability)))
}


# Customer operations
initializeCustomer <- function(customer){
  customer %>%
    mutate(simulation_step = 0,
           simulation_month = 0) %>%
    as.data.frame()
  
}

revenueCurr <- function(months) months * 10

mutateCustomer <- function(
  customer, 
  new_months, 
  num_previous_months_breaks = c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38), 
  age_to_join_sitevers = 5,
  revenue = TRUE,
  full = TRUE
) {
  customer$simulation_step <- customer$simulation_step + 1
  customer$simulation_month <- customer$simulation_month + new_months
  
  customer$num_previous_months <- customer$num_previous_months + customer$months
  customer$num_previous_months_binned <- as.numeric(as.character(cut(customer$num_previous_months, 
                                                                     breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                     labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1))))
  customer$siteverkey_cat2 <- if_else(customer$num_previous_months_binned <= age_to_join_sitevers, as.character(customer$siteverkey_cat), 'MUT')
  customer$chosen_subs_length <- ifelse((customer$siteverkey_cat != "SS" & customer$num_previous_months == 1), 
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
