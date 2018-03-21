
predict_2fct_model <- function(data, model){
  data %>%
    split(paste(.$market_category, .$subscription_summary_no_market)) %>%
    map_dfr(~{
      .$predict_log_month_churn <- tryCatch(predict(new_model, newdata = .), error = function(e) NA)
      .$predict_log_month_churn[.$predict_log_month_churn > 0] <- -0.001
      .
    }) %>%
    mutate(predict_month_churn = exp(predict_log_month_churn),
           predict_churn = 1 - (1 - predict_month_churn)^months) %>%
    select(-predict_log_month_churn)
}

predict_general <- function(data, model){
  data %>%
    mutate(., predict_churn = predict(model, newdata = ., type = 'response')) %>%
    mutate(predict_month_churn = 1 - (1 - predict_churn) ^ (1/as.double(months)))
}

validation <- function(data, model, predict_fun = NULL){
  if(is.null(predict_fun)) predict_fun <- predict_general
  
  prediction_table <-data %>%
    predict_fun(model)
  
  summary_table <- prediction_table %>%
    split(.$set_type) %>%
    imap_dfr(~{
      tmp <- .x[!is.na(.x$predict_churn),]
      
      data.frame(
        name = .y,
        missing_predictions = sum(is.na(.x$predict_churn)),
        auc = as.numeric(pROC::auc(tmp$churnind, tmp$predict_churn)),
        logloss = Metrics::logLoss(tmp$churnind, tmp$predict_churn),
        stringsAsFactors = F
      )
    })
  
  roc_plot <- ggplot(prediction_table, aes(d = churnind, m = predict_churn, color = set_type)) + 
    plotROC::geom_roc(n.cuts = 0) +
    plotROC::style_roc() +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  return(list(prediction_table = prediction_table, summary_table = summary_table, roc_plot = roc_plot))
}


