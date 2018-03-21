
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
  
  calibration_plots <- prediction_table %>%
    filter(siteverkey_cat2 != "ORG" & !is.na(predict_churn)) %>%
    mutate(subs_length = case_when(
      num_previous_months_binned == 0 & months == 1 ~ as.numeric(as.character(chosen_subs_length)),
      num_previous_months_binned != 0 & chosen_subs_length == 'gen' ~ months,
      T ~ -1
    )) %>%
    filter(subs_length > 0) %>%
    mutate(predict_churn_binned = cut(predict_churn, breaks = seq(0, 1, 0.1))) %>%
    split(.$market_category) %>%
    imap(~{
      data <- .x
      
      p1 <- data %>%
        group_by(predict_churn_binned, subs_length, set_type) %>%
        summarise(predict_churn = mean(predict_churn),
                  real_probability = mean(churnind),
                  cnt = n()) %>% 
        filter(cnt >=  20) %>%
        ggplot(aes(predict_churn, real_probability, color = as.factor(subs_length))) +
        geom_point() +
        geom_line() +
        geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = .5) +
        xlim(0,1) + ylim(0,1) +
        ggtitle(.y) +
        facet_wrap(~set_type) +
        theme_bw() +
        theme(legend.position = 'none')
      
      p1
    }) 
  
  real_churn_plot <- 
    prediction_table %>%
    filter(siteverkey_cat2 != "ORG" & !is.na(predict_churn)) %>%
    mutate(subs_length = case_when(
      num_previous_months_binned == 0 & months == 1 ~ as.numeric(as.character(chosen_subs_length)),
      num_previous_months_binned != 0 & chosen_subs_length == 'gen' ~ months,
      T ~ -1
    )) %>%
    filter(subs_length > 0) %>%
    group_by(market_category, subs_length, set_type, num_previous_months_binned, months) %>%
    summarise(churn_rate = mean(churnind),
              prediction = mean(predict_month_churn),
              cnt = n()) %>%
    group_by(market_category, subs_length, set_type) %>%
    mutate(real = 1 - (1- churn_rate) ^ (1/as.double(months)),
           shr = cnt/sum(cnt)) %>%
    gather(key, val, c(real, prediction)) %>%
    ungroup() %>%
    filter(subs_length <= 12) %>%
    split(.$market_category) %>%
    imap(~{
      p <- ggplot(.x, aes(num_previous_months_binned, val, color = key)) +
        geom_point() +
        geom_line() +
        geom_bar(aes(y = shr/2, color = NULL), alpha = 0.3, stat = 'identity', width = 2) +
        ggtitle(.y) +
        facet_grid(subs_length~set_type, scales = 'free') +
        theme_bw() +
        theme(legend.position = 'bottom')
      
      p
    })
  
  return(list(prediction_table = prediction_table, summary_table = summary_table, roc_plot = roc_plot, calibration_plots = calibration_plots, real_churn_plot = real_churn_plot))
}

