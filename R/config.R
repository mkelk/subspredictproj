knitr::opts_chunk$set(echo = TRUE, warning=F, message=F)
knitr::opts_knit$set(global.device = TRUE)

# source files
customers_file <- '../data/CustomersV2.export.tsv'
subscriptions_file <- '../data/SubscriptionsV2.export.tsv'
gdp_file <- '../data/gdp-simple.export.tsv'

# time window settings
begin_train_window <- as.Date('2017-01-01')
begin_validation_window <- as.Date('2017-09-01')
end_window <- as.Date('2018-01-01')

last_valid_month <- '2018-03-01'
