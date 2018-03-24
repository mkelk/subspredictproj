knitr::opts_chunk$set(echo = TRUE, warning=F, message=F)

# source files
# Mortencomment: Read V3 data files instead of V2
customers_file <- '../data/CustomersV3.export.tsv'
subscriptions_file <- '../data/SubscriptionsV3.export.tsv'
gdp_file <- '../data/gdp-simple.export.tsv'

# time window settings
begin_train_window <- as.Date('2017-01-01')
begin_validation_window <- as.Date('2017-09-01')
end_window <- as.Date('2018-01-01')

last_valid_month <- '2018-03-01'
