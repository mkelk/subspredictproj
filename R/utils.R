library(tidyverse)
library(lubridate)


readData <- function(customers_file, subscriptions_file, gdp_file){
  customers_org <<- read.table(file = customers_file, sep = '\t', header = TRUE, stringsAsFactors = F)
  subscriptions_org <<- read.table(file = subscriptions_file, sep = '\t', header = TRUE,
                                   stringsAsFactors=F)
  
  gdp <<- read.table(file = gdp_file, sep = '\t', header = TRUE, stringsAsFactors=F) %>%
    rename(marketname = market, 
           gdppercapita = gdppercapita2016)
  
}

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

budgetMarket <- function(x, is_full = TRUE){
  budget_market_full <- 
    list (
      default = 'LowGeneric',
      individual = c('DK', 'SE', 'NO', 'NL', 'BE', 'FR', 'IT', 'ES', 'FI', 'DE', 'US', 'GB', 'AU', 'CA', 'CH'),
      aggregated = 
        list (
          # Above ~1000
          "HighLTV" = c("IE", "AT", "LU", "NZ", "AR"),
          # below ~1000, but above ~500
          "MediumLTV" = c("SG", "MX", "AE", "CZ", "ZA", "AE")
        )
    )
  
  budget_market_simple <- 
    list (
      default = 'US',
      individual = c('DK'),
      aggregated = list ()
    )
  
  if(is_full) budget_market_list <- budget_market_full else budget_market_list <- budget_market_simple
  
  tmp <- rep(budget_market_list$default, length(x))
  tmp[x %in% budget_market_list$individual] <- x[ x %in% budget_market_list$individual ]
  
  for(i in names(budget_market_list$aggregated)) tmp[ x %in% budget_market_list$aggregated[[i]] ] <- i
  
  return(tmp)
}

processCustomers <- function(customers, SAMPLE_DATA, fraction_sample) {
  customers %>%
    mutate_at(vars(firstpaiddate, firstpaidmonth), funs(as.Date)) %>%
    # Optionally slimming the data for faster development
    sample_frac(if (SAMPLE_DATA) fraction_sample else 1) %>%
    mutate(market_category = budgetMarket(marketname),
           siteverkey_cat = if_else(siteverkey == "US", "SS", "ORG"))
}


joinProcessCustomers <- function(subscriptions, customers, age_to_join_sitevers = 5) {
  subscriptions %>%
    inner_join(customers, by = 'customerid') %>%
    mutate(siteverkey_cat2 = if_else(num_previous_months_binned <= age_to_join_sitevers, siteverkey_cat, 'MUT'),
           chosen_subs_length = if_else( (siteverkey_cat == "SS" & num_previous_months == 0) | (siteverkey_cat != "SS" & num_previous_months == 1), 
                                         as.character(paymentperiodchosenatstart),
                                         'gen')
    ) %>%
    # Mortencomment: engineer three-to-one month variables
    adjustAndAddThreeToOneVars()
}


joinProcessGDP <- function(subscriptions, gdp) {
  subscriptions %>%
    left_join(gdp, by='marketname') %>%
    mutate(gdppercapita = if_else(is.na(gdppercapita), mean(gdppercapita, na.rm = T), gdppercapita)) %>%
    mutate(gdppercapita_scaled = as.vector(scale(gdppercapita, center = T, scale = T)))
}

# Mortencomment
# Create states for three-to-one aspect
# Some customers on three month plans are "by hand" set in a state where their next 
# subscription (if not churned) is set to a slightly discounted one-month subs instead of the
# normal 3 months subs that they would normally get.
# we then continue giving them the discounted one-month subses.
#
# threetoonestartdate: The date from where we start treating these customers differently
# isthreetoonesubs: 1 if this current subs is such a special one-month subs, 0 otherwise
# isthreetoonestate: 1 if the next subs from this one _ought_ to be a special one-month subs
adjustAndAddThreeToOneVars <- function(subscriptions)
{
  subscriptions %>%
    # transform periodend and threetoonestartdatedt to dates for use a little later
    dplyr::mutate(threetoonestartdate = as.Date(threetoonestartdate)) %>%
    
    # if the next subs is a threetoonesubs then the current subs codes for it, if it isn't, then it does not
    dplyr::arrange(subscriptionid) %>%
    dplyr::group_by(customerid) %>%
    dplyr::mutate(isthreetoonestate = lead(isthreetoonesubs, default = NA)) %>%
    dplyr::ungroup() %>%
    
    # if there is no next subs, we instead look at the startdate for three-to-one
    # we execute orders around 10 days before subs expiry, take that into account
    dplyr::mutate(isthreetoonestate = 
                    if_else(is.na(isthreetoonestate),
                           # no next subs, infer from threetoonestartdate If exists and subs ends minus 10 days falls after 
                           # threetoonestartdate, then it is in the three-to-one state
                           if_else(!is.na(threetoonestartdate) & periodend + days(-10) >= threetoonestartdate, 1L, 0L),
                           # a next subs, don't introduce changes, use already present value
                           isthreetoonestate
                           )
                  )
}




concatCategrories <- function(subscriptions_with_customers) {
  subscriptions_with_customers %>%
    mutate(subscription_summary = sprintf("mc-%s_ssc-%s_ac-%d_m-%d_ccsl-%s", market_category, siteverkey_cat2, num_previous_months_binned, months, chosen_subs_length),
           subscription_summary_no_market = sprintf("ssc-%s_ac-%d_m-%d_ccsl-%s", siteverkey_cat2, num_previous_months_binned, months, chosen_subs_length)) 
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
