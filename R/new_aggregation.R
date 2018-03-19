
# -----------------------------------------------------------------------------
# make a grouping of markets
# -----------------------------------------------------------------------------
# divide into fewer markets that share characteristics like churn and lifetime
# TODO: Get CH in individually, here and in real budget
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

