# -----------------------------------------------------------------------------
# define groupings that govern churn behavior for individual expiring subscriptions
# -----------------------------------------------------------------------------


agetojoinsiteversforchurn <- 5



# -----------------------------------------------------------------------------
# make distinction between old and new sitevers
# groups sitevers into original sitevers (DK, SE, etc  = ORG) and SimpleSite = SS
createSiteVerCats <- function(subsdataset)
{
  subsdataset %>%
    dplyr::group_by(siteverkey) %>%
    dplyr::summarise(num = length(siteverkey)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(siteverkeycat = ifelse(siteverkey == "US", "SS", "ORG")) %>%
    dplyr::select(-num)
}

addSiteVerCats <- function(subsdataset)
{
  sitevercats <- createSiteVerCats(subsdataset)
  subsdataset %>%
    dplyr::inner_join(sitevercats, by = "siteverkey") %>%
    dplyr::mutate(siteverkeycat = factor(siteverkeycat))
}



# -----------------------------------------------------------------------------
# make a grouping of markets

# -----------------------------------------------------------------------------
# divide into fewer markets that share characteristics like churn and lifetime
# TODO: Get CH in individually, here and in real budget
bmchosenfull <- 
  list (
    default = "LowGeneric",
    enumerations = 
      list (
        "DK" = c("DK"), "SE" = c("SE"), "NO" = c("NO"), "NL" = c("NL"), "BE" = c("BE"),
        "FR" = c("FR"), "IT" = c("IT"), "ES" = c("ES"), "FI" = c("FI"),
        "DE" = c("DE"), "US" = c("US"),"GB" = c("GB"), "AU" = c("AU"), "CA" = c("CA"), "CH" = c("CH"),
        # Above ~1000
        "HighLTV" = c("IE", "AT", "LU", "NZ", "AR"),
        # below ~1000, but above ~500
        "MediumLTV" = c("SG", "MX", "AE", "CZ", "ZA", "AE")
      )
  )

bmchosensimple <- 
  list (
    default = "US",
    enumerations = 
      list (
        "DK" = c("DK")
      )
  )
bmchosen <- bmchosenfull

fn_budgetmarket <- Vectorize(
  function(x) { 
    tst <- bmchosen$default
    for (i in 1:length(names(bmchosen$enumerations)))
    {
      if (x %in% bmchosen$enumerations[[i]]) { tst <- names(bmchosen$enumerations)[[i]][[1]] }
    }
    tst
  }
)





createMarketcats <- function(subsdataset)
{ 
  subsdataset %>%
    dplyr::group_by(marketname) %>%
    dplyr::summarise() %>%
    dplyr::mutate(marketcat = fn_budgetmarket(marketname), 
                  marketcat = factor(marketcat))
}

# join marketcats onto a subsciption data set containing already markets
addMarketCats <- function(subsdataset)
{
  marketcats <- createMarketcats(subsdataset)
  subsdataset %>%
    dplyr::inner_join(marketcats, by = "marketname")
}






# do age brackets for starting subs, used for churn and renew
getAgeAtStartCatChurnFull <- Vectorize( 
  function(ageatstart)
  {
    cat <- 0
    if (ageatstart <= 0) cat <- 0
    else if (ageatstart <= 1) cat <- 1
    else if (ageatstart <= 2) cat <- 2
    else if (ageatstart <= 3) cat <- 3
    else if (ageatstart <= 5) cat <- 5
    else if (ageatstart <= 8) cat <- 8
    else if (ageatstart <= 11) cat <- 11
    else if (ageatstart <= 14) cat <- 14
    else if (ageatstart <= 26) cat <- 26
    else if (ageatstart <= 38) cat <- 38
    else cat <- 39  # more than 26
    
    cat
  }
)
getAgeAtStartCatChurnOldBudget <- Vectorize( 
  function(ageatstart)
  {
    cat <- 0
    if (ageatstart <= 3) cat <- 3
    else if (ageatstart <= 6) cat <- 6
    else if (ageatstart <= 9) cat <- 9
    else cat <- 10 # more than 26
    
    cat
  }
)
getAgeAtStartCatChurn <- getAgeAtStartCatChurnFull


addAgeAtStartCatsChurn <- function(subsdataset)
{
  tmp <- subsdataset %>%
    dplyr::mutate(ageatstartcatchurn = getAgeAtStartCatChurn(months_total))
  
  tmp
}

getChosenSubsLengthCatChurn <- Vectorize( 
  function(ageatstart, siteverkeycat, custchosensubslength)
  {
    cat <- 'gen'  # generic
    if ((siteverkeycat == "SS" & ageatstart == 0) |
        (siteverkeycat != "SS" & ageatstart == 1))
      cat <- custchosensubslength
    
    cat
  }
)

addChosenSubsLengthCatChurn <- function(subsdataset)
{
  tmp <- subsdataset %>%
    dplyr::mutate(custchosensubslengthcatchurn = getChosenSubsLengthCatChurn(months_total, siteverkeycat, paymentperiodchosenatstart),
                  custchosensubslengthcatchurn = factor(custchosensubslengthcatchurn))
  
  tmp
}


getSiteVerKeyCatChurn <- Vectorize(vectorize.args = c("ageatstartcatchurn", "siteverkeycat"),
                                   function(ageatstartcatchurn, siteverkeycat)
                                   {
                                     ifelse(ageatstartcatchurn <= agetojoinsiteversforchurn, as.character(siteverkeycat), 'MUT')
                                   }
)

addSiteVerKeyCatChurn <- function(subsdataset)
{
  tmp <- subsdataset %>%
    dplyr::mutate(siteverkeycatchurn = getSiteVerKeyCatChurn(ageatstartcatchurn, siteverkeycat), 
                  siteverkeycatchurn = factor(siteverkeycatchurn))
  
  tmp
}


addChurnCatsEtc <- function(subsdataset, drophelperentities = FALSE)
{
  tmp <- subsdataset %>%  
    addMarketCats() %>%
    addSiteVerCats() %>%
    addAgeAtStartCatsChurn() %>%
    addChosenSubsLengthCatChurn() %>%
    addSiteVerKeyCatChurn() %>%
    composeChurncat() %>%
    composeChurncatNoMarket()
  
  if(drophelperentities)
  {
    tmp <- tmp %>% dplyr::select(-ageatstartcatchurn, -custchosensubslengthcatchurn, -siteverkeycatchurn)
  }
  
  tmp
}

composeChurncat <- function(subsdataset)
{
  tmp <- subsdataset %>%    
    dplyr::mutate(churncat = paste(
      "mc", marketcat, 
      "scc", siteverkeycatchurn,
      "ac", ageatstartcatchurn, 
      "m", months, 
      "ccsl", custchosensubslengthcatchurn,
      sep = "-"), 
      churncat = factor(churncat))
  
  tmp  
}

composeChurncatNoMarket <- function(subsdataset)
{
  tmp <- subsdataset %>%    
    dplyr::mutate(churncatnomarket = paste(
      "scc", siteverkeycatchurn,
      "ac", ageatstartcatchurn, 
      "m", months, 
      "ccsl", custchosensubslengthcatchurn,
      sep = "-"), 
      churncatnomarket = factor(churncatnomarket))
  
  tmp  
}
