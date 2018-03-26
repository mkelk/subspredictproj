################################################################################
# Categories for churn prediction - in current budget
################################################################################

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
    else cat <- 27 # more than 26
    
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


addAgeAtStartCatsChurn <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%
    dplyr::mutate(ageatstartcatchurn = getAgeAtStartCatChurn(ageatstart))
  
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

addChosenSubsLengthCatChurn <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%
    dplyr::mutate(custchosensubslengthcatchurn = getChosenSubsLengthCatChurn(ageatstart, siteverkeycat, custchosensubslength))
  
  tmp
}


getSiteVerKeyCatChurn <- Vectorize(vectorize.args = c("ageatstartcatchurn", "siteverkeycat"),
                                   function(ageatstartcatchurn, siteverkeycat, myconf)
                                   {
                                     ifelse(ageatstartcatchurn <= myconf$agetojoinsiteversforchurn, siteverkeycat, "MUT")
                                   }
)

addSiteVerKeyCatChurn <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%
    dplyr::mutate(siteverkeycatchurn = getSiteVerKeyCatChurn(ageatstartcatchurn, siteverkeycat, myconf))
  
  tmp
}


addChurnCats <- function(subsdataset, myparms, myconf, drophelperentities = TRUE)
{
  tmp <- subsdataset %>%    
    addAgeAtStartCatsChurn(myparms, myconf) %>%
    addChosenSubsLengthCatChurn(myparms, myconf) %>%
    addSiteVerKeyCatChurn(myparms, myconf) %>%
    composeChurncat()
  
  if(drophelperentities)
  {
    tmp <- tmp %>% dplyr::select(-ageatstartcatchurn, -custchosensubslengthcatchurn, -siteverkeycatchurn)
  }
  
  tmp
}

addChurnCatsNoMarket <- function(subsdataset, myparms, myconf, drophelperentities = TRUE)
{
  tmp <- subsdataset %>%    
    addAgeAtStartCatsChurn(myparms, myconf) %>%
    addChosenSubsLengthCatChurn(myparms, myconf) %>%
    addSiteVerKeyCatChurn(myparms, myconf) %>%
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
      "m", periodmonths, 
      "ccsl", custchosensubslengthcatchurn,
      sep = "-"))
  
  tmp  
}

# used for simpleprophaz
composeChurncatNoMarket <- function(subsdataset)
{
  tmp <- subsdataset %>%    
    dplyr::mutate(churncatnomarket = paste(
      "scc", siteverkeycatchurn,
      "ac", ageatstartcatchurn, 
      "m", periodmonths, 
      "ccsl", custchosensubslengthcatchurn,
      sep = "-"))
  
  tmp  
}





################################################################################
# Categories for continuation prediction - in current budget
################################################################################

# do age brackets for starting subs, used for getting typical revenue for age, periodlength
getAgeAtStartCatCont <- Vectorize( 
  function(ageatstart)
  {
    cat <- 0
    if (ageatstart <= 0) cat <- 0
    else if (ageatstart <= 1) cat <- 1
    else cat <- 2 # more than 1
    
    cat
  }
)

addAgeAtStartCatsCont <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%
    dplyr::mutate(ageatstartcatcont = getAgeAtStartCatCont(ageatstart))
  
  tmp
}

getChosenSubsLengthCatCont <- Vectorize( 
  function(ageatstart, siteverkeycat, custchosensubslength)
  {
    cat <- 'gen'  # generic
    if ((siteverkeycat == "SS" & ageatstart == 0) |
        (siteverkeycat != "SS" & ageatstart == 1))
      cat <- custchosensubslength
    
    cat
  }
)

addChosenSubsLengthCatCont <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%
    dplyr::mutate(custchosensubslengthcatcont = getChosenSubsLengthCatChurn(ageatstart, siteverkeycat, custchosensubslength))
  
  tmp
}


addContinuationCats <- function(subsdataset, myparms, myconf)
{
  # addChurnCats(subsdataset, myparms, myconf) %>%
  #   dplyr::mutate(continuationcat = churncat) %>%
  #   dplyr::select(-churncat)
  tmp <- subsdataset %>%
    addAgeAtStartCatsCont(myparms, myconf) %>%
    addChosenSubsLengthCatCont(myparms, myconf) %>%
    dplyr::mutate(continuationcat = paste(marketcat, siteverkeycat,
                                          "ac", ageatstartcatcont,
                                          "m", periodmonths,
                                          "ccsl", custchosensubslengthcatcont,
                                          sep = "-")) %>%
    dplyr::select(-ageatstartcatcont)

  tmp
}


################################################################################
# Categories for getting distribution of new customers - in current budget
################################################################################
# Note: We primarily use it for predicting the chosen subs length 
################################################################################

addNewSalesSubsCats <- function(subsdataset, myparms, myconf)
{
  tmp <- subsdataset %>%    
    dplyr::mutate(newsalessubscat = paste(
      "mc", marketcat,
      sep = "-")) 
  
  tmp
}


################################################################################
# Categories for getting distribution of new customers - in current budget
################################################################################
# Note: We primarily use it for predicting the chosen subs length 
################################################################################
