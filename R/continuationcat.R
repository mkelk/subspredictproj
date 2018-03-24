#########################################################################################
# Categories for continuation
#########################################################################################

# -----------------------------------------------------------------------------
# define groupings that govern continuation behavior for individual expiring subscriptions
# the grouping that determines how renewed subses are replaced by new subses

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

