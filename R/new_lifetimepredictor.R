library(data.table)

#marketcat = "US"
#siteverkey <- "SS"
#model <- new_model  


simplepredictor3m <- function(model, marketname, siteverkey)
{
  # begin with a 1m subs at month 0
  mysubs <- data.table(marketname = marketname, paymentperiodchosenatstart = '3', months = 1, months_total = 0, siteverkey = siteverkey )
  mysubs2 <- mysubs %>% addChurnCatsEtc()
  
  sumlife <- 0.0
  numiterations <- 0

  # get the logmonthchurn for the first month
  logmonthchurninit <- predict(model, newdata = mysubs2, type="response")
  churnonemonth <- exp(logmonthchurninit)
  churnfull <- 1-(1-churnonemonth)^1
  continuedfractionfromhere <- 1 - churnfull
  remainingatnext <- 1 * continuedfractionfromhere
  nextsubs <- mysubs2 %>%
    dplyr::mutate(months = 3, months_total = months_total + 1) %>%
    dplyr::select(marketname, months, paymentperiodchosenatstart, months_total, siteverkey) %>%
    addChurnCatsEtc()
  
  sumlife <- 1 + simpleprop3msumlife(model, nextsubs, numiterations + 1, remainingatnext)
  
  print(paste(marketname, sumlife / 12))
}

simpleprop3msumlife <- function(model, mysubs, numiterations, remaininghere)
{
  #print(paste("----Iteration:", numiterations))
  #print(mysubs)
  #print(paste("Remaininghere:", remaininghere))
  if(remaininghere >= 0.001)
  {
    # get the logmonthchurn for a three month continuation
    logmonthchurninit <- predict(model, newdata = mysubs, type="response")
    churnonemonth <- exp(logmonthchurninit)
    churnfull <- 1-(1-churnonemonth)^3
    continuedfractionfromhere <- 1 - churnfull
    #print(paste("logmonthchurninit:", logmonthchurninit))
    #print(paste("churnonemonth:", churnonemonth))
    #print(paste("churnfull:", churnfull))
    #print(paste("Continuedfractionfromhere:", continuedfractionfromhere))
    remainingatnext <- remaininghere * continuedfractionfromhere
    #print(paste("remainingatnext:", remainingatnext))
    nextsubs <- mysubs %>%
      dplyr::mutate(months_total = months_total + 3) %>%
      dplyr::select(marketname, months, paymentperiodchosenatstart, months_total, siteverkey) %>%
      addChurnCatsEtc()

    thissubscontribution <- remaininghere * 3
    #print(paste("thissubscontribution:", thissubscontribution))
    
    sumlife <- thissubscontribution * 3 + simpleprop3msumlife(model, nextsubs, numiterations + 1, remainingatnext)
  } else {
    sumlife <- 0
  }
  
  sumlife
}
