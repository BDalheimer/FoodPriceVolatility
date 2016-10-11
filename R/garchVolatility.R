## this function estimates weekly GARCH volatilities

garchVolatility = function(annualize = T, frequency = c("monthly", "weekly")){
library(rugarch)
wheatPriceList = c("WHEATSF", "WHEATHD", "WHEATF1")
  
if(frequency == "monthly"){
  arOrder = 12
  garchData = makeGarchData(save = FALSE, log = F, frequency = "monthly", output = T, 
                                               indicatorType = "policyTradeWeight", wheatPrice = NULL, 
                                               dataRange = c("2002-01-01", "2014-12-08"), policyWeight = "max")
}else{
  arOrder = 52
  garchData = makeGarchData(save = FALSE, log = F, frequency = "weekly", output = T, 
                            indicatorType = "policyTradeWeight", wheatPrice = NULL, 
                            dataRange = c("2002-01-01", "2014-12-8"), policyWeight = "max")
}


weeklyModelSelectionTable = readRDS("Data/weeklyModelSelectionTable.RDA")

estimateGarch = function(x){
  
  
  testSpec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),  external.regressors = NULL), 
                        mean.model = list(armaOrder = c(arOrder,0),
                                          include.mean = F, external.regressors = NULL), distribution.model = "std")
  
   
  testModel =  ugarchfit(testSpec, diff(garchData[, get(x)]), solver.control=list(trace=0), solver="hybrid", fit.control =
                          list())
  

  sigma(testModel) * sqrt(arOrder) 

}

garchParam = lapply(wheatPriceList, function(x){
  return(tryCatch(estimateGarch(x), error=function(e) NULL))
})

spotSoftWinter = unlist(garchParam[1])
spotHardWinter = unlist(garchParam[2])
futures = unlist(garchParam[3])

weeklyVolatility = cbind(spotSoftWinter, spotHardWinter, futures) 
colnames(weeklyVolatility) = c("spotSoftWinter", "spotHardWinter", "futures")

return(weeklyVolatility)

}
