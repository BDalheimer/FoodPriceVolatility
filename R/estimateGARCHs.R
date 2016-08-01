#' This function estimates GARCH-X models


estimateGARCHs = function(priceCodes = wheatPriceList, indicatorType = c("policyTradeWeight", "TradeWeight"),
                          frequency = c("daily", "weekly"), lag = 1){

  wheatPriceList = readRDS("Data/wheatPriceList.RDA")
if(frequency == "daily"){  
data = readRDS("Data/garchDataLogged.RDA")
modelSelectionTable = readRDS("Data/modelSelectionTable.RDA")
}else if(frequency == "weekly"){
data = readRDS("Data/garchDataLoggedWeekly.RDA")

modelSelectionTable = readRDS("Data/weeklyModelSelectionTable.RDA")
}

data[, c("Export prohibition", "Export quota", "Export tax", "Export prohibitionNoPweight", "Export quotaNoPweight", "Export taxNoPweight") 
     := lapply(.SD, shift, lag), .SDcols = c("Export prohibition", "Export quota", "Export tax", "Export prohibitionNoPweight", "Export quotaNoPweight", "Export taxNoPweight")]

if(indicatorType == "policyTradeWeight"){
externalRegsWeighted = data[-c(1:lag), .(`Export prohibition`, `Export quota`, `Export tax`)]
}else if(indicatorType == "TradeWeight"){
  externalRegsWeighted = data[-c(1:lag), .(`Export prohibitionNoPweight`, `Export quotaNoPweight`, `Export taxNoPweight`)]
  colnames(externalRegsWeighted) = c("Export prohibition", "Export quota", "Export tax")
}

externalRegsWeightedMatrix = as.matrix(externalRegsWeighted)
# substitute NA's with zeroes
externalRegsWeightedMatrix[is.na(externalRegsWeightedMatrix)] = 0
externalRegsWeightedMatrixSquared = externalRegsWeightedMatrix^2


estimateGarchX = function(x){
  

  testSpecX = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),  external.regressors = externalRegsWeightedMatrixSquared), 
                         mean.model = list(armaOrder = c(as.numeric(substring(modelSelectionTable[OriginalCode == x, arimaSpec], 3,3)),
                                                         as.numeric(substring(modelSelectionTable[OriginalCode == x, arimaSpec], 9,9))),
                                           include.mean = TRUE, external.regressors = externalRegsWeightedMatrix), distribution.model = "norm")
  
  testModelX = ugarchfit(testSpecX, diff(data[-(1:lag), get(x)]), solver.control=list(trace=0), solver="hybrid", fit.control =
                           list())
  
  
  
  out = data.table(testModelX@fit$robust.matcoef, keep.rownames = TRUE)
  out = out[, c(1,2,4), with = F]
  names(out) =  c("Parameter", "Estimate",  "TStatistic")
  out[, OriginalCode := x]
  out[]
  
}

garchXParam = lapply(priceCodes, function(x){
  return(tryCatch(estimateGarchX(x), error=function(e) NULL))
})

longGarchParams = rbindlist(garchXParam)

longGarchParams[, Significance  := ifelse(abs(TStatistic) > 2.326, "***",
                                          ifelse(abs(TStatistic) < 2.326 & abs(TStatistic) > 1.960, "**",
                                                 ifelse(abs(TStatistic) > 1.645 & abs(TStatistic) < 1.960, "*", 
                                                        "")))]
longGarchParams[, Estimate := round(Estimate, digits = 6)]
longGarchParams[, Estimate := paste(Estimate, Significance, sep ="")]

wideGarchParams = dcast(longGarchParams, Parameter ~ OriginalCode, value.var = c("Estimate"))
garchParameterExpressions = readRDS("Data/garchParameterExpressions.RDA")

wideGarchParams[, Parameter := factor(as.character(Parameter), levels = garchParameterExpressions[, Parameter])]
wideGarchParams = wideGarchParams[order(Parameter),]
wideGarchParams[is.na(wideGarchParams)] = "-"
          
garchResulTable = merge(wideGarchParams, garchParameterExpressions, by = "Parameter", all.x = T)
garchResulTable[, Parameter := expression]

garchResulTable[, -"expression", with = F]
}