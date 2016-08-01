
estimateEGARCHX = function(priceCode = "SHUSGC1", indicatorType = c("policyTradeWeight", "TradeWeight"), 
                            periodBreaks = c("2002-01-01", "2007-05-10", "2008-03-01", "2010-06-01", "2011-05-27"), 
                            lag = 1){


garchData = readRDS("Data/garchDataLogged.RDA")
modelSelectionTable = readRDS("Data/modelSelectionTable.RDA")
garchData = garchData[Date > "2001-12-31", ]
garchData = garchData[Date < "2016-01-01",]

garchData[, c("Export prohibition", "Export quota", "Export tax", "Export prohibitionNoPweight", "Export quotaNoPweight", "Export taxNoPweight") 
          := lapply(.SD, shift, lag), .SDcols = c("Export prohibition", "Export quota", "Export tax", "Export prohibitionNoPweight", "Export quotaNoPweight", "Export taxNoPweight")]

if(indicatorType == "policyTradeWeight"){
  externalRegsWeighted = garchData[-c(1:lag), .(`Export prohibition`, `Export quota`, `Export tax`)]
}else if(indicatorType == "TradeWeight"){
  externalRegsWeighted = garchData[-c(1:lag), .(`Export prohibitionNoPweight`, `Export quotaNoPweight`, `Export taxNoPweight`)]
  colnames(externalRegsWeighted) = c("Export prohibition", "Export quota", "Export tax")
}

externalRegsWeightedMatrix = as.matrix(externalRegsWeighted)
# substitute NA's with zeroes
externalRegsWeightedMatrix[is.na(externalRegsWeightedMatrix)] = 0
externalRegsWeightedMatrixSquared = externalRegsWeightedMatrix^2

eGarchXSpec = ugarchspec(variance.model = list(model="eGARCH", submodel = NULL, garchOrder = c(1,1),  external.regressors = externalRegsWeightedMatrixSquared), 
                         mean.model = list(armaOrder = c(1,1), external.regressors = externalRegsWeightedMatrix,
                                           include.mean = F), distribution.model ="std")

eGarchX = ugarchfit(eGarchXSpec, diff(garchData[-c(1:lag), get(priceCode)]), solver.control=list(trace=0), solver="gosolnp", fit.control =
                      list())

eGarchTable = as.data.table(eGarchX@fit$robust.matcoef, keep.rownames = T)
eGarchTable = eGarchTable[!rn %in% c("mu", "ar1", "ma1", "mxreg1", "mxreg2", "mxreg3", "shape"), ]
Parameter = c("$\\omega$", "$\\alpha$", "$\\beta$", "$\\gamma$", "Export Bans", "Export Quotas", "Export Taxes")

eGarchTable = cbind(Parameter, eGarchTable[, -"rn", with =F])

## Crisis Models

if(!is.null(periodBreaks)){
preCrisis= seq(as.Date(periodBreaks[1], format = "%Y-%m-%d"), as.Date(periodBreaks[2],format = "%Y-%m-%d") - 1, by = "day")
crisisPeriodOne = seq(as.Date(periodBreaks[2], format = "%Y-%m-%d"), as.Date(periodBreaks[3], format = "%Y-%m-%d") - 1, by = "day")
betweenCrisis = seq(as.Date(periodBreaks[3], format = "%Y-%m-%d"), as.Date(periodBreaks[4],format = "%Y-%m-%d") - 1, by = "day")
crisisPeriodTwo = seq(as.Date(periodBreaks[4], format = "%Y-%m-%d"), as.Date(periodBreaks[5],format = "%Y-%m-%d") - 1 , by = "day")
postCrisis =  seq(as.Date(periodBreaks[5], format = "%Y-%m-%d"), as.Date("2015-12-31",format = "%Y-%m-%d"), by = "day")

periodList = c("preCrisis", "crisisPeriodOne", "betweenCrisis", "crisisPeriodTwo", "postCrisis")

crisisModels = lapply(periodList, function(x){
  garchData = readRDS("Data/garchDataLogged.RDA")
  period = get(x)
  data = garchData[ Date %in% period,]
  
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
  
  
  eGarchSpecCrisis = ugarchspec(variance.model = list(model="eGARCH", submodel = NULL, garchOrder = c(1,1), external.regressors = externalRegsWeightedMatrixSquared), 
                                mean.model = list(armaOrder = c(1,1), external.regressors = externalRegsWeightedMatrix,
                                                  include.mean = F), distribution.model ="std")
  
  eGarchXCrisis = ugarchfit(eGarchSpecCrisis, diff(data[-c(1:lag), get(priceCode)]), solver.control=list(trace=0), solver="hybrid", fit.control =
                              list())
  
  
  output = as.data.table(eGarchXCrisis@fit$robust.matcoef, keep.rownames = T)
  output[, period := x]
  return(output)
})


modelTables = lapply(1:length(crisisModels), function(x){
  modelOne = rbindlist(crisisModels[x])
  setnames(modelOne, c(2, 4), c("Estimate", "TStatistic"))
  modelOne[, Estimate := as.numeric(Estimate)]
  modelOne[, Estimate := ifelse(abs(TStatistic) > 2.326, paste(round(Estimate, digits = 4), "***", sep = ""),
                                ifelse(abs(TStatistic) < 2.326 & abs(TStatistic) > 1.960, paste(round(Estimate, digits = 4), "**", sep = ""),
                                       ifelse(abs(TStatistic) > 1.645 & abs(TStatistic) < 1.960, paste(round(Estimate, digits = 4), "*", sep = ""), round(Estimate, digits =4))))]
  
})


reducedTable = lapply(1:5, function(x){
  
  model = rbindlist(modelTables[x])
  model = model[!rn %in% c("mu", "ar1", "ma1", "mxreg1", "mxreg2", "mxreg3", "shape"), ]
})

crisisTable = data.table(
  Parameter = c("$\\omega$", "$\\alpha$", "$\\beta$", "$\\gamma$", "Export Bans", "Export Quotas", "Export Taxes"),
  "Pre Crisis" = reducedTable[[1]]$Estimate,
  "2007/08 Crisis" = reducedTable[[2]]$Estimate,
  "Between Crisis" = reducedTable[[3]]$Estimate,
  "2010/11 Crisis" = reducedTable[[4]]$Estimate,
  "Post Crisis" = reducedTable[[5]]$Estimate
)
return(list(eGarchTable, crisisTable))

}else{
  
return(eGarchTable)
}
}
