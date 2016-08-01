# This function returns OLS models 
library(data.table)
library(xts)
library(dynlm)

estimateOLS = function(indicatorType = c("policyTradeWeight","TradeWeight")){
  
  garchData = readRDS("Data/garchDataLogged.RDA")
  modelSelectionTable = readRDS("Data/modelSelectionTable.RDA")
  
  wheatPriceList = readRDS("Data/wheatPriceList.RDA")
  
  
  monthlyVolatilites = ts(
    sapply(wheatPriceList, function(x){
      
      priceData = garchData[, .(Date, get(x))]                  
      setnames(priceData, 2, x)
      priceDataXTS = xts(priceData[, get(x)], order.by = priceData[, Date])
      monthly = apply.monthly(priceDataXTS, function(x){
        
        volatility = sum((x - mean(x))^2)
        
        volatility = volatility / (length(x)-1)
        
        volatility * sqrt(12)})
      
    }), start = c(2002, 1), end = c(2015,12), frequency = 12) 
  
  
  if(indicatorType == "policyTradeWeight"){
  indicators = readRDS("Data/indicatorPolicyWeightedMonthly.RDA")
  indicators[is.na(indicators)] = 0
  monthlyIndicators = ts(indicators[, .(`Export prohibition`, `Export quota`, `Export tax`)], 
                         start = c(2002, 1), end = c(2015,12), frequency = 12)
  }else if(indicatorType == "TradeWeight"){
    indicators = readRDS("Data/indicatorNoPolicyWeightedMonthly.RDA")
    indicators[is.na(indicators)] = 0
    names(indicators) = c("timeLine", "Export prohibition", "Export quota", "Export tax", "quotaBans")
    monthlyIndicators = ts(indicators[, .(`Export prohibition`, `Export quota`, `Export tax`)], 
                           start = c(2002, 1), end = c(2015,12), frequency = 12)
    
  }
    
  
  
  OLSData = ts(cbind.data.frame(monthlyVolatilites, monthlyIndicators), start = c(2002, 1), end = c(2015,12), frequency = 12)
  OLSData[is.na(OLSData)] = 0
  
  olsModels = lapply(wheatPriceList, function(x){
    ols = dynlm(data = OLSData, get(x) ~ L(`Export prohibition`) + L(`Export quota`) + L(`Export tax`))
    
    
    model = as.data.table(coef(summary(ols)), keep.rownames = T)
    setnames(model, 4, "TStatistic")
    model[, Estimate := ifelse(abs(TStatistic) > 2.326, paste(round(Estimate, digits = 4), "***", sep = ""),
                               ifelse(abs(TStatistic) < 2.326 & abs(TStatistic) > 1.960, paste(round(Estimate, digits = 4), "**", sep = ""),
                                      ifelse(abs(TStatistic) > 1.645 & abs(TStatistic) < 1.960, paste(round(Estimate, digits = 4), "*", sep = ""), round(Estimate, digits =4))))]
    
    model[, .(rn, Estimate)]
  })
  
  
  olsTable = cbind(olsModels[[1]][, rn], olsModels[[1]][, Estimate],
                   olsModels[[2]][, Estimate], olsModels[[3]][, Estimate], 
                   olsModels[[4]][, Estimate], olsModels[[5]][, Estimate], olsModels[[6]][, Estimate],
                   olsModels[[7]][, Estimate], olsModels[[8]][, Estimate], olsModels[[9]][, Estimate],
                   olsModels[[10]][, Estimate], olsModels[[11]][, Estimate], olsModels[[12]][, Estimate],
                   olsModels[[13]][, Estimate], olsModels[[14]][, Estimate])
  olsTable = data.table(olsTable)
  names(olsTable) = c("Parameter", wheatPriceList)
  
  olsTableUSOne = olsTable[, .(Parameter, WHEATSF, WHEATF1, SHUSGC1, SHNOLC1, SSNOLC1)]
  olsTableUSTwo = olsTable[, .(Parameter, SSUSGC1, WHTKANS, HRWWNO2, WUSHRWO)]
  olsTableRussia = olsTable[, .(Parameter, WECRPP1, WENCPP1, WESBPP1, WEURPP1, WEVRPP1)]
  
  list(olsTableUSOne, olsTableUSTwo, olsTableRussia)
}