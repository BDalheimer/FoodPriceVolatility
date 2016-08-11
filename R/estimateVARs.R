#' This function estimates VAR models 

library(data.table)
library(rugarch)
library(vars)
library(xts)
library(xtable)

estimateVARs = function(priceCodes = c("SHUSGC1", "WENCPP1"), volatilityType = c("realized", "GARCH"), 
                   indicatorType = c("policyTradeWeight", "TradeWeight", "countryIndicators", "custom"), frequency = c("monthly", "weekly"),
                   lag = c(1,2), exogenNames = NULL){
   
  wheatPriceList = readRDS("Data/wheatPriceList.RDA")                  
                   

  if(volatilityType == "realized"){
  if(frequency == "weekly"){stop("Realized Volatility must be calculated within months")}
  garchData =readRDS("Data/garchDataLogged.RDA")
  modelSelectionTable = readRDS("Data/modelSelectionTable.RDA")
  
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
  
  ## workaround only weekly avaialble russian Data --> GATCH volatility and prick 3rd observation of each month
  if(priceCodes[2] %in% c("WECRPP1", "WENCPP1", "WESBPP1", "WEURPP1", "WEVRPP1", "WEUAPP1")){
    garchDataWeekly = readRDS("Data/garchDataLoggedWeekly.RDA")
    
    russiaPrice = xts(diff(garchDataWeekly[, get(priceCodes[2])]), order.by = garchDataWeekly[-1, Date])
    
    testSpec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),  external.regressors = NULL), 
                          mean.model = list(armaOrder = c(52,0),
                                            include.mean = TRUE, external.regressors = NULL), distribution.model = "norm")
    
    testModel = ugarchfit(testSpec, russiaPrice, solver.control=list(trace = 0), solver="hybrid", fit.control =
                            list())
    
    
    volatility = sigma(testModel) * sqrt(52)
    
    volatilityXTS = xts(volatility, order.by = garchDataWeekly[-1, Date])
    russiaVolatility = apply.monthly(volatilityXTS, function(x){ x[length(x)] })
  }else{
    russiaVolatility = monthlyVolatilites[, priceCodes[2]]
    seasonalityControlRussia = residuals(arima(monthlyVolatilites[, priceCodes[2]], order = c(12,0,0)))
  }
  
  
  seasonalityControlUS = residuals(arima(monthlyVolatilites[, priceCodes[1]], order = c(12,0,0)))
  
  usVolatility = monthlyVolatilites[, priceCodes[1]]
  
  
}else if(volatilityType == "GARCH"){
  garchData = readRDS("Data/garchDataLoggedWeekly.RDA")
  
  weeklyModelSelectionTable = readRDS("Data/weeklyModelSelectionTable.RDA")
  
  estimateGarch = function(x){
    
    
    testSpec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),  external.regressors = NULL), 
                          mean.model = list(armaOrder = c(52,0),
                                            include.mean = TRUE, external.regressors = NULL), distribution.model = "norm")
    
    testModel = ugarchfit(testSpec, diff(garchData[, get(x)]), solver.control=list(trace=0), solver="hybrid", fit.control =
                            list())
    
    
    sigma(testModel) * sqrt(52)
    
  }
  
  garchParam = lapply(priceCodes, function(x){
    return(tryCatch(estimateGarch(x), error=function(e) NULL))
  })
  
  usVolatility = unlist(garchParam[1])
  russiaVolatility = unlist(garchParam[2])
}      
  
  
                     if(indicatorType == "policyTradeWeight"){
                       if(frequency == "monthly"){
                       indicators = readRDS("Data/indicatorPolicyWeightedMonthly.RDA")
                       indicators = indicators[, .(`Export prohibition`, `Export quota`, `Export tax`)]
                       indicators = as.matrix(indicators)
                       }else{
                         countryIndicators = data.table(readRDS("Data/indicatorPolicyWeighted.RDA"))
                         setnames(countryIndicators, "timeLine", "Date")
                         
                         intermediateData = merge(garchData[, .(Date)], countryIndicators[, .(Date, `Export prohibition`, `Export quota`, `Export tax`)], by = "Date", all.x = T)
                         indicators = as.matrix(intermediateData[, -"Date", with = F])
                       }
                       indicators[is.na(indicators)] = 0
                       #indicators = ts(indicators[, .(`Export prohibition`, `Export quota`, `Export tax`)], 
                        #                     start = c(2002, 1), end = c(2015,12), frequency = 12)
                       varExogenLagged = embed(indicators, 3)
                       varNames = colnames(indicators)
                       allVarNames = lapply(0:2, function(x){ 
                         
                         paste(varNames, "Lag", x, sep="")
                       })
                       colnames(varExogenLagged) = unlist(allVarNames)
                       if(lag == 1){
                       varExogenLagged = varExogenLagged[, 4:6]
                       }else if(lag == 2){
                         varExogenLagged = varExogenLagged[, 7:9]
                       }
                     }else if(indicatorType == "TradeWeight"){
                       if(frequency == "monthly"){
                         indicators = readRDS("Data/indicatorNoPolicyWeightedMonthly.RDA")
                       }else{
                         countryIndicators = data.table(readRDS("Data/indicatorNoPolicyWeighted.RDA"))
                         setnames(countryIndicators, "timeLine", "Date")
                         intermediateData = merge(garchData[, .(Date)], countryIndicators, by = "Date", all.x = T)
                         indicators = as.matrix(intermediateData)
                       }

                       indicators[is.na(indicators)] = 0
                       names(indicators) = c("timeLine", "Export prohibition", "Export quota", "Export tax", "quotaBans")
                       monthlyIndicators = ts(indicators[, .(`Export prohibition`, `Export quota`, `Export tax`)], 
                                              start = c(2002, 1), end = c(2015,12), frequency = 12)
                       varExogenLagged = embed(monthlyIndicators, 3)
                       varNames = colnames(monthlyIndicators)
                       allVarNames = lapply(0:2, function(x){ 
                         
                         paste(varNames, "Lag", x, sep="")
                       })
                       colnames(varExogenLagged) = unlist(allVarNames)
                       if(lag == 1){
                         varExogenLagged = varExogenLagged[, 4:6]
                       }else if(lag == 2){
                         varExogenLagged = varExogenLagged[, 7:9]
                       }
                     }else if(indicatorType == "countryIndicators" | indicatorType == "custom"){
                       if(frequency == "monthly"){
                       countryIndicators = readRDS("Data/countryIndicatorsMonthly.RDA")
                       setnames(countryIndicators, "timeLine", "Date")
                       }else{
                         garchData = readRDS("Data/garchDataLoggedWeekly.RDA")
                         countryIndicators = data.table(readRDS("Data/countryIndicators.RDA"))
                         setnames(countryIndicators, "timeLine", "Date")
                         #garchData = readRDS("Data/garchDataLogged.RDA")
                         intermediateData = merge(garchData[, .(Date)], countryIndicators, by = "Date", all.x = T)
                         countryIndicators = intermediateData
                       }
                       

                       countryIndicators = as.matrix(countryIndicators[, -"Date", with = F])
                    
                       varExogenLagged = embed(countryIndicators, 3)
                       varExogenLagged[is.na(varExogenLagged)] = 0
                       
                       varNames = colnames(countryIndicators)
                       
                       allVarNames = lapply(0:2, function(x){ 
                         
                         paste(varNames, "Lag", x, sep="")
                       })
                       colnames(varExogenLagged) = unlist(allVarNames)
                       
                      
                       if(indicatorType == "custom" & !is.null(exogenNames)){
                         varExogenLagged = varExogenLagged[, exogenNames]
                       }else{
                         if(lag == 1){
                       varExogenLagged = varExogenLagged[,20:38]
                         }else if(lag == 2){
                           varExogenLagged = varExogenLagged[,39:57]
                         }
                       }
                     }      
      if(volatilityType == "GARCH"){               
       endogen = cbind(usVolatility[-1], russiaVolatility[-1]) 
      }else{
        endogen = cbind(usVolatility[-c(1:2)], russiaVolatility[-c(1:2)])
      }
       colnames(endogen) = priceCodes
       if(frequency == "monthly"){
         varExogenLagged = cbind(varExogenLagged, seasonalityControlUS[-c(1:2)], deparse.level = 2)
         colnames(varExogenLagged) = gsub("seasonalit...", "seasonalityControlUS", colnames(varExogenLagged))
         varModel = VAR(y = endogen, exogen = varExogenLagged, lag.max = 3, ic = "SC", type = "const")
       }
  varModel = VAR(y = endogen, exogen = varExogenLagged, lag.max = 3, ic = "SC", type = "const")
                     
varTables = lapply(1:2, function(x){
  
  
  xtable(varModel$varresult[[x]])
  
})     
                     
      varTables[]               
                     
                   }