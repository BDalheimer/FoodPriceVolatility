
volatilityTables = function(x){
  
  # monthly realized volatility
  garchData =readRDS("Data/garchDataLogged.RDA")
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
      #volatility})
    }), start = c(2002, 1), end = c(2015,12), frequency = 12)
  
  monthlyVolatilityTable = as.data.table(monthlyVolatilites)
#   monthlyPlotData = monthlyVolatilityTable[, -c("WECRPP1", "WENCPP1", "WESBPP1", "WEURPP1", "WEVRPP1"), with = F]
#   meltdf = melt(monthlyVolatilites)
#   meltdf = meltdf[!meltdf$Var2 %in% c("WECRPP1", "WENCPP1", "WESBPP1", "WEURPP1", "WEVRPP1"), ]
#   meltdf = sqrt(meltdf)
#   ggplot(meltdf,aes(x=Var1, y=sqrt(value))) + geom_line() +
#     facet_wrap(~ Var2, scales = 'free_y')
  
  
  monthlyVolatilityTable = monthlyVolatilityTable[, -c("WECRPP1", "WENCPP1", "WESBPP1", "WEURPP1", "WEVRPP1"), with = F]
 
  descriptives = lapply(c(mean, sd, min, max, skewness, kurtosis), function(x){
    
    sapply(monthlyVolatilityTable, FUN = x)
    
  }) 
  
   realizedTable = data.table(sqrt(t(do.call(rbind, descriptives))), keep.rownames = TRUE)
   names(realizedTable) = c("Series", "Mean", "SD", "Min", "Max", "Skewness", "Kurtosis")
   realizedTable[, start := "01/2002"]
   realizedTable[, end := "12/2015"]
   #realizedTable[, frequency := "monthly"]
  # realizedTable[, volatilityType := "realized"]
   setcolorder(realizedTable, c("Series", "start", "end", "Mean", "SD", "Min", "Max", "Skewness", "Kurtosis"))
    garchData = readRDS("Data/garchDataLoggedWeekly.RDA")
   
  weeklyModelSelectionTable = readRDS("Data/weeklyModelSelectionTable.RDA")
   
   estimateGarchDistr = function(x){
     
    
    testSpec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),  external.regressors = NULL), 
                          mean.model = list(armaOrder = c(52,0),
                                            include.mean = TRUE, external.regressors = NULL), distribution.model = "norm")
    
    testModel = ugarchfit(testSpec, diff(garchData[, get(x)]), solver.control=list(trace=0), solver="hybrid", fit.control =
                            list())
    
    
    volatility = sigma(testModel) * sqrt(52)
data.table(Series = x, Mean = mean(volatility), SD = sd(volatility), Min = min(volatility), Max = max(volatility), Skewness = skewness(volatility),
                Kurtosis = kurtosis(volatility))

  }
  
  volatilities = lapply(wheatPriceList, function(x){
    return(tryCatch(estimateGarchDistr(x), error=function(e) NULL))
  })
  
  names(volatilities) = wheatPriceList
  weeklyVolatilities = rbindlist(volatilities)
  weeklyVolatilities[, start := "1/2002"]
  weeklyVolatilities[, end := "52/2015"]
  setcolorder(weeklyVolatilities, c("Series", "start", "end", "Mean", "SD", "Min", "Max", "Skewness", "Kurtosis"))
  
   return(list(realizedTable =realizedTable[], weeklyVolatilities = weeklyVolatilities[]))
}
