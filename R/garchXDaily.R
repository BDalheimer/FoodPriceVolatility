priceData = data.table(read.csv("~/GitHub/FoodPriceVolatility/Data/wheatPriceIndexMundi.csv"))
priceData[, timeLine := as.Date(paste("01", as.character(Month)), format = "%d %b %Y")]
policyCount = readRDS("~/GitHub/FoodPriceVolatility/Data/policyCount.RDA")


garchData = merge(policyCount, priceData[, .(timeLine, Price)], by = "timeLine", all.x = T)
setnames(garchData, "N", "weightedIndicator")
garchData[, dummy := ifelse(timeLine <= "2011-12-01", 0,1)]
adfTests = sapply(garchData[, -"timeLine", with = F], ur.df, type = "trend", selectlags = "AIC")
# ADF tests on first Difference
adfPriceDiff = ur.df(diff(garchData[, Price]), type = "drift", selectlags = "AIC")
adfIndicatorDiff = ur.df(diff(garchData[, weightedIndicator]), type = "drift", selectlags = "AIC")


# kpss confirmatory test
#kpssTest = sapply(garchData[, -"timeLine", with = FALSE], kpss.test, lshort = T)
#ppTest = sapply(garchData[, - "timeLine", with = FALSE], ur.pp, lags = "short")
kpssTest = sapply(garchData[, -"timeLine", with = FALSE], ur.kpss, type = "mu", lags = "short")

#+ unitRootTestTable, echo = FALSE, message = FALSE, warning = FALSE
outputTable = data.table("Time Series" = c("Wheat Price", "Export Restriction Indicator", 
                                           "Critical Value 0.99", "Critical Value 0.95", "Critical Value 0.90"), 
                         "ADF" = c(adfTests$Price@teststat[1], adfTests$weightedIndicator@teststat[1], adfTests$Price@cval[1],
                                   adfTests$Price@cval[4], adfTests$Price@cval[7]),
                         "ADF 1st Diff" = c(adfPriceDiff@teststat[1], adfIndicatorDiff@teststat[1], adfPriceDiff@cval[1],
                                            adfPriceDiff@cval[3], adfPriceDiff@cval[5]  ),
                         "KPSS" =c(kpssTest$Price@teststat, kpssTest$weightedIndicator@teststat, kpssTest$Price@cval[1],
                                   kpssTest$Price@cval[2], kpssTest$Price@cval[3]))


kable(outputTable, format="pandoc", caption = "Unit Root Tests", digits = 2)


testVar = VAR(y = cbind(diff(garchData[, Price]), garchData[, weightedIndicator[2:length(weightedIndicator)]], garchData[, dummy]), type = ("none"), ic ="AIC", lag.max = 20)



ARDLData = embed(cbind(garchData[, Price][2:length(garchData[,Price])], garchData[, weightedIndicator][2:length(garchData[, weightedIndicator])], diff(garchData[, Price]), diff(garchData[, weightedIndicator])), 2)

ARDLData = ts(garchData, start = c(2002,1), end = c(2012, 12), frequency = 12)
fit1 = dynlm(d(Price) ~ L(d(Price)) + L(d(Price, 5)) + L(d(weightedIndicator, 2)) + L(weightedIndicator, 4) + L(weightedIndicator,6) +
              L(Price) + L(weightedIndicator, 1), data = ARDLData)
bgtest(formula = fit4, order = 5)
acTests = lapply(1:20, bgtest, formula = fit4)

fit2 = dynlm(d(Price) ~ L(d(Price)) +  L(d(Price, 2)) + L(d(Price, 3)) + L(d(Price, 4)) + L(d(Price, 5)) + L(d(weightedIndicator)) + L(d(weightedIndicator, 2)) + 
               L(d(weightedIndicator, 3)) +L(d(weightedIndicator, 4)) +L(d(weightedIndicator, 5)) + L(d(weightedIndicator,6)) +
               L(Price) + L(weightedIndicator), data = ARDLData)

acTestResults = data.table("LM"  = c(acTests[[1]]$parameter, acTests[[2]]$parameter, acTests[[3]]$parameter, acTests[[4]]$parameter, acTests[[5]]$parameter, acTests[[6]]$parameter),
                           "p-value" = c( acTests[[1]]$p.value, acTests[[2]]$p.value, acTests[[3]]$p.value, acTests[[4]]$p.value, acTests[[5]]$p.value, acTests[[6]]$p.value))
            
acTestResults

fit3 = dynlm(d(Price) ~ dummy + L(d(Price)) + L(d(Price, 2)) + L(d(Price, 4)) + L(d(Price, 5))+ L(d(weightedIndicator, 1)) + L(d(weightedIndicator, 2)) + L(Price) + L(weightedIndicator), data = ARDLData)

fit4 = dynlm(d(Price) ~ L(d(Price)) + L(d(Price, 2)) + L(d(weightedIndicator)) + L(d(weightedIndicator, 5)) + L(d(Price, 8)) + L(d(weightedIndicator, 10)) +
                            L(d(Price), 10) + L(d(weightedIndicator), 12) + L(Price) + L(weightedIndicator), data = ARDLData) 

linearHypothesis(fit3, c('L(Price) = 0', 'L(weightedIndicator) = 0'), test = "F")
waldtest(fit2, fit3 )

garchData = garchData[timeLine >= "2003-01-01",]
garchData = garchData[timeLine <= "2011-10-01",]
tets = lm(diff(garchData[, Price]) ~ garchData[, weightedIndicator[2:length(weightedIndicator)]])
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                         submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = matrix(residuals(testVar)[,1])), 
                                     distribution.model = "norm")

garch <- ugarchfit(spec=spec,data=diff(garchData[, Price[3:length(Price)]]),solver.control=list(trace=0))         


spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                         submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = matrix(log(garchData[, weightedIndicator]), 
                   distribution.model = "norm")

garch <- ugarchfit(spec=spec,data=diff(garchData[, Price]),solver.control=list(trace=0))         
