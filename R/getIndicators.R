
getIndicators = function(frequency = "daily", save = FALSE, 
                         type = c("all", "policyTradeWeight", 
                                  "TradeWeight", "count", "countryIndicators")){

library(data.table)
library(zoo)
library(dygraphs)
library(xts)
library(countrycode)
source("R/policyStringData.R")

amisData = readRDS("Data/standardizedDailyAmisData.RDA")

oecdData = readRDS("nonAmisData.RDA")
policyData = rbind(amisData, oecdData)
policyData[, value := as.numeric(value)]
stretchedPolicyData = rbindlist(lapply(unique(policyData[, cpl_id ]), policyTimeSeries, frequency = frequency, policyData = policyData)) 


stretchedPolicyData[, iso3c := as.character(countrycode(country_name, 
                                                        origin = "country.name", 
                                                        destination = "iso3c", 
                                                        warn = T))] 

stretchedPolicyData[, period := substring(timeLine,1,4)]

## USDA export data, this needs to be replaced with Trade map data and HS codes, preferrably monthly
usdaExportData = readRDS("Data/usdaExportData.RDA")

averageExportShares = data.table(aggregate(data = usdaExportData,
                                           exportShare ~ iso3c + period, mean))
# lag exportshares for exogeneity
averageExportShares[, period := as.numeric(period)]
indicatorData = merge(stretchedPolicyData, usdaExportData, by=c("iso3c", "period"), all.x = T)
indicatorData[, value := as.numeric(value)]
taxMax = 60
taxMin = 0
quotaMax = 4000000
quotaMin = 0
 indicatorData[, indicatorValues := ifelse(policymeasure_name == "Export quota", value / quotaMax, 
                                                 ifelse(policymeasure_name == "Export tax", value / taxMax, 
                                                        ifelse(policymeasure_name == "Export prohibition", 1, 0)))]

#  indicatorData[, indicatorValues := ifelse(policymeasure_name == "Export quota", 2,
#                                            ifelse(policymeasure_name == "Export tax", 1, 
#                                                   ifelse(policymeasure_name == "Export prohibition", 3, 0)))]


#indicatorData[, indicatorCount := .N, by = timeLine]

 # policy and trade weighted Indicator
policyCount = indicatorData[, .N, by = c("timeLine", "policymeasure_name")]
setnames(policyCount, "N", "globalPolicies")
countryPolicyCount = indicatorData[, .N, by = c("timeLine","policymeasure_name", "iso3c")]
setnames(countryPolicyCount, "N", "countryCount")
quotaBansCount = aggregate(data = countryPolicyCount[policymeasure_name != "Export tax", ], 
                           countryCount  ~ timeLine + iso3c, sum)
quotaBansGlobal = indicatorData[policymeasure_name != "Export tax", .N, by = c("timeLine")]
quotaBansData = merge(quotaBansGlobal, quotaBansCount, by = "timeLine", all.x = TRUE)
quotaBansData[, period := substring(timeLine,1,4)]
quotaBansIndicatorData = merge(quotaBansData, usdaExportData, all.x = TRUE, by = c("period", "iso3c"))
quotaBansIndicatorData[, quotaBans := ((countryCount / N)   * exportShare) *100]
quotaBansIndicatorData[, quotaBansNoPWeight := (countryCount   * exportShare) *100]

quotaBans = data.table(aggregate(data = quotaBansIndicatorData, quotaBans ~ timeLine, sum))
quotaBansNoPWeight = data.table(aggregate(data = quotaBansIndicatorData, quotaBansNoPWeight ~ timeLine, sum))

countData = merge(countryPolicyCount, policyCount, by = c("timeLine", "policymeasure_name"))
countData[, period := substring(timeLine,1,4)]
newIndicator = merge(countData, usdaExportData, all.x = T, by = c("period", "iso3c"))
newIndicator[, indicatorNoPoliyWeight := ((countryCount   * exportShare) *100)]
newIndicator[, indicatorValue := ((countryCount / globalPolicies)   * exportShare) *100]

countryIndicators = dcast(newIndicator, timeLine ~ policymeasure_name + iso3c, value.var = "indicatorValue")
countryNoPolicyIndicators = dcast(newIndicator, timeLine ~ policymeasure_name + iso3c, value.var = "indicatorNoPoliyWeight")
globalIndicators = data.table(aggregate(data = newIndicator, indicatorValue ~ timeLine + policymeasure_name, sum))
globalNoPolicyIndicators = data.table(aggregate(data = newIndicator, indicatorNoPoliyWeight ~ timeLine + policymeasure_name, sum))





quotaValueIndicator = indicatorData[policymeasure_name == "Export quota",]
countryQuotaIndicators = data.table(aggregate(data = quotaValueIndicator, 
                                         indicatorValues ~ timeLine + country_name + iso3c + exportShare, sum))

countryQuotaIndicators[, indicatorValues := indicatorValues * 100]
countryQuotaIndicators[, weightedIndicator := exportShare * indicatorValues * 10000 ]

quotaIndicators = data.table(aggregate(data = countryQuotaIndicators, 
                                       indicatorValues ~ timeLine, sum))

indicatorDataWide = dcast(globalIndicators, timeLine ~ policymeasure_name, value.var = "indicatorValue")
test = merge(indicatorDataWide, quotaBans, by = "timeLine", all.x = TRUE)
indicatorDataNoPolicyWide = dcast(globalNoPolicyIndicators, timeLine ~ policymeasure_name, value.var = "indicatorNoPoliyWeight")
setnames(indicatorDataNoPolicyWide, c("Export prohibition", "Export quota", "Export tax"), c("Export prohibitionNoPweight", "Export quotaNoPweight", "Export taxNoPweight"))


#indicatorDataWideExtended = merge(indicatorDataWide, quotaIndicators, all.x = T, by = "timeLine")
#setnames(indicatorDataWideExtended, "weightedIndicator", "weightedQuotas")


taxValueIndicator = indicatorData[policymeasure_name == "Export tax",]

countryTaxIndicators = data.table(aggregate(data = taxValueIndicator, 
                                              indicatorValues ~ timeLine + country_name + iso3c + exportShare, sum))

countryTaxIndicators[, indicatorValues := indicatorValues * 100]


countryTaxIndicators[, weightedIndicator := exportShare * indicatorValues * 100 ]

taxIndicators = data.table(aggregate(data = countryTaxIndicators, 
                                       weightedIndicator ~ timeLine, sum))
setnames(quotaIndicators, "indicatorValues", "measuredQuota")
setnames(taxIndicators, "weightedIndicator", "measuredTax")
# library(ggplot2)
# 
# ggplot(data=globalIndicators,
#        aes(x=timeLine, y=indicatorValue, colour=policymeasure_name)) +
#   geom_line()

indicatorDataWide = merge(indicatorDataWide, quotaBans, by = "timeLine", all.x = TRUE)
indicatorDataNoPolicyWide = merge(indicatorDataNoPolicyWide, quotaBansNoPWeight, by = "timeLine", all.x = TRUE)
if(save == TRUE){
if(frequency == "daily"){
saveRDS(indicatorDataWide, "Data/indicatorPolicyWeighted.RDA")
saveRDS(indicatorDataNoPolicyWide, "Data/indicatorNoPolicyWeighted.RDA")  
saveRDS(policyCount, "Data/policyCount.RDA")
saveRDS(countryIndicators, "Data/countryIndicators.RDA")
}else{
  saveRDS(indicatorDataWide, "Data/indicatorPolicyWeightedMonthly.RDA")
  saveRDS(indicatorDataNoPolicyWide, "Data/indicatorNoPolicyWeightedMonthly.RDA")  
  saveRDS(policyCount, "Data/policyCountMonthly.RDA")
  saveRDS(countryIndicators, "Data/countryIndicatorsMonthly.RDA")
}
}
if(type == "policyTradeWeight"){
  return(indicatorDataWide)
}else if(type == "tradeWeight"){
  return(indicatorDataNoPolicyWide)
}else if(type == "count"){
    return(policyCount)
}else if(type == "countryIndicators"){
  return(countryIndicators)
}else{
  exportRestrictionsIndicatorsList = list(indicatorDataWide, indicatorDataNoPolicyWide, policyCount, countryIndicators)
  return(exportRestrictionsIndicatorsList)
}
  
    
}
