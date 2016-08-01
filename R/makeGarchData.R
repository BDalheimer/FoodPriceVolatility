# this script sources raw data and provides an RDA object which contains all variables 
# used in the analysis in either logged or non-logged form

makeGarchData = function(save = FALSE, log = TRUE, frequency = c("daily", "weekly"), output = TRUE){
library(data.table)
library(rio)
source("R/getIndicators.R")
  
# Get indicator Data from function
# indicatorData = getIndicators(frequency = "daily", save = F, type = "all") 
#  indicatorDataWide = rbindlist(indicatorData[1])
#  indicatorDataNoPolicyWide = rbindlist(indicatorData[2])
#  policyCount = rbindlist(indicatorData[3])

## Alternatively: load indicator Data from file

indicatorDataWide = readRDS("Data/indicatorPolicyWeighted.RDA")
setnames(indicatorDataWide, 1, "Date")
indicatorDataNoPolicyWide = readRDS("Data/indicatorNoPolicyWeighted.RDA")
setnames(indicatorDataNoPolicyWide, 1, "Date")
#policyCount = readRDS("Data/policyCount.RDA")
#setnames

if(frequency == "daily"){
priceData = data.table(import("Data/wheatPriceDatastream.csv"))
metaTable = t(priceData[1:2, ])
names(priceData) = as.character(priceData[1,])
priceData = priceData[-c(1:2), ]
setnames(priceData, "Code", "Date")
WUSHRWO = data.table(read.csv("Data/addWheat.csv"))
WUSHRWO = WUSHRWO[, c(1,2), with = F]
names(WUSHRWO) = c("Date", "WUSHRWO")
WUSHRWO = WUSHRWO[-c(1:2), ]
priceData = merge(priceData, WUSHRWO, by = "Date", all.x = T)
setkey(priceData, Date)
priceData[, Date := as.Date(Date, format = "%d.%m.%Y")]
}

if(frequency == "weekly"){
priceData = data.table(import("Data/wheatWeekly.csv"))
priceData[, Date := as.Date(Date, format = "%d.%m.%Y")]
}

priceData = priceData[Date > "2001-12-31", ]
priceData = priceData[Date < "2016-01-01", ] 
wheatPriceList = readRDS("Data/wheatPriceList.RDA")
wheatPrices = wheatPriceList[wheatPriceList %in% colnames(priceData)]
restrictedPriceData = priceData[, c("Date", wheatPrices), with = F]
#priceData[, c("Date", wheatPriceList), with = F]
setkey(priceData, Date)
setkey(indicatorDataWide, Date)
firstIndicatorMerge = merge(restrictedPriceData, indicatorDataWide, by = "Date", all.x = T)
garchData = merge(firstIndicatorMerge, indicatorDataNoPolicyWide, by = "Date", all.x = T)

sdCols = names(garchData[,-"Date", with =F])
garchData[, c(sdCols)
          := lapply(.SD, as.numeric), .SDcols = c(sdCols)]

if(log == TRUE){
  sdCols = names(garchData[,-"Date", with =F])
  garchData[, c(sdCols)
            := lapply(.SD, log), .SDcols = c(sdCols)]
}
if(save == TRUE){
  saveRDS(garchData, paste("Data/garchData", if(log == T){"Logged"}, if(frequency == "weekly"){"Weekly"}, ".RDA", sep = ""))
}

if(output == TRUE){
garchData[]
}
}

