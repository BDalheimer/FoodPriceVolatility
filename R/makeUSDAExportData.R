# USDA export data
psdExportData = data.table(read.csv("Data/psdWheatExportData.csv"))
worldWheatExports = psdExportData[Country == "World",]



## this script handels USDA export data to determine marketshares to be used as weights in the indicator

#worldWheatExports = worldWheatExports[, -c("cou"), with = F]
worldWheatExportsLong = melt(worldWheatExports, id = "Country", variable.name = "period", value.name = "worldExports")
worldWheatExportsLong = worldWheatExportsLong[, -"Country", with=F]
worldWheatExportsLong[, period := gsub("X", "", period)]
worldWheatExportsLong[, period := substring(period, 1,4)]
psdExportData[, iso3c := as.character(countrycode(Country, 
                                                  origin = "country.name", 
                                                  destination = "iso3c", 
                                                  warn = T))] 

exportData = psdExportData[, -"Country", with=F]
exportLongData = melt(exportData, id = "iso3c", variable.name = "period", value.name = "exports")
exportLongData[, period := gsub("X", "", period)]
exportLongData[, period := substring(period, 1,4)]

if(aggregateRussia == T){
  russiaAggregate = data.table(aggregate(data =exportLongData[iso3c %in% c("KAZ", "RUS", "UKR", "KGZ"), ], exports ~ period, sum))
  
  russiaAggregate[, iso3c := "RUK"]
  
  exportLongData = rbind(exportLongData[!iso3c %in% c("KAZ", "RUS", "UKR", "KGZ"), ], russiaAggregate)
}

exportShareData = merge(exportLongData, worldWheatExportsLong, by = "period")




exportShareData[ ,exportShare := exports / worldExports]

if(save == T){
saveRDS(exportShareData, paste("Data/usdaExportData", if(aggregateRussia == T){ "RusAgg" }, ".RDA", sep =""))
}