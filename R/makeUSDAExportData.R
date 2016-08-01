# USDA export data
psdExportData = data.table(read.csv("Data/psdWheatExportData.csv"))
worldWheatExports = psdExportData[Country == "World",]



## this script handels USDA export data to determine marketshares to be used as weights in the indicator

#worldWheatExports = worldWheatExports[, -c("cou"), with = F]
worldWheatExportsLong = melt(worldWheatExports, id = "Country", variable.name = "period", value.name = "worldExports")
worldWheatExportsLong = worldWheatExportsLong[, -"Country", with=F]
worldWheatExportsLong[, period := gsub("X", "", period)]
psdExportData[, iso3c := as.character(countrycode(Country, 
                                                  origin = "country.name", 
                                                  destination = "iso3c", 
                                                  warn = T))] 

exportData = psdExportData[, -"Country", with=F]
exportLongData = melt(exportData, id = "iso3c", variable.name = "period", value.name = "exports")
exportLongData[, period := gsub("X", "", period)]


exportShareData = merge(exportLongData, worldWheatExportsLong, by = "period")
exportShareData[, period := substring(period, 1,4)]
exportShareData[ ,exportShare := exports / worldExports]

saveRDS(exportShareData, "Data/usdaExportData.RDA")
