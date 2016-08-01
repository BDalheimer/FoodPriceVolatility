library(data.table)

policyDataAll = data.table(read.csv("Data/oecdPolicyData.csv"))

## narrow down to wheat policies (likely to be extended to all grain affecting policies)
wheatDataAll = policyDataAll[grepl("wheat", product_group, ignore.case = T) == T,]
wheatData = wheatDataAll[,.(cou, tom, tomx, HS6, HS8, dir, val, val_shared, val_unit, year,
                            date_reference, dati, end)] 

nonAmisData = wheatData[cou %in% c("EGY", "KAZ", "KGZ", "MDA", "PAK"),]


nonAmisData[, c("HS6", "HS8", "year","val", "val_shared") := lapply(.SD, as.character), .SDcols = c("HS6", "HS8", "year","val", "val_shared")]
nonAmisData[, val_shared := gsub(val_shared, pattern = ",", replacement = "")]
nonAmisData[, c("val", "val_shared") := lapply(.SD, as.numeric), .SDcols = c("val", "val_shared")]
nonAmisData[, val := ifelse(is.na(val) & !is.na(val_shared), val_shared, val)]

policyGroups = readRDS("Data/policyGroups.RDA")

nonAmisData = merge(nonAmisData, policyGroups, all.x = T, by ="tom")
nonAmisData[, tom := policyGroup]
nonAmisData = nonAmisData[, HS8 := rep("HS2007")]

nonAmisData[, c("date_reference", "dati", "end") := lapply(.SD, function(x){as.Date(format(strptime(as.character(x), "%d-%m-%Y"),usetz = F)) }), 
            .SDcols = c("date_reference", "dati", "end")]


nonAmisData[, dati := as.Date(ifelse(is.na(dati) & !is.na(date_reference), date_reference, dati))]
nonAmisData = nonAmisData[!tom == "other"]


countryTable = data.table(cou = c("KGZ", "PAK", "KAZ", "MDA"), name = c("Kyrgyzstan", "Pakistan",  "Kazakhstan", "Moldova"))

nonAmisData = merge(nonAmisData, countryTable, all.x = T, by = "cou")
nonAmisData = nonAmisData[, .(name, tom, HS6, HS8, dir, val, dati, end)]
names(nonAmisData) = c("country_name", "policymeasure_name", "hs_code", "hs_version", "type_of_change_name", "value", "start_date", "end_date")

nonAmisData[, policy_id := rasters <- sprintf("OECD%s",seq(1:length(country_name)))]


write.csv(nonAmisData, "Data/nonAmisData.csv")
nonAmisData = data.table(read.csv("Data/nonAmisDataManipulated.csv"))
nonAmisData = merge(nonAmisData, commodityList, by = c("hs_version", "hs_code"), all.x = T)
commodityList = readRDS("Data/commodityList.RDA")

nonAmisData[, c("start_date", "end_date") := lapply(.SD, function(x){as.Date(format(strptime(as.character(x), "%Y-%m-%d"),usetz = F)) }), 
            .SDcols = c("start_date", "end_date")]

# additional policy information taken from Sharma(2011)

sharma = data.table("hs_version" = rep("NA", 2),
                      "hs_code" = rep("NA", 2),
                      "country_name" = c("Ethiopia", "Tanzania"),
                      "policymeasure_name" = c("Export prohibition", "Export prohibition"),
                      "type_of_change_name" = c("Introduction", "Introduction"),
                      "value" = c(NA, NA),
                      "start_date" = as.Date(c("2008-05-01", "2008-01-01"), format = "%Y-%m-%d"),
                      "end_date" = as.Date(c("2015-12-31", "2010-04-01"), format = "%Y-%m-%d"),
                      "policy_id" = c("SHARMA1/USDA", "SHARMA2"),
                      "cpl_id" = c(6001, 6002),
                      "commodity_id" = c(1, 1))

nonAmisData = rbind(nonAmisData, sharma)
saveRDS(nonAmisData, "nonAmisData.RDA")
