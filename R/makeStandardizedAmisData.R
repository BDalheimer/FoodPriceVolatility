# standardize  AMIS policy data
library(data.table)

#load raw data
amisWheatPolicyData = data.table(read.csv("Data/amisPolicyData.csv"))

# keep variables of interest only
wheatData = amisWheatPolicyData[, .(policy_id, cpl_id, country_name, policymeasure_name, 
                                    hs_version, hs_code, commodity_id, start_date, end_date, type_of_change_name, units, value, value_text)]

# convert to proper time format
wheatData[,start_date := as.Date(paste(strftime(format(strptime(start_date, "%d/%m/%Y"),usetz = F), "%Y-%m"), "-01", sep = ""))]
wheatData[,end_date := as.Date(paste(strftime(format(strptime(end_date, "%d/%m/%Y"),usetz = F), "%Y-%m"), "-01", sep = ""))]

# load pre-defined converison table to onvert values
conversionTable = readRDS("Data/amisValuesConversionTable.RDA")                      
standardizedValueData = merge(wheatData, conversionTable, by ="value_text", all.x=T)
standardizedValueData[, value := ifelse(is.na(value), newValue, value)]

standardizedPolicyData = standardizedValueData[, .(policy_id, cpl_id, country_name, policymeasure_name, commodity_id, type_of_change_name, start_date, end_date, value)] 

setkey(standardizedPolicyData, country_name, start_date,commodity_id)

saveRDS(standardizedPolicyData, "Data/standardizedAmisData.RDA")

