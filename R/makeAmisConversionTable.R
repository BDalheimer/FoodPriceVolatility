# create a conversion table for amis policy values by vreating averages or simply taking over numbers from text fields (hardcoded)

amisWheatPolicyData = data.table(read.csv("Data/amisPolicyDataAllGrains.csv"))

conversionTable = data.table(value_text = unique(wheatData[,value_text]),
                             newValue = c("NA", "NA", 10, 40, 21.5, 33, 33, 20.8, 30.8, 32.5, 30.8, 20.08, "NA", 9, 12, 15)) 

saveRDS(conversionTable, "Data/amisValuesConversionTable.RDA")
