# This function stretches each policy string from its starting point to its end point assigning appropriate values for each period
# and taking increases, decreases and extensions into consideration

library(data.table)
library(zoo)

policyTimeSeries = function(cpl_idString, frequency ="monthly", policyData = policyData){
  
  
  policyStringData = subset(policyData, cpl_id == cpl_idString)
  
  policyStringData[, type_of_change_name :=  paste(toupper(substring(type_of_change_name,1,1)), 
                                                   substring(type_of_change_name, 2), sep = "")]
  
  
  extendedString = rbindlist(lapply(policyStringData[, policy_id], function(x){
    singleEntry = policyStringData[policy_id == x, ]
    singleEntry[, timeLine := start_date]
    startDate = singleEntry[, start_date]
    
    if(!is.na(singleEntry[, end_date])){
      endDate = singleEntry[, end_date]
    }else{
      if("Elimination" %in% policyStringData[, type_of_change_name]){
        endDate = policyStringData[ type_of_change_name == "Elimination", start_date]
        
        
      }else if(!is.na(policyStringData[length(cpl_id), end_date])){ 
        endDate = policyStringData[length(cpl_id), end_date]
        
      }else{
        endDate = as.Date("2015-12-31")
      }
      
    }
    if(frequency == "monthly"){
      startDate = as.Date(as.yearmon(startDate), 
                                           format ="%m %Y")
      endDate = as.Date(as.yearmon(endDate), 
                          format ="%m %Y")
      singleEntry[, timeLine := as.Date(as.yearmon(timeLine), 
                                        format ="%m %Y")]
    }
    
    
    
    timeSpan = seq(startDate, endDate, if(frequency == "monthly") {
                        by = "month"
                    }else if(frequency == "daily") {by = "day"
                   }else{
                     stop("Frequency must be either daily or monthly")
                     })
    timeLine = data.table(timeLine = timeSpan)
    
    extendedString = merge(timeLine, singleEntry, by = "timeLine", all.x=T)
    
    
    extendedString[]
  })
  
  )
  
  setkey(extendedString, timeLine, type_of_change_name)
  
  duplicated = extendedString[duplicated(timeLine),]
  
  setkey(duplicated, timeLine)
  toEliminate = extendedString[timeLine %in% duplicated[,timeLine] & is.na(type_of_change_name), ]
  
  
  extendedString = extendedString[!toEliminate]
  
#   extendedString[hs_version == "", hs_version := "NA"]
#   extendedString[hs_code == "", hs_code := NA] 
#   
  extendedString[, c("cpl_id", "country_name", "policymeasure_name", "commodity_id", "hs_code", "hs_version", "value") 
                 := lapply(.SD, na.locf, 
                           if(all(is.na(extendedString[, value]))){
                             na.rm = F
                           }else if(extendedString[1, hs_version == ""] | extendedString[1, hs_code == ""] ){
                             na.rm = F
                           
                           }else{
                             na.rm = T
                             }), .SDcols = c("cpl_id", "country_name", "policymeasure_name", 
                                                      "commodity_id", "hs_code", "hs_version", "value")]
  
  if(frequency == "monthly"){
    extendedString[, timeLine := as.Date(as.yearmon(timeLine), 
                                         format ="%m %Y")]
    
   extendedString = extendedString[!duplicated(timeLine),]
  }
  
  extendedString[]
  
}
