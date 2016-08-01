## function which standardizes policy Groups values. It converts a character/fator vector into a numerical one of same length

standardizePolicyValues = function(vector){
  
  vector = as.character(vector)
  id = seq(1:length(vector))
  
  definedData = data.table(id = id, val = vector)
  
  targetLevels = definedData[nchar(val) > 14,]
  
  map = data.table(id = targetLevels[, id], val = targetLevels[, val],
                   newVal = c("33", "33", "32", "10", "40", "40", "9"))
  saveRDS(map, "Data/metaValueMap.RDA")
  
  combinedData = merge(definedData, map, by = c("id", "val"), all.x = T)
  combinedData[, val := ifelse(nchar(val) > 14, newVal, val)]
  out = combinedData[, val]
  out
}