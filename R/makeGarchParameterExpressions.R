order = c("mu", "ar1", "ar2", "ar3", "ar4", "ar5", "ma1", "ma2", "ma3", "ma4", "ma5", "mxreg1", "mxreg2" ,"mxreg3", 
          "omega", "alpha1", "beta1", "vxreg1", "vxreg2", "vxreg3")

garchParameterExpressions = data.table(
  
  Parameter = factor( as.character(order), levels=order),
  expression = c("$\\mu$", "AR(1)", "AR(2)", "AR(3)", "AR(4)","AR(5)", "MA(1)", "MA(2)", "MA(3)", "MA(4)", "MA(5)", 
                 "Export Ban (mean eq.)", "Export Quota (mean eq.)", "Export Tax (mean eq)", "$\\omega$", "$\\alpha$",
                 "$\\beta$", "Export Bans", "Export Quotas", "Export Taxes"))

setkey(garchParameterExpressions, Parameter)

saveRDS(garchParameterExpressions, "Data/garchParameterExpressions.RDA")
