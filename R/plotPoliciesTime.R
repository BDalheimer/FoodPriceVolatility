plotPoliciesTime = function(frequency = "daily"){
  
  library(ggplot2)
  library(RColorBrewer)
  
  if(frequency == "daily"){
  plotDataWide = readRDS("Data/indicatorPolicyWeightedNotLogged.RDA")
  }else if(frequency == "monthly"){
  plotDataWide = readRDS("Data/indicatorPolicyWeightedMonthlyNotLogged.RDA")
  }
  
  plotData = melt(plotDataWide, id.vars = "timeLine", measure.vars = c("Export prohibition", "Export quota", "Export tax"), variable.name = "policyType", value.name = "indicatorLevel")
  ggplot(data=plotData,
         aes(x=timeLine, y=indicatorLevel, colour=policyType)) +
     geom_line(size = 1) +
    scale_colour_brewer(palette = "Set1", guide = guide_legend(title = "Policy Type")) +
    theme_bw()+
    ylab("Indicator Value x 100") + xlab("") +
    theme(axis.line = element_line(colour = "black"), legend.text=element_text(size=4), legend.position=c(0.9, 0.9),
          axis.text=element_text(size=5),
          axis.title=element_text(size=5,face="bold"),
          legend.title=element_text(size=5)) + 
    theme(plot.title = element_text(lineheight=4, face="bold"), plot.background = element_blank(),panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,panel.border = element_blank())
    
}