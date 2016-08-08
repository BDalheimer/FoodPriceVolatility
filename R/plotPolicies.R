#' plot
#' 

plotPolicies = function(x){

library(ggplot2)
library(RColorBrewer)


policyData = readRDS("Data/policyData.RDA")

plotData =policyData[, .N, by = c("country_name", "policymeasure_name")]

plotData[, country_name:= as.character(country_name)]
setkey(plotData, country_name, policymeasure_name)

orderData = data.table(aggregate(data = plotData, N ~ country_name, sum))

orderData = orderData[order(N, decreasing = T),]
orderFactors = as.factor(orderData[, country_name])
plotData[, country_name := factor(country_name, levels = orderFactors)]
plotData = plotData[order(country_name), ]

ggplot(plotData, aes(x = country_name, y = N, fill = policymeasure_name)) + scale_fill_brewer(palette = "Set1", guide = guide_legend(title = "Policy Type")) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black")) +
  geom_bar(stat = "identity") +
    ylab("Number of implemented policies") + xlab("") +
    ggtitle("") + 
    theme(plot.title = element_text(lineheight=4, face="bold"), plot.background = element_blank(),panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,panel.border = element_blank(),
          axis.text=element_text(size=5),
          axis.title=element_text(size=5,face="bold"),
          legend.title=element_text(size=5)) +
  scale_y_continuous(expand = c(0,0001)) +
  scale_x_discrete(expand = c(0,01)) +
  theme(axis.line = element_line(color = 'black'), legend.text=element_text(size=5), legend.position=c(0.9, 0.9))
  
}