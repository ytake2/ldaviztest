library(ggplot2) #ggplotの呼び出し

library(RColorBrewer)

cols<-brewer.pal(12, "Set3")

effect$covariate.value<-as.numeric(effect$covariate.value)

effect%>%filter(covariate.value>1990)->effect2


library(viridis)
library(hrbrthemes)
names(effect2)[2]<-"Topic"

effect2$Topic<-as.factor(effect2$Topic)




g = ggplot(effect2, aes( x = covariate.value, 
                         y = estimate, fill = Topic))

g = g + geom_area(col="white")+
  ylim(0,1.10)+
  xlab("time")+
  ylab("Relative topic prevalence")+
 scale_fill_brewer(palette = "Set3")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot(g)


library(plotly)
library(htmlwidgets)

l<-plotly::ggplotly(g)

htmlwidgets::saveWidget(l, "stucked.html")
