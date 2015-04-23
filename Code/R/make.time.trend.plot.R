

rm(list=ls())
load('modpoint.sim8.RData')
library(ggplot2)
library(scales)

  
time.plot = ggplot()+geom_path(aes(x=mod3$summary.random$total.period$ID,
                       y=mod3$summary.random$total.period$mean))+
  geom_path(aes(x=mod3$summary.random$total.period$ID,
                y=mod3$summary.random$seasonal$mean),col='red',lty=2)+
  scale_y_continuous('Temporal and Seasonal Trend',limits=c(-0.3,.3))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.background=element_blank(),
        legend.position=c(220,-.2))+
  scale_x_continuous('Time Period',breaks= seq(1,228,24),labels=c(seq(1995,2013,2)),expand=c(0,0))

ggsave(filename='time.trend.plot.png',time.plot)
