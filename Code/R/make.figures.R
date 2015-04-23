


Grant.Value = 2000000
r = seq(.00,.20,.001)
vals = Grant.Value / (1+r)^20

discount.example = ggplot()+geom_path(aes(x=r,y=vals),size=3.5)+
  scale_y_continuous('Grant Value at 20 Years',breaks=c(500000,1000000,1500000,2000000),expand=c(0,0),
                     labels=c('$0.5M','$1M','$1.5M','$2M'))+
  scale_x_continuous('Discount Rate (Yearly)',expand=c(0,0))+theme_tufte()+
  geom_vline(aes(xintercept=0.07),linetype=2)

print(discount.example)
ggsave('//Users/TScott/Google Drive/quinalt/discount.example.png',discount.example,
       width=4,height=3,units=c('in'))
?ggsave
?theme
install.packages('ggthemes')
library(ggthemes)
ggthemes::
?scale_y_continuous

library(lubridate)
install.packages('lubridate')
time.sequence = 
  
  seq(ymd('20020701') , months(1),6)
  

time.sequence <- ymd('20020701') + months(0:24)







val.mat = matrix(NA,ncol=(length(time.sequence)),nrow=length(r))
for (i in 1:nrow(val.mat))
{
  for (j in 1:ncol(val.mat))
  {
   val.mat[i,j] = Grant.Value / {(1+r[i])^(j-1)}
  }
}




rownames(val.mat) = r * 12 * 100
colnames(val.mat) = as.character(time.sequence)

temp = melt(val.mat) ; colnames(temp) = c('r','time','value')
library(ggplot2)
ggplot(temp) + geom_path(aes(x=time,colour=r,y=value,group=r))


  scale_colour_brewer(type='div',palette=3)

+ scale_colour_brewer



dim(val.mat)
library(reshape2)


melt(val.mat)




val.mat
rep(NA,)


for (i in 1:length(time.sequence))
{
(Grant.Value / ((1+r) ^ i))
}







for (i in 1:length(time.sequence))
{
  (2000000)/((1+r)^(time.sequence[i])) 
}



