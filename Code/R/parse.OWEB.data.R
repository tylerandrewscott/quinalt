#OWEB Data


setwd('H:/quinalt')
rm(list=ls())
load('midpoint.3.RData')

require(plyr)
require(dplyr)
require(sp)
require(rgdal)
require(maptools)
require(proj4)
require(ggplot2)
setwd('H:/quinalt/OWEB_grants/')

#####load detailed grant data###3
dat<-read.csv('H:/quinalt/oweb_download_grant.csv')

dat$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(dat$Project.Summary,dat$X,sep=' '),
                                                                      dat$X.1,sep=''),dat$X.2,sep=''),dat$X.3, sep=''),dat$X.4, sep =''),
                                              dat$X.5, sep = ''), dat$X.6, sep = ''), dat$X.7, sep = ''),
                            dat$X.8, sep = ''), dat$X.9, sep = '')

levels(dat$Region) <- c('NW','SW','WIL','CENT','EAST','MID','SW')

dat$Award.Date<-as.Date(dat$Award.Date,format = '%m/%d/%y')
dat$Project.Start.Date<-as.Date(dat$Project.Start.Date,format = '%m/%d/%y')
dat$Project.End.Date<-as.Date(dat$Project.End.Date,format = '%m/%d/%y')

dat$Grantee1<-NA
dat$Grantee2<-NA



#########load spatial grant data

oweb.grants = readOGR(dsn="H:/quinalt/OWEB_grants", layer="OWEB_Grants_8-4-2014")
oweb.grants@data$id = rownames(oweb.grants@data)

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

temp.which.huc8 = over(spTransform(oweb.grants,CRSobj=CRS(proj4string(oregon.huc8))),oregon.huc8)
oweb.grants$HUC8 = temp.which.huc8$HUC8

oweb.grants@data = data.frame(oweb.grants@data,dat[match(oweb.grants$gr_project,dat$Project.ID),])

grant.temp = oweb.grants@data
param.temp = all.params.spdf@data


sum(oweb.dat$Project.Number[oweb.dat$Project.Type=='Restoration'] %in% proj.info$drvdOwebNum)



levels(grant.temp$project_ty)[8] = 'Education.Outreach'
levels(grant.temp$project_ty)[5] = 'Education.Outreach'

grant.temp = grant.temp[grant.temp$Grantee.Type=='Watershed Council',]
grant.temp = grant.temp[grant.temp$project_ty!='Research',]

require(lubridate);require(reshape2)
grant.temp$Project.Start.Date = ymd(grant.temp$Project.Start.Date)
grant.temp$Project.End.Date = ymd(grant.temp$Project.End.Date)
grant.temp$Start.Year = year(grant.temp$Project.Start.Date)
grant.temp$End.Year = year(grant.temp$Project.End.Date)
grant.temp$End.Month = month(grant.temp$Project.End.Date)
grant.temp$Start.Month = month(grant.temp$Project.Start.Date)
grant.temp$Award.Year = year(grant.temp$Award.Date)
grant.temp = filter(grant.temp,!is.na(End.Year))

#####MAKE TABLE SUMMARIZING GRANTS
dat2 = grant.temp[grant.temp$Project.Type%in%levels(grant.temp$Project.Type)[c(2,3,5,8,9,12,14)],]
dat2$Project.Type = as.factor(as.character(dat2$Project.Type))
dat2 = dat2[dat2$Start.Year<=2013,]
monthlength=(dat2$Project.Start.Date %--% dat2$Project.End.Date) 
dat2$projlengthmonths = as.period(monthlength)%/%months(1)
levels(dat2$Project.Type) = c('Monitoring/Assessment/Tech. Assistance','Council Support',
                              'Education/Outreach','Monitoring/Assessment/Tech. Assistance',
                              'Education/Outreach','Restoration','Monitoring/Assessment/Tech. Assistance')
grant.table = join(tally(group_by(dat2,Project.Type)),
                   dat2 %>% group_by(Project.Type) %>% summarise(round(mean(Project.Amount),2)))
grant.table$Project.Type = as.character(grant.table$Project.Type)
tem = dat2 %>% group_by(Project.Type) %>% summarise(round(mean(projlengthmonths),2))
grant.table = join(grant.table,tem)

colnames(grant.table) = c('Project Type','N','Avg. Amount ($)','Avg. Length (months)')
grant.table[5,] = c('All Grants',sum(grant.table$N),round(mean(dat2$Project.Amount),2),round(mean(dat2$projlengthmonths),2))

rownames(grant.table) = grant.table[,1]
grant.table = grant.table[,-1]

library(lubridate)
library(stargazer)
setwd('H:/quinalt')
stargazer(grant.table,summary=F ,out='granttable.tex',type='latex',title='Grant summary statistics',
          table.placement='!hbtp',label='table:grantsummary')
###########

base.start.month = grant.temp$Start.Year-1995
base.end.month = grant.temp$End.Year-1995
base.start.month = base.start.month * 12 + grant.temp$Start.Month
base.end.month = base.end.month * 12 + grant.temp$End.Month

#fix grant that has mislabeled end year (should be + 1 year)
grant.temp$End.Year[base.start.month - base.end.month >=0] = 2004
base.end.month[base.start.month - base.end.month >= 0] = base.end.month[base.start.month - base.end.month >= 0] + 12


grant.amounts = NULL
grant.month.year = NULL
grant.huc8 = NULL
grant.type = NULL
tt = grant.temp$Project.Amount/(base.end.month-base.start.month+1)
for (i in 1:length(tt))
{
  grant.amounts = append(grant.amounts,rep(tt[i],(base.end.month-base.start.month+1)[i]))
  grant.month.year = append(grant.month.year,seq(base.start.month[i],base.end.month[i],1))
  grant.huc8 = append(grant.huc8,rep(as.character(grant.temp$HUC8[i]),(base.end.month-base.start.month+1)[i]))
  grant.type = append(grant.type,rep(as.character(grant.temp$project_ty[i]),(base.end.month-base.start.month+1)[i]))
}

grant.monthly = data.frame(grant.amounts,grant.month.year,grant.huc8,grant.type)

jan1595 = mdy('Jan 15 1995')
date.seq = jan1595 %m+% months(0:max(grant.monthly$grant.month.year))
date.ref = data.frame(date.seq,((date.seq %--% date.seq[1]) %/% months(1))*-1)
colnames(date.ref) = c('date','grant.month.year')
grant.monthly = join(grant.monthly,date.ref,type='left')



sum.funds1 = ddply(grant.monthly,.(grant.huc8,grant.month.year,grant.type),summarise,sum(grant.amounts))
sum.funds2 = ddply(grant.monthly,.(grant.huc8,grant.month.year),summarise,sum(grant.amounts))
sum.funds3 = ddply(filter(grant.monthly,grant.type!='Restoration'),
      .(grant.huc8,grant.month.year),summarise,sum(grant.amounts))
sum.funds2$grant.type ='All'
sum.funds3$grant.type = 'Non.Restoration'


sum.funds = join_all(list(sum.funds1,sum.funds2,sum.funds3),type='full')
temp = all.params.spdf@data

#need to make this merge work
colnames(sum.funds) = c('HUC8','TOTAL.MONTH','Grant.Type','oweb.yearmonth.fund')

wide.sum.funds = dcast(sum.funds,formula=HUC8+TOTAL.MONTH~Grant.Type,fill=0)


colnames(wide.sum.funds) = c('HUC8','TOTAL.MONTH','oweb.yearmonth.all',
                             'oweb.yearmonth.assessment','oweb.yearmonth.council.support',
                             'oweb.yearmonth.education.outreach',
                             'oweb.yearmonth.monitoring','oweb.yearmonth.non.restoration',
                             'oweb.yearmonth.restoration',
                         'oweb.yearmonth.tech.assist')

wide.sum.funds$date = date.ref$date[match(wide.sum.funds$TOTAL.MONTH,date.ref$grant.month.year)]
wide.sum.funds$YEAR = year(wide.sum.funds$date)
wide.sum.funds$MONTH = month(wide.sum.funds$date,label=TRUE)
wide.sum.funds$MONTH.NUM = month(wide.sum.funds$date)
intersect(names(huc8.database),names(wide.sum.funds))
huc8.database = dplyr::left_join(huc8.database,wide.sum.funds)


huc8.database$owri.yearmonth.spend[is.na(huc8.database$owri.yearmonth.spend)] = 0
huc8.database$oweb.yearmonth.all[is.na(huc8.database$oweb.yearmonth.all)] = 0
huc8.database$oweb.yearmonth.assessment[is.na(huc8.database$oweb.yearmonth.assessment)] = 0
huc8.database$oweb.yearmonth.council.support[is.na(huc8.database$oweb.yearmonth.council.support)] = 0
huc8.database$oweb.yearmonth.education.outreach[is.na(huc8.database$oweb.yearmonth.education.outreach)] = 0
huc8.database$oweb.yearmonth.monitoring[is.na(huc8.database$oweb.yearmonth.monitoring)] = 0
huc8.database$oweb.yearmonth.non.restoration[is.na(huc8.database$oweb.yearmonth.non.restoration)] = 0
huc8.database$oweb.yearmonth.restoration[is.na(huc8.database$oweb.yearmonth.restoration)] = 0
huc8.database$oweb.yearmonth.tech.assist[is.na(huc8.database$oweb.yearmonth.tech.assist)] = 0

rm(list=ls()[intersect(grep('huc8.database',ls(),invert=TRUE),grep('all.params.spdf',ls(),invert=TRUE))])

setwd('H:/quinalt')
save.image('midpoint.4.RData')


