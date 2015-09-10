rm(list=ls())
setwd('H:/quinalt')


load('midpoint.4.RData')
rm(list=ls())

temp = all.params.spdf@data

head(huc8.database)
head(temp)
library(zoo)

library(lubridate)
zoo::rollsum()
library(magrittr)
magrittr::
huc8.database$MONTH.NUM = match(huc8.database$MONTH,month.abb)
huc8.database$date = mdy(paste(huc8.database$MONTH,15,huc8.database$YEAR))
huc8.database$TOTAL.MONTH=match(huc8.database$date,seq(mdy('Jan 15 1990'),mdy('Dec 15, 2013'),by='months'))

temp = huc8.database
  
temp$oweb.yearmonth.tech.assess.monit = temp$oweb.yearmonth.monitoring+temp$oweb.yearmonth.assessment+
  temp$oweb.yearmonth.tech.assist
temp$dec.date = decimal_date(temp$date)

tt = 1:10
rollsum(tt,k=2,align='right',fill=0)

rsum.oweb.council.1yr = 
  
temp %>% group_by(HUC8) %>% arrange(dec.date) %>%
  summarise(rollsum(oweb.yearmonth.council.support,k=12,fill=0,align='right'))



  rollsum(temp$oweb.yearmonth.council.support,fill=0,align='right')

rsum.oweb.tech.1yr
rsum.oweb.rest.1yr
rsum.oweb.all.1yr
rsum.oweb.non.rest.1yr
rsum.oweb.ed.1yr

rsum.oweb.
lubridate::decimal_date
oweb.yearmonth.tech.assess.monit (dbl), dec.date (dbl),
rsum.owri.1yr (dbl), rsum.owri.3yr (dbl), rsum.owri.5yr (dbl), rsum.oweb.council.1yr (dbl),
rsum.oweb.council.3yr (dbl), rsum.oweb.council.5yr (dbl), rsum.oweb.tech.1yr (dbl), rsum.oweb.tech.3yr
(dbl), rsum.oweb.tech.5yr (dbl), rsum.oweb.rest.1yr (dbl), rsum.oweb.rest.3yr (dbl), rsum.oweb.rest.5yr
(dbl), rsum.oweb.ed.1yr (dbl), rsum.oweb.ed.3yr (dbl), rsum.oweb.ed.5yr (dbl), rsum.oweb.non.rest.1yr
(dbl), rsum.oweb.non.rest.3yr (dbl), rsum.oweb.non.rest.5yr (dbl), rsum.oweb.all.1yr (dbl),
rsum.oweb.all.3yr (dbl), rsum.oweb.all.5yr (dbl)