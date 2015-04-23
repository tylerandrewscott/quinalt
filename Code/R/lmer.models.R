
rm(list = ls(all = TRUE))
setwd('/homes/tscott1/win/user/quinalt')
load('midpoint.6.v2.RData');rm(list=ls()[ls()!='huc8.database'])
load('midpoint.7.v2.RData')
require(xtable)
library(INLA)
library(rgdal);library(rgeos);library(maptools)

test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 

temp = params.spdf@data
library(lme4)
library(gamm4)
library(mgcv)
gam(counts ~ dependent_variable + ID + s(t, by = ID) , family="poisson")

gam(counts ~ dependent_variable + ID + s(t, by = ID) , family="poisson")


temp$uqfac = paste(temp$HUC8,temp$total.period)





exp(summary(lm.all.rev)$coef)


temp$funds.3yr.later = NA
for (i in 1:nrow(temp))
{
 t1 =  temp %>% filter(STATION==temp$STATION[i]) %>%
    filter(YEAR == as.numeric(temp$YEAR[i])+3) %>%
    filter(MONTH == temp$MONTH[i])
temp$funds.3yr.later[i] = t1$oweb.yearmonth.all[1]
}

lm.form.rev <-  (funds.3yr.later/100000) ~ l.owqi + Ag + Forst + Dev  + 
  elevation + seaDist  + (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.all.rev = lmer(lm.form.rev,data=temp)
BIC(lm.all.rev)
summary(lm.all.rev)$coef

temp$funds.3yr.later


temp$funds.3yr.later
temp$funds.3yr.later
temp$MONTH + 3


lm.form.all.1yr <-  l.owqi ~  Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.1yr + (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.all.1yr = lmer(lm.form.all.1yr,data=temp)
BIC(lm.all.1yr)
summary(lm.all.1yr)

lm.form.diff.1yr <-  l.owqi ~  0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  
  rsum.oweb.rest.1yr + rsum.oweb.council.1yr + rsum.oweb.tech.1yr + rsum.oweb.ed.1yr + 
  rsum.oweb.rest.1yr:rsum.oweb.council.1yr:rsum.oweb.tech.1yr:rsum.oweb.ed.1yr + 
  (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.diff.1yr = lmer(lm.form.diff.1yr,data=temp)


lm.form.all.3yr <-  l.owqi ~  Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.3yr + (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.all.3yr = lmer(lm.form.all.3yr,data=temp)
BIC(lm.all.3yr)
summary(lm.all.3yr)

lm.form.diff.3yr <-  l.owqi ~  0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  
  rsum.oweb.rest.3yr + rsum.oweb.council.3yr + rsum.oweb.tech.3yr + rsum.oweb.ed.3yr + 
  rsum.oweb.rest.3yr:rsum.oweb.council.3yr:rsum.oweb.tech.3yr:rsum.oweb.ed.3yr + 
  (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.diff.3yr = lmer(lm.form.diff.3yr,data=temp)

lm.form.all.5yr <-  l.owqi ~  Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.5yr + (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.all.5yr = lmer(lm.form.all.5yr,data=temp)
BIC(lm.all.5yr)
summary(lm.all.5yr)

lm.form.diff.5yr <-  l.owqi ~  0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  
  rsum.oweb.rest.5yr + rsum.oweb.council.5yr + rsum.oweb.tech.5yr + rsum.oweb.ed.5yr + 
  rsum.oweb.rest.5yr:rsum.oweb.council.5yr:rsum.oweb.tech.5yr:rsum.oweb.ed.5yr + 
  (1|STATION) + (1|HUC8) + (1|total.period) + (1|SEASON)
lm.diff.5yr = lmer(lm.form.diff.5yr,data=temp)

summary(lm.form.diff.5yr)

library(stargazer)

stargazer(lm.all.1yr,lm.all.3yr,lm.all.5yr,out='lmmodelsallgrants.tex',title = "Linear models for all grants combined")
stargazer(lm.diff.1yr,lm.diff.3yr,lm.diff.5yr,out='lmmodelsdiffgrants.tex', title = 'Linear models comparing different grant types')





