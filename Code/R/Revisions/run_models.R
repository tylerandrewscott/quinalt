rm(list=ls())

setwd('/homes/tscott1/win/user/quinalt/')



require(foreign)
require(plyr)
require(dplyr)
require(rgdal)
require(sp)
require(rgeos)
require(maptools)
require(ggplot2)
require(reshape2)
library(RcppRoll)
library(devtools)
library(RCurl)
library(gdata)
require(proj4)
library(lubridate)
require(RODBC)
require(gridExtra)
require(lattice)
require(splancs)
require(fields)
library(raster)
library(shapefiles)
library(raster)
library(rasterVis)  # raster visualisation
library(rWBclimate)
library(mail)
library(stargazer)
library(texreg)
library(INLA)
require(xtable)
library(maptools)



load("temp_workspace_noprecip.RData")

temp = (oweb.all %>% filter(which.group == 'WC' |which.group=='SWCD') %>% group_by(Project.Type,which.group) %>%
  summarise_each(funs(nobs,mean),Project.Amount,abs.length))[,-3]
temp[,1] = ifelse(duplicated(data.frame(temp)[,1]),'',data.frame(temp)[,1])
names(temp) = c('Project','Grantee','N','Average cost','Average length (months)')
stargazer(data.frame(temp),summary=F,rownames = F,no.space = T,digits=0,type='latex',
          out='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/grantsummarytable.tex',
          title = 'OWEB grant summary statistics (1997-2013)',
          label='table:grantsummary')


#test = readOGR(dsn='government_units','state_nrcs_a_or')

INLA::inla.setOption(num.threads=16) 

mod.data = all.params.spdf@data

mod.data$seasonal = mod.data$Abs.Month
mod.data$total.period = mod.data$Abs.Month
mod.data$sq.owqi = ((as.numeric(as.character(mod.data$owqi)))^2)
mod.data$l.owqi = log(as.numeric(as.character(mod.data$owqi)))
mod.data = filter(mod.data,YEAR>=1992)
mod.data$HUC8 = as.character(mod.data$HUC8)

covars = mod.data[,c('elevation','seaDist','HUC8','total.period','YEAR',
                    'ag.huc8','dev.huc8','wet.huc8','forst.huc8','l.owqi',
                    'county.pop.growthrate','owqi',
                 'seasonal','Ag','Dev','Wetl','Forst',
                 grep('OWEB',names(mod.data),value=T))]

k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k

covars[is.na(covars)] = 0

covars$elev100m = covars$elevation/100
covars$seaDist10km = covars$seaDist/10

covars = mutate(covars,OWEB_Grant_All_12_WC = OWEB_Grant_Restoration_12_WC+
                  OWEB_Grant_Capacity_12_WC +
                  OWEB_Grant_Tech_12_WC +
                  OWEB_Grant_Outreach_12_WC,
                OWEB_Grant_All_36_WC = OWEB_Grant_Restoration_36_WC+
                  OWEB_Grant_Capacity_36_WC +
                  OWEB_Grant_Tech_36_WC +
                  OWEB_Grant_Outreach_36_WC,
                OWEB_Grant_All_60_WC = OWEB_Grant_Restoration_60_WC+
                  OWEB_Grant_Capacity_60_WC +
                  OWEB_Grant_Tech_60_WC +
                  OWEB_Grant_Outreach_60_WC,
                OWEB_Grant_All_12_SWCD = OWEB_Grant_Restoration_12_SWCD+
                  OWEB_Grant_Capacity_12_SWCD +
                  OWEB_Grant_Tech_12_SWCD +
                  OWEB_Grant_Outreach_12_SWCD,
                OWEB_Grant_All_36_SWCD = OWEB_Grant_Restoration_36_SWCD+
                  OWEB_Grant_Capacity_36_SWCD +
                  OWEB_Grant_Tech_36_SWCD +
                  OWEB_Grant_Outreach_36_SWCD,
                OWEB_Grant_All_60_SWCD = OWEB_Grant_Restoration_60_SWCD+
                  OWEB_Grant_Capacity_60_SWCD +
                  OWEB_Grant_Tech_60_SWCD +
                  OWEB_Grant_Outreach_60_SWCD
)


covars$OWEB_Grant_Capacity_PriorTo12 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_12_WC
covars$OWEB_Grant_Capacity_PriorTo36 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_36_WC
covars$OWEB_Grant_Capacity_PriorTo60 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_60_WC


#or.bond = inla.nonconvex.hull(cbind(mod.data$DECIMAL_LONG,mod.data$DECIMAL_LAT),2,2)
(mesh.a <- inla.mesh.2d(cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),max.edge=c(5, 40),cut=.05))$n

spde.a <- inla.spde2.matern(mesh.a) 


# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=covars$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

form_nonspatial <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)

form_spatial <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

mod.base.nonspatial <- inla(form_nonspatial, 
                       data=data.frame(y=covars$l.owqi, covars,b0=1), 
                       control.predictor=list(compute=TRUE),
                       control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)


mod.base.spatial<- inla(form_spatial, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

tempcoef = data.frame(exp(mod.base.nonspatial$summary.fixed[-1,c(1,3,5)]))
rownames(tempcoef) = 
  c(
    "\\%  Agric. (1000m buffer)",
    '\\%  Forest (1000m buffer)',
    '\\%  Devel. (1000m buffer)',
    '\\%  Devel. in HUC8',
    "\\%  Agric. in HUC8",
    '\\%  Forest in HUC8',
    'Elevation (10m)',
    'Dist. from coast (10km)',
    'Non-OWEB Restoration')
tempcoef.justcoef = data.frame(tempcoef[,'mean'])


tempcoef2 = data.frame(exp(mod.base.spatial$summary.fixed[-1,c(1,3,5)]))
rownames(tempcoef2) = 
  c(
    "\\%  Agric. (1000m buffer)",
    '\\%  Forest (1000m buffer)',
    '\\%  Devel. (1000m buffer)',
    '\\%  Devel. in HUC8',
    "\\%  Agric. in HUC8",
    '\\%  Forest in HUC8',
    'Elevation (10m)',
    'Dist. from coast (10km)',
    'Total Non-OWEB Restoration')

tempcoef2.justcoef = data.frame(tempcoef2[,'mean'])

rowname.vector = c(
  "\\%  Agric. (1000m buffer)",
  '\\%  Forest (1000m buffer)',
  '\\%  Devel. (1000m buffer)',
  '\\%  Devel. in HUC8',
  "\\%  Agric. in HUC8",
  '\\%  Forest in HUC8',
  'Elevation (10m)',
  'Dist. from coast (10km)',
  'Total Non-OWEB Restoration')

rownames(tempcoef.justcoef) = rowname.vector
rownames(tempcoef2.justcoef) = rowname.vector

library(lme4)
library(texreg)


modbase.nonspatial.present = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoef[,1],
  ci.low = tempcoef[,2],
  ci.up = tempcoef[,3],
gof.names = 'DIC',
gof = mod.base.nonspatial$dic$dic)

modbase.spatial.present = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoef2[,1],
  ci.low = tempcoef2[,2],
  ci.up = tempcoef2[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial$dic$dic)


texreg(l = list(modbase.nonspatial.present,modbase.spatial.present),
       stars=numeric(0),ci.test = 1,digits = 4,
       caption = "Baseline model w/ and w/out spatial correlation", caption.above = TRUE, 
       custom.model.names = c('w/ spatial correlation','w/out spatial correlation'),
       label = c('table:basemods'),
       custom.note = "^* 1 outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/basemods.tex')


form_all_12m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_12_WC + 
  OWEB_Grant_All_12_SWCD + 
  OWEB_Grant_All_12_WC:OWEB_Grant_All_12_SWCD +
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.all.12m <- inla(form_all_12m, family='gaussian', data=inla.stack.data(stk.1),
                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                        #  control.inla=list(strategy='laplace'), 
                        control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)


form_all_36m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_36_WC + 
  OWEB_Grant_All_36_SWCD + 
  OWEB_Grant_All_36_WC:OWEB_Grant_All_36_SWCD +
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.all.36m <- inla(form_all_36m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

form_all_60m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_60_WC + 
  OWEB_Grant_All_60_SWCD + 
  OWEB_Grant_All_60_WC:OWEB_Grant_All_60_SWCD +
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.all.60m <- inla(form_all_60m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)


tempcoef1 = data.frame(exp(mod.all.12m$summary.fixed[-1,c(1,3,5)]))
tempcoef2 = data.frame(exp(mod.all.36m$summary.fixed[-1,c(1,3,5)]))
tempcoef3 = data.frame(exp(mod.all.60m$summary.fixed[-1,c(1,3,5)]))

rownames(tempcoef1) = rownames(tempcoef2) = rownames(tempcoef3) =  
  c(
    "Agric. (100m buffer)",
    'Forest (100m buffer)',
    'Devel. (100m buffer)',
    'Devel. in HUC8',
    "Agric. in HUC8",
    'Forest in HUC8',
    'Elevation (10m)',
    'Dist. from coast (10km)',
    'Total Non-OWEB Restoration ($100k)',
    'OWEB grants to WC ($100k)',
    'OWEB grants SWCD ($100k)',
    'OWEB grants to WC * OWEB grants to SWCD ($100k)')


mod.all.12m = texreg::createTexreg(
  coef.names = rownames(tempcoef1),
  coef = tempcoef1[,1],
  ci.low = tempcoef1[,2],
  ci.up = tempcoef1[,3],
  gof.names = 'DIC',
  gof = mod.all.12m$dic$dic)

mod.all.36m = texreg::createTexreg(
  coef.names = rownames(tempcoef2),
  coef = tempcoef2[,1],
  ci.low = tempcoef2[,2],
  ci.up = tempcoef2[,3],
  gof.names = 'DIC',
  gof = mod.all.36m$dic$dic)

mod.all.60m = texreg::createTexreg(
  coef.names = rownames(tempcoef3),
  coef = tempcoef3[,1],
  ci.low = tempcoef3[,2],
  ci.up = tempcoef3[,3],
  gof.names = 'DIC',
  gof = mod.all.60m$dic$dic)


texreg(l = list(mod.all.12m,mod.all.36m,mod.all.60m),
       stars=numeric(0),ci.test = 1,digits = 4,
       custom.model.names = c('Past 12 months','Past 36 months','Past 60 months'),
       caption.above=T,omit.coef = "(100m)|(HUC8)|(10m)|(10km)|Total",
       label = c('table:allfunding'),
       custom.note = "^* 1 outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/allfunding.tex')

##########Project type funding###############
form_ind_12m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_12_WC + 
  OWEB_Grant_Tech_12_WC + 
  OWEB_Grant_Capacity_12_WC + 
  OWEB_Grant_Restoration_12_WC + 
  OWEB_Grant_Outreach_12_SWCD + 
  OWEB_Grant_Tech_12_SWCD + 
  OWEB_Grant_Capacity_12_SWCD + 
  OWEB_Grant_Restoration_12_SWCD + 
  OWEB_Grant_Outreach_12_WC:OWEB_Grant_Tech_12_WC:OWEB_Grant_Capacity_12_WC:OWEB_Grant_Restoration_12_WC + 
  OWEB_Grant_Outreach_12_SWCD:OWEB_Grant_Tech_12_SWCD:OWEB_Grant_Capacity_12_SWCD:OWEB_Grant_Restoration_12_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

form_ind_36m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_36_WC + 
  OWEB_Grant_Tech_36_WC + 
  OWEB_Grant_Capacity_36_WC + 
  OWEB_Grant_Restoration_36_WC + 
  OWEB_Grant_Outreach_36_SWCD + 
  OWEB_Grant_Tech_36_SWCD + 
  OWEB_Grant_Capacity_36_SWCD + 
  OWEB_Grant_Restoration_36_SWCD + 
  OWEB_Grant_Outreach_36_WC:OWEB_Grant_Tech_36_WC:OWEB_Grant_Capacity_36_WC:OWEB_Grant_Restoration_36_WC + 
  OWEB_Grant_Outreach_36_SWCD:OWEB_Grant_Tech_36_SWCD:OWEB_Grant_Capacity_36_SWCD:OWEB_Grant_Restoration_36_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=36)+ 
  f(s, model=spde.a,replicate=s.repl)

form_ind_60m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_60_WC + 
  OWEB_Grant_Tech_60_WC + 
  OWEB_Grant_Capacity_60_WC + 
  OWEB_Grant_Restoration_60_WC + 
  OWEB_Grant_Outreach_60_SWCD + 
  OWEB_Grant_Tech_60_SWCD + 
  OWEB_Grant_Capacity_60_SWCD + 
  OWEB_Grant_Restoration_60_SWCD + 
  OWEB_Grant_Outreach_60_WC:OWEB_Grant_Tech_60_WC:OWEB_Grant_Capacity_60_WC:OWEB_Grant_Restoration_60_WC + 
  OWEB_Grant_Outreach_60_SWCD:OWEB_Grant_Tech_60_SWCD:OWEB_Grant_Capacity_60_SWCD:OWEB_Grant_Restoration_60_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=60)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.ind.12m <- inla(form_ind_12m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

mod.ind.36m <- inla(form_ind_36m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

mod.ind.60m <- inla(form_ind_60m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

tempcoef1 = data.frame(exp(mod.ind.12m$summary.fixed[-1,c(1,3,5)]))
tempcoef2 = data.frame(exp(mod.ind.36m$summary.fixed[-1,c(1,3,5)]))
tempcoef3 = data.frame(exp(mod.ind.60m$summary.fixed[-1,c(1,3,5)]))

rownames(tempcoef1) = rownames(tempcoef2) = rownames(tempcoef3) =  
  c(
    "Agric. (100m buffer)",
    'Forest (100m buffer)',
    'Devel. (100m buffer)',
    'Devel. in HUC8',
    "Agric. in HUC8",
    'Forest in HUC8',
    'Elevation (10m)',
    'Dist. from coast (10km)',
    'Total Non-OWEB Restoration ($100k)',
    "WC Outreach",
    'WC Tech',
    'WC Capacity',
    'WC Restoration',
    "SWCD Outreach",
    'SWCD Tech',
    'SWCD Capacity',
    'SWCD Restoration',
    'WC Outreach * Tech * Capacity * Restoration',
    'SWCD Outreach * Tech * Capacity * Restoration')


mod.ind.12m = texreg::createTexreg(
  coef.names = rownames(tempcoef1),
  coef = tempcoef1[,1],
  ci.low = tempcoef1[,2],
  ci.up = tempcoef1[,3],
  gof.names = 'DIC',
  gof = mod.ind.12m$dic$dic)

mod.ind.36m = texreg::createTexreg(
  coef.names = rownames(tempcoef2),
  coef = tempcoef2[,1],
  ci.low = tempcoef2[,2],
  ci.up = tempcoef2[,3],
  gof.names = 'DIC',
  gof = mod.ind.36m$dic$dic)

mod.ind.60m = texreg::createTexreg(
  coef.names = rownames(tempcoef3),
  coef = tempcoef3[,1],
  ci.low = tempcoef3[,2],
  ci.up = tempcoef3[,3],
  gof.names = 'DIC',
  gof = mod.ind.60m$dic$dic)


texreg(l = list(mod.ind.12m,mod.ind.36m,mod.ind.60m),
       stars=numeric(0),ci.test = 1,digits = 4,
       custom.model.names = c('Past 12 months','Past 36 months','Past 60 months'),
       caption.above=T,omit.coef = "(100m)|(HUC8)|(10m)|(10km)|Total",
       label = c('table:typefunding'),
       custom.note = "^* 1 outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/typefunding.tex')



##########Capacity Building#############

form_cap_12m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_12_WC + 
  OWEB_Grant_Tech_12_WC + 
  OWEB_Grant_Restoration_12_WC + 
  OWEB_Grant_Capacity_PriorTo12 + 
  OWEB_Grant_Capacity_PriorTo12:OWEB_Grant_Outreach_12_WC + 
  OWEB_Grant_Capacity_PriorTo12:OWEB_Grant_Tech_12_WC + 
  OWEB_Grant_Capacity_PriorTo12:OWEB_Grant_Restoration_12_WC + 
  OWEB_Grant_Capacity_PriorTo12:OWEB_Grant_Outreach_12_WC:OWEB_Grant_Tech_12_WC:OWEB_Grant_Capacity_12_WC:OWEB_Grant_Restoration_12_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


form_cap_36m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_36_WC + 
  OWEB_Grant_Tech_36_WC + 
  OWEB_Grant_Restoration_36_WC + 
  OWEB_Grant_Capacity_PriorTo36 + 
  OWEB_Grant_Capacity_PriorTo36:OWEB_Grant_Outreach_36_WC + 
  OWEB_Grant_Capacity_PriorTo36:OWEB_Grant_Tech_36_WC + 
  OWEB_Grant_Capacity_PriorTo36:OWEB_Grant_Restoration_36_WC + 
  OWEB_Grant_Capacity_PriorTo36:OWEB_Grant_Outreach_36_WC:OWEB_Grant_Tech_36_WC:OWEB_Grant_Capacity_36_WC:OWEB_Grant_Restoration_36_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

form_cap_60m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elev100m + seaDist10km + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_Outreach_60_WC + 
  OWEB_Grant_Tech_60_WC + 
  OWEB_Grant_Restoration_60_WC + 
  OWEB_Grant_Capacity_PriorTo60 + 
  OWEB_Grant_Capacity_PriorTo60:OWEB_Grant_Outreach_60_WC + 
  OWEB_Grant_Capacity_PriorTo60:OWEB_Grant_Tech_60_WC + 
  OWEB_Grant_Capacity_PriorTo60:OWEB_Grant_Restoration_60_WC + 
  OWEB_Grant_Capacity_PriorTo60:OWEB_Grant_Outreach_60_WC:OWEB_Grant_Tech_60_WC:OWEB_Grant_Capacity_60_WC:OWEB_Grant_Restoration_60_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.cap.12m <- inla(form_cap_12m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

mod.cap.36m <- inla(form_cap_36m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

mod.cap.60m <- inla(form_cap_60m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

tempcoef1 = data.frame(exp(mod.cap.12m$summary.fixed[-1,c(1,3,5)]))
tempcoef2 = data.frame(exp(mod.cap.36m$summary.fixed[-1,c(1,3,5)]))
tempcoef3 = data.frame(exp(mod.cap.60m$summary.fixed[-1,c(1,3,5)]))

rownames(tempcoef1) = rownames(tempcoef2) = rownames(tempcoef3) =  
  c(
    "Agric. (100m buffer)",
    'Forest (100m buffer)',
    'Devel. (100m buffer)',
    'Devel. in HUC8',
    "Agric. in HUC8",
    'Forest in HUC8',
    'Elevation (10m)',
    'Dist. from coast (10km)',
    'Total Non-OWEB Restoration ($100k)',
    "WC Outreach",
    'WC Tech',
    'WC Restoration',
    'Prior Capacity',
    "Prior Capacity * WC Outreach",
    'Prior Capacity * WC Tech',
    'Prior Capacity * WC Restoration',
    'Prior Capacity * Outreach * Tech * Restoration')


mod.cap.12m = texreg::createTexreg(
  coef.names = rownames(tempcoef1),
  coef = tempcoef1[,1],
  ci.low = tempcoef1[,2],
  ci.up = tempcoef1[,3],
  gof.names = 'DIC',
  gof = mod.cap.12m$dic$dic)

mod.cap.36m = texreg::createTexreg(
  coef.names = rownames(tempcoef2),
  coef = tempcoef2[,1],
  ci.low = tempcoef2[,2],
  ci.up = tempcoef2[,3],
  gof.names = 'DIC',
  gof = mod.cap.36m$dic$dic)

mod.cap.60m = texreg::createTexreg(
  coef.names = rownames(tempcoef3),
  coef = tempcoef3[,1],
  ci.low = tempcoef3[,2],
  ci.up = tempcoef3[,3],
  gof.names = 'DIC',
  gof = mod.cap.60m$dic$dic)


texreg(l = list(mod.cap.12m,mod.cap.36m,mod.cap.60m),
       stars=numeric(0),ci.test = 1,digits = 4,
       custom.model.names = c('Past 12 months','Past 36 months','Past 60 months'),
       caption.above=T,omit.coef = "(100m)|(HUC8)|(10m)|(10km)|Total",
       label = c('table:capacityfunding'),
       custom.note = "^* 1 outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/capacitybuilding.tex')

mail::sendmail('tyler.andrew.scott@gmail.com','run_models.R finished','nori has finished quinalt project data prep (no precip)')

