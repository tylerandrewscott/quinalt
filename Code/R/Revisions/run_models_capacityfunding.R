
setwd('/homes/tscott1/win/user/quinalt')
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


#load("/homes/tscott1/win/user/quinalt/temp_workspace_precip.RData")
mod.data = read.csv('Input/temp_modeldata_precip.csv')
#load('temp_workspace_precip.RData')

#test = readOGR(dsn='government_units','state_nrcs_a_or')

INLA::inla.setOption(num.threads=16) 

#mod.data = all.params.spdf@data

mod.data$seasonal = mod.data$Abs.Month
mod.data$total.period = mod.data$Abs.Month
#mod.data$sq.owqi = ((as.numeric(as.character(mod.data$owqi)))^2)
#mod.data$l.owqi = log(as.numeric(as.character(mod.data$owqi)))
mod.data = filter(mod.data,YEAR>=1995)
mod.data$HUC8 = as.character(mod.data$HUC8)

covars = mod.data[,c('elevation','seaDist','HUC8','total.period','YEAR',
                     'ag.huc8','dev.huc8','wet.huc8','forst.huc8','owqi',
                     'owqi','monthly.precip.median',
                     'seasonal','Ag','Dev','Wetl','Forst',
                     grep('OWEB',names(mod.data),value=T))]

covars[is.na(covars)] = 0

covars$OWEB_Grant_Capacity_PriorTo12 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_12_WC
covars$OWEB_Grant_Capacity_PriorTo36 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_36_WC
covars$OWEB_Grant_Capacity_PriorTo60 = covars$OWEB_Grant_Capacity_All_WC - covars$OWEB_Grant_Capacity_60_WC


covars = mutate(covars,OWEB_Grant_All_12_WC = OWEB_Grant_Restoration_12_WC+
                  OWEB_Grant_Capacity_12_WC +
                  OWEB_Grant_Tech_12_WC +
                  OWEB_Grant_Outreach_12_WC,
                OWEB_Grant_All_24_WC = OWEB_Grant_Restoration_24_WC+
                  OWEB_Grant_Capacity_24_WC +
                  OWEB_Grant_Tech_24_WC +
                  OWEB_Grant_Outreach_24_WC,
                OWEB_Grant_All_36_WC = OWEB_Grant_Restoration_36_WC+
                  OWEB_Grant_Capacity_36_WC +
                  OWEB_Grant_Tech_36_WC +
                  OWEB_Grant_Outreach_36_WC,
                OWEB_Grant_All_48_WC = OWEB_Grant_Restoration_48_WC+
                  OWEB_Grant_Capacity_48_WC +
                  OWEB_Grant_Tech_48_WC +
                  OWEB_Grant_Outreach_48_WC,
                OWEB_Grant_All_60_WC = OWEB_Grant_Restoration_60_WC+
                  OWEB_Grant_Capacity_60_WC +
                  OWEB_Grant_Tech_60_WC +
                  OWEB_Grant_Outreach_60_WC,
                OWEB_Grant_All_All_WC = OWEB_Grant_Restoration_All_WC+
                  OWEB_Grant_Capacity_All_WC +
                  OWEB_Grant_Tech_All_WC +
                  OWEB_Grant_Outreach_All_WC,
                OWEB_Grant_All_12_SWCD = OWEB_Grant_Restoration_12_SWCD+
                  OWEB_Grant_Capacity_12_SWCD +
                  OWEB_Grant_Tech_12_SWCD +
                  OWEB_Grant_Outreach_12_SWCD,
                OWEB_Grant_All_24_SWCD = OWEB_Grant_Restoration_24_SWCD+
                  OWEB_Grant_Capacity_24_SWCD +
                  OWEB_Grant_Tech_24_SWCD +
                  OWEB_Grant_Outreach_24_SWCD,
                OWEB_Grant_All_36_SWCD = OWEB_Grant_Restoration_36_SWCD+
                  OWEB_Grant_Capacity_36_SWCD +
                  OWEB_Grant_Tech_36_SWCD +
                  OWEB_Grant_Outreach_48_SWCD,
                OWEB_Grant_All_48_SWCD = OWEB_Grant_Restoration_48_SWCD+
                  OWEB_Grant_Capacity_48_SWCD +
                  OWEB_Grant_Tech_48_SWCD +
                  OWEB_Grant_Outreach_48_SWCD,
                OWEB_Grant_All_60_SWCD = OWEB_Grant_Restoration_60_SWCD+
                  OWEB_Grant_Capacity_60_SWCD +
                  OWEB_Grant_Tech_60_SWCD +
                  OWEB_Grant_Outreach_60_SWCD,
                OWEB_Grant_All_All_SWCD = OWEB_Grant_Restoration_All_SWCD+
                  OWEB_Grant_Capacity_All_SWCD +
                  OWEB_Grant_Tech_All_SWCD +
                  OWEB_Grant_Outreach_All_SWCD
)



# for (i in 1:ncol(covars))
# {
#  if (class(covars[,i]) =='numeric'|class(covars[,i]) =='integer')
#  {
#    covars[,i] = as.numeric(base::scale(covars[,i],center = TRUE,scale=TRUE))
#  }
# }


# 
# 

covars$elev100m = covars$elevation/100
covars$seaDist10km = covars$seaDist/10
covars$ag.huc8 = 100 * covars$ag.huc8
covars$dev.huc8 = 100 * covars$dev.huc8
covars$forst.huc8 = 100 * covars$forst.huc8
covars$Ag = 100 * covars$Ag
covars$Forst = 100 * covars$Forst
covars$Dev = 100 * covars$Dev
covars$monthly.precip.median = covars$monthly.precip.median/100

covars[,c('elev100m','seaDist10km','ag.huc8','dev.huc8',
          'forst.huc8','Ag','Dev','Forst','monthly.precip.median','owqi')] = 
  log(covars[,c('elev100m','seaDist10km','ag.huc8','dev.huc8',
                'forst.huc8','Ag','Dev','Forst','monthly.precip.median','owqi')] + 0.001)

k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k
covars[,grep('OWEB',names(covars))] = log(covars[,grep('OWEB',names(covars))]+0.001)

# some book keeping
n.data = length(covars$owqi)


#or.bond = inla.nonconvex.hull(cbind(covars$DECIMAL_LONG,covars$DECIMAL_LAT),2,2)
(mesh.a <- inla.mesh.2d(
  cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
  max.edge=c(5, 40),cut=.2))$n

spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)
stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))




##########Capacity Building#############


# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, 
          covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_12,
          covars$OWEB_Grant_Outreach_12_WC, 
          covars$OWEB_Grant_Tech_12_WC,
          covars$OWEB_Grant_Restoration_12_WC,
          covars$OWEB_Grant_Outreach_12_WC*covars$OWEB_Grant_Capacity_12_WC, 
          covars$OWEB_Grant_Tech_12_WC*covars$OWEB_Grant_Capacity_12_WC,
          covars$OWEB_Grant_Restoration_12_WC*covars$OWEB_Grant_Capacity_12_WC,
          covars$OWEB_Grant_Capacity_12_WC*covars$OWEB_Grant_Outreach_12_WC*covars$OWEB_Grant_Tech_12_WC*covars$OWEB_Grant_Capacity_12_WC*covars$OWEB_Grant_Restoration_12_WC, 
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_cap_12m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_12+
OWEB_Grant_Outreach_12_WC+
OWEB_Grant_Tech_12_WC+
OWEB_Grant_Restoration_12_WC+
OWEB_Grant_Outreach_12_WC:OWEB_Grant_Capacity_12_WC+
OWEB_Grant_Tech_12_WC:OWEB_Grant_Capacity_12_WC+
OWEB_Grant_Restoration_12_WC:OWEB_Grant_Capacity_12_WC+
OWEB_Grant_Capacity_12_WC:OWEB_Grant_Outreach_12_WC:OWEB_Grant_Tech_12_WC:OWEB_Grant_Capacity_12_WC:OWEB_Grant_Restoration_12_WC+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


mod.cap.12m <- inla(form_cap_12m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T, control.inla = list(correct = TRUE, correct.factor = 10))

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, 
          covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_24,
          covars$OWEB_Grant_Outreach_24_WC, 
          covars$OWEB_Grant_Tech_24_WC,
          covars$OWEB_Grant_Restoration_24_WC,
          covars$OWEB_Grant_Outreach_24_WC*covars$OWEB_Grant_Capacity_24_WC, 
          covars$OWEB_Grant_Tech_24_WC*covars$OWEB_Grant_Capacity_24_WC,
          covars$OWEB_Grant_Restoration_24_WC*covars$OWEB_Grant_Capacity_24_WC,
          covars$OWEB_Grant_Capacity_24_WC*covars$OWEB_Grant_Outreach_24_WC*covars$OWEB_Grant_Tech_24_WC*covars$OWEB_Grant_Capacity_24_WC*covars$OWEB_Grant_Restoration_24_WC, 
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_cap_24m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_24+
  OWEB_Grant_Outreach_24_WC+
  OWEB_Grant_Tech_24_WC+
  OWEB_Grant_Restoration_24_WC+
  OWEB_Grant_Outreach_24_WC:OWEB_Grant_Capacity_24_WC+
  OWEB_Grant_Tech_24_WC:OWEB_Grant_Capacity_24_WC+
  OWEB_Grant_Restoration_24_WC:OWEB_Grant_Capacity_24_WC+
  OWEB_Grant_Capacity_24_WC:OWEB_Grant_Outreach_24_WC:OWEB_Grant_Tech_24_WC:OWEB_Grant_Capacity_24_WC:OWEB_Grant_Restoration_24_WC+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)



mod.cap.24m <- inla(form_cap_24m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T, control.inla = list(correct = TRUE, correct.factor = 10))

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, 
          covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_36,
          covars$OWEB_Grant_Outreach_36_WC, 
          covars$OWEB_Grant_Tech_36_WC,
          covars$OWEB_Grant_Restoration_36_WC,
          covars$OWEB_Grant_Outreach_36_WC*covars$OWEB_Grant_Capacity_36_WC, 
          covars$OWEB_Grant_Tech_36_WC*covars$OWEB_Grant_Capacity_36_WC,
          covars$OWEB_Grant_Restoration_36_WC*covars$OWEB_Grant_Capacity_36_WC,
          covars$OWEB_Grant_Capacity_36_WC*covars$OWEB_Grant_Outreach_36_WC*covars$OWEB_Grant_Tech_36_WC*covars$OWEB_Grant_Capacity_36_WC*covars$OWEB_Grant_Restoration_36_WC, 
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_cap_36m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_36+
  OWEB_Grant_Outreach_36_WC+
  OWEB_Grant_Tech_36_WC+
  OWEB_Grant_Restoration_36_WC+
  OWEB_Grant_Outreach_36_WC:OWEB_Grant_Capacity_36_WC+
  OWEB_Grant_Tech_36_WC:OWEB_Grant_Capacity_36_WC+
  OWEB_Grant_Restoration_36_WC:OWEB_Grant_Capacity_36_WC+
  OWEB_Grant_Capacity_36_WC:OWEB_Grant_Outreach_36_WC:OWEB_Grant_Tech_36_WC:OWEB_Grant_Capacity_36_WC:OWEB_Grant_Restoration_36_WC+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

mod.cap.36m <- inla(form_cap_36m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),verbose=T, control.inla = list(correct = TRUE, correct.factor = 10))

tempcoef1 = data.frame(exp(mod.cap.12m$summary.fixed[-1,c(1,3,5)]))
tempcoef2 = data.frame(exp(mod.cap.24m$summary.fixed[-1,c(1,3,5)]))
tempcoef3 = data.frame(exp(mod.cap.36m$summary.fixed[-1,c(1,3,5)]))

# 
# 
tempcoefcapacity12 = data.frame((mod.cap.12m$summary.fixed[-c(1:11),c(1,3,5)]))
tempcoefcapacity24 = data.frame((mod.cap.24m$summary.fixed[-c(1:11),c(1,3,5)]))
tempcoefcapacity36 = data.frame((mod.cap.36m$summary.fixed[-c(1:11),c(1,3,5)]))

rownames(tempcoefcapacity12) = gsub('_12','',rownames(tempcoefcapacity12))
rownames(tempcoefcapacity24) = gsub('_24','',rownames(tempcoefcapacity24))
rownames(tempcoefcapacity36) = gsub('_36','',rownames(tempcoefcapacity36))

tempcoefcapacity12$Covar = rownames(tempcoefcapacity12)
tempcoefcapacity24$Covar = rownames(tempcoefcapacity24)
tempcoefcapacity36$Covar = rownames(tempcoefcapacity36)
tempcoefcapacity12$Lag = 'Past 12 months\' funding'
tempcoefcapacity24$Lag = 'Past 24 months\' funding'
tempcoefcapacity36$Lag = 'Past 36 months\' funding'
tempcoefcapacity12$Order = -(1:nrow(tempcoefcapacity12))
tempcoefcapacity24$Order = -(1:nrow(tempcoefcapacity24))
tempcoefcapacity36$Order = -(1:nrow(tempcoefcapacity36))

tempcoefcapacity12$Name = c( 
  'Outreach',
  'Tech.',
  'Restoration',
  'Outreach * Capacity',
  'Tech. * Capacity',
  'Restoration * Capacity.',
  'Full interaction'
)
tempcoefcapacity24$Name = c( 
  'Outreach',
  'Tech.',
  'Restoration',
  'Outreach * Capacity',
  'Tech. * Capacity',
  'Restoration * Capacity.',
  'Full interaction'
)
tempcoefcapacity36$Name = c( 
  'Outreach',
  'Tech.',
  'Restoration',
  'Outreach * Capacity',
  'Tech. * Capacity',
  'Restoration * Capacity.',
  'Full interaction'
)

plottemp = rbind(tempcoefcapacity12,tempcoefcapacity24,tempcoefcapacity36)

name.vec = c( 
  'Outreach',
  'Tech.',
  'Restoration',
  'Outreach * Capacity',
  'Tech. * Capacity',
  'Restoration * Capacity.',
  'Full interaction')

library(ggplot2)
library(ggthemes)

capacitycoefplot = ggplot(data=plottemp) + 
  geom_segment(aes(x=X0.025quant,xend=X0.975quant,y=Order,yend=Order),
               lwd=2,lineend='round') + 
  facet_wrap(~Lag) + 
  #scale_x_continuous(name='Credible interval (0.025 to 0.975)',
   #                  limits=c(-0.006,0.006))+
  scale_x_continuous(name='Credible interval (0.025 to 0.975)')+
  theme_bw() +
  scale_y_discrete(name='',labels= name.vec)+
  theme(
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    strip.text = element_text(size=14))  +
  geom_vline(xintercept = 0,lty=2)

plot(capacitycoefplot)
c(min(plottemp$Order),max(plottemp$Order))
plottemp
head(plottemp$Covar)
ggsave('JPART_Submission/Version2/capacitycoefplot.png',capacitycoefplot)


mod.12tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefcapacity12[,1],
  ci.low = tempcoefcapacity12[,2],
  ci.up = tempcoefcapacity12[,3],
  gof.names = 'DIC',
  gof = mod.cap.12m$dic$dic)


mod.cap.12m$names.fixed
mod.cap.12m$summary.fixed[c(12:17),]


mod.24tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefcapacity24[,1],
  ci.low = tempcoefcapacity24[,2],
  ci.up = tempcoefcapacity24[,3],
  gof.names = 'DIC',
  gof = mod.cap.24m$dic$dic)

mod.36tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefcapacity36[,1],
  ci.low = tempcoefcapacity36[,2],
  ci.up = tempcoefcapacity36[,3],
  gof.names = 'DIC',
  gof = mod.cap.36m$dic$dic)

# 
texreg(l = list(mod.12tex,mod.24tex,mod.36tex),
       stars=numeric(0),ci.test = 0,digits = 3,
       custom.model.names = c('Past 12 months','Past 36 months','Past 60 months'),
       caption.above=T,#omit.coef = "(100m)|(HUC8)|(10m)|(10km)|Total|precip",
       label = c('table:capacityfunding'),
       caption = 'Predicted water quality impact by grant type',
       custom.note = "$^* 0$ outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/capacityfunding.tex')

rm(list=ls())
load('results_capacityfunding.RData')

#save.image('results_capacityfunding.RData')
mail::sendmail('tyler.andrew.scott@gmail.com','run_models_capacityfunding.R finished','nori has finished running capacityfunding comp models')
# 

