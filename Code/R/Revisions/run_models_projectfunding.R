
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



##########Project type funding###############

X = cbind(rep(1,n.data),
          covars$Ag,
          covars$Forst,
          covars$Dev,  covars$ag.huc8, covars$forst.huc8,
          covars$dev.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_12,
          covars$OWEB_Grant_Outreach_12_WC,
          covars$OWEB_Grant_Tech_12_WC, 
          covars$OWEB_Grant_Restoration_12_WC, 
          covars$OWEB_Grant_Outreach_12_SWCD, 
          covars$OWEB_Grant_Tech_12_SWCD,
          covars$OWEB_Grant_Restoration_12_SWCD, 
          covars$OWEB_Grant_Outreach_12_WC*covars$OWEB_Grant_Tech_12_WC*covars$OWEB_Grant_Restoration_12_WC, 
          covars$OWEB_Grant_Outreach_12_SWCD*covars$OWEB_Grant_Tech_12_SWCD*covars$OWEB_Grant_Restoration_12_SWCD,
          covars$OWEB_Grant_Outreach_12_WC*covars$OWEB_Grant_Tech_12_WC*covars$OWEB_Grant_Restoration_12_WC*
          covars$OWEB_Grant_Outreach_12_SWCD*covars$OWEB_Grant_Tech_12_SWCD*covars$OWEB_Grant_Restoration_12_SWCD,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))

form_ind_12m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  ag.huc8+
  forst.huc8 + dev.huc8 + elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_12 + 
  OWEB_Grant_Outreach_12_WC + 
  OWEB_Grant_Tech_12_WC + 
  OWEB_Grant_Restoration_12_WC + 
  OWEB_Grant_Outreach_12_SWCD + 
  OWEB_Grant_Tech_12_SWCD + 
  OWEB_Grant_Restoration_12_SWCD + 
  OWEB_Grant_Outreach_12_WC:OWEB_Grant_Tech_12_WC:OWEB_Grant_Restoration_12_WC +
OWEB_Grant_Outreach_12_SWCD:OWEB_Grant_Tech_12_SWCD:OWEB_Grant_Restoration_12_SWCD + 
OWEB_Grant_Outreach_12_WC:OWEB_Grant_Tech_12_WC:OWEB_Grant_Restoration_12_WC:
  OWEB_Grant_Outreach_12_SWCD:OWEB_Grant_Tech_12_SWCD:OWEB_Grant_Restoration_12_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.ind.12m <- inla(form_ind_12m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),
                    control.fixed= list(prec.intercept = 1),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = 10))

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
          covars$OWEB_Grant_Outreach_24_SWCD, 
          covars$OWEB_Grant_Tech_24_SWCD,
          covars$OWEB_Grant_Restoration_24_SWCD, 
          covars$OWEB_Grant_Outreach_24_WC*covars$OWEB_Grant_Tech_24_WC*covars$OWEB_Grant_Restoration_24_WC, 
          covars$OWEB_Grant_Outreach_24_SWCD*covars$OWEB_Grant_Tech_24_SWCD*covars$OWEB_Grant_Restoration_24_SWCD,
          covars$OWEB_Grant_Outreach_24_WC*covars$OWEB_Grant_Tech_24_WC*covars$OWEB_Grant_Restoration_24_WC*
            covars$OWEB_Grant_Outreach_24_SWCD*covars$OWEB_Grant_Tech_24_SWCD*covars$OWEB_Grant_Restoration_24_SWCD,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))

form_ind_24m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  ag.huc8+
  forst.huc8 + dev.huc8 + elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_24+
  OWEB_Grant_Outreach_24_WC + 
  OWEB_Grant_Tech_24_WC + 
  OWEB_Grant_Restoration_24_WC + 
  OWEB_Grant_Outreach_24_SWCD + 
  OWEB_Grant_Tech_24_SWCD + 
  OWEB_Grant_Restoration_24_SWCD + 
  OWEB_Grant_Outreach_24_WC:OWEB_Grant_Tech_24_WC:OWEB_Grant_Restoration_24_WC +
  OWEB_Grant_Outreach_24_SWCD:OWEB_Grant_Tech_24_SWCD:OWEB_Grant_Restoration_24_SWCD + 
  OWEB_Grant_Outreach_24_WC:OWEB_Grant_Tech_24_WC:OWEB_Grant_Restoration_24_WC:
  OWEB_Grant_Outreach_24_SWCD:OWEB_Grant_Tech_24_SWCD:OWEB_Grant_Restoration_24_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))



mod.ind.24m <- inla(form_ind_24m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),
                    control.fixed= list(prec.intercept = 1),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = 10))
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
          covars$OWEB_Grant_Outreach_36_SWCD, 
          covars$OWEB_Grant_Tech_36_SWCD,
          covars$OWEB_Grant_Restoration_36_SWCD, 
          covars$OWEB_Grant_Outreach_36_WC*covars$OWEB_Grant_Tech_36_WC*covars$OWEB_Grant_Restoration_36_WC, 
          covars$OWEB_Grant_Outreach_36_SWCD*covars$OWEB_Grant_Tech_36_SWCD*covars$OWEB_Grant_Restoration_36_SWCD,
          covars$OWEB_Grant_Outreach_36_WC*covars$OWEB_Grant_Tech_36_WC*covars$OWEB_Grant_Restoration_36_WC*
            covars$OWEB_Grant_Outreach_36_SWCD*covars$OWEB_Grant_Tech_36_SWCD*covars$OWEB_Grant_Restoration_36_SWCD,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))

form_ind_36m <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  ag.huc8+
  forst.huc8 +dev.huc8 +  elevation + seaDist + 
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_36 + 
OWEB_Grant_Outreach_36_WC + 
  OWEB_Grant_Tech_36_WC + 
  OWEB_Grant_Restoration_36_WC + 
  OWEB_Grant_Outreach_36_SWCD + 
  OWEB_Grant_Tech_36_SWCD + 
  OWEB_Grant_Restoration_36_SWCD + 
  OWEB_Grant_Outreach_36_WC:OWEB_Grant_Tech_36_WC:OWEB_Grant_Restoration_36_WC +
  OWEB_Grant_Outreach_36_SWCD:OWEB_Grant_Tech_36_SWCD:OWEB_Grant_Restoration_36_SWCD + 
  OWEB_Grant_Outreach_36_WC:OWEB_Grant_Tech_36_WC:OWEB_Grant_Restoration_36_WC:
  OWEB_Grant_Outreach_36_SWCD:OWEB_Grant_Tech_36_SWCD:OWEB_Grant_Restoration_36_SWCD + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.ind.36m <- inla(form_ind_36m, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE),
                    control.fixed= list(prec.intercept = 1),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = 10))
       
#save.image('temp_projectresults.RData')
#mail::sendmail('tyler.andrew.scott@gmail.com','run_models_projectfunding.R finished','nori has finished running projectfunding comp models')

#load('temp_projectresults.RData')


# 
# 
tempcoefproject12 = data.frame((mod.ind.12m$summary.fixed[-c(1:11),c(1,3,5)]))
tempcoefproject24 = data.frame((mod.ind.24m$summary.fixed[-c(1:11),c(1,3,5)]))
tempcoefproject36 = data.frame((mod.ind.36m$summary.fixed[-c(1:11),c(1,3,5)]))

rownames(tempcoefproject12) = gsub('_12','',rownames(tempcoefproject12))
rownames(tempcoefproject24) = gsub('_24','',rownames(tempcoefproject24))
rownames(tempcoefproject36) = gsub('_36','',rownames(tempcoefproject36))

tempcoefproject12$Covar = rownames(tempcoefproject12)
tempcoefproject24$Covar = rownames(tempcoefproject24)
tempcoefproject36$Covar = rownames(tempcoefproject36)
tempcoefproject12$Lag = 'Past 12 months\' funding'
tempcoefproject24$Lag = 'Past 24 months\' funding'
tempcoefproject36$Lag = 'Past 36 months\' funding'
tempcoefproject12$Order = nrow(tempcoefproject12):1
tempcoefproject24$Order = nrow(tempcoefproject24):1
tempcoefproject36$Order = nrow(tempcoefproject36):1


# 
plottemp = rbind(tempcoefproject12,tempcoefproject24,tempcoefproject36)
# 
name.vec = c( 
              'All projects interaction',
              'All SWCD interaction',
              'All WC interaction',
              'SWCD Restoration',
              'SWCD Tech.',
              'SWCD Outreach',
              'WC Restoration',
              'WC Tech.',
              'WC Outreach'
              )

library(ggplot2)
library(ggthemes)
projectcoefplot = ggplot(data=plottemp) + 
  geom_segment(aes(x=X0.025quant,xend=X0.975quant,y=as.factor(Order),yend=as.factor(Order)),
               lwd=3,lineend='round') + 
  facet_wrap(~Lag) + scale_x_continuous(name='Posterior credible interval (0.025 to 0.975)',
                                        limits=c(-0.015,0.015),breaks=c(-0.01,0.00,0.01))+ 
  theme_bw() +
  scale_y_discrete(name='',labels= name.vec) + 
  theme(
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    strip.text = element_text(size=14))  +
  geom_vline(xintercept = 0,lty=2)


plot(projectcoefplot)
ggsave('JPART_Submission/Version2/projectcoefplot.png',projectcoefplot)



# 
# 
# 

# tempcoef1 = data.frame(exp(mod.ind.12m$summary.fixed[-1,c(1,3,5)]))
# tempcoef2 = data.frame(exp(mod.ind.24m$summary.fixed[-1,c(1,3,5)]))
# tempcoef3 = data.frame(exp(mod.ind.36m$summary.fixed[-1,c(1,3,5)]))
# 
# rownames(tempcoef1) = rownames(tempcoef2) = rownames(tempcoef3) =  
#   c(
#     "$\\%$  Agric. (100m buffer)",
#     '$\\%$  Forest (100m buffer)',
#     '$\\%$  Devel. (100m buffer)',
#     '$\\%$  Devel. in HUC8',
#     "$\\%$  Agric. in HUC8",
#     '$\\%$  Forest in HUC8',
#     'Elevation (10m)',
#     'Dist. from coast (10km)',
#     'Monthly precip.',
#     'Total Non-OWEB Restoration',
#     "WC Outreach",
#     'WC Tech',
#     'WC Capacity',
#     'WC Restoration',
#     "SWCD Outreach",
#     'SWCD Tech',
#     'SWCD Capacity',
#     'SWCD Restoration',
#     'WC 4-Way Interaction',
#     'SWCD 4-Way Interaction')
# 
# 
mod.12tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefproject12[,1],
  ci.low = tempcoefproject12[,2],
  ci.up = tempcoefproject12[,3],
  gof.names = 'DIC',
  gof = mod.ind.12m$dic$dic)

mod.24tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefproject24[,1],
  ci.low = tempcoefproject24[,2],
  ci.up = tempcoefproject24[,3],
  gof.names = 'DIC',
  gof = mod.ind.24m$dic$dic)

mod.36tex = texreg::createTexreg(
  coef.names = rev(name.vec),
  coef = tempcoefproject36[,1],
  ci.low = tempcoefproject36[,2],
  ci.up = tempcoefproject36[,3],
  gof.names = 'DIC',
  gof = mod.ind.36m$dic$dic)

# 
texreg(l = list(mod.12tex,mod.24tex,mod.36tex),
       stars=numeric(0),ci.test = 0,digits = 3,
       custom.model.names = c('Past 12 months','Past 36 months','Past 60 months'),
       caption.above=T,#omit.coef = "(100m)|(HUC8)|(10m)|(10km)|Total|precip",
       label = c('table:typefunding'),
       caption = 'Association between prior funding by project type and water quality ',
       custom.note = "$^* 0$ outside the credible interval",
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/projectfunding.tex')

rm(list=ls())
setwd('~/win/user/quinalt/')
list.files()
load('results_projectfunding.RData')


save.image('results_projectfunding.RData')
mail::sendmail('tyler.andrew.scott@gmail.com','run_models_projectfunding.R finished','nori has finished running projectfunding comp models')
# 


