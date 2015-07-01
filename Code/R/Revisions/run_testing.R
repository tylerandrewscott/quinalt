
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

names(mod.data)

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

k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k

  
  

# some book keeping
n.data = length(covars$owqi)

#or.bond = inla.nonconvex.hull(cbind(covars$DECIMAL_LONG,covars$DECIMAL_LAT),2,2)
(mesh.a <- inla.mesh.2d(
  cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
  max.edge=c(5, 40),cut=.05))$n
spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)
stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

#######12 months##########
form_base_nonspatial.12 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_12_WC + 
  #f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.12 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_12_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, covars$NOT_OWEB_OWRI.wq.TotalCash,
            covars$OWEB_Grant_All_12_WC,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_spatial_restricted.12 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_12_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
f(s, model=spde.a,
  extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.base.nonspatial.12 <- inla(form_base_nonspatial.12, 
                            data=data.frame(y=covars$owqi, covars,b0=1), 
                            control.predictor=list(compute=TRUE),
                            control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                            control.inla = list(
                              correct = TRUE,
                              correct.factor = 10))

mod.base.spatial.unrestricted.12 <- inla(form_base_spatial_unrestricted.12, family='gaussian',
                                   data=inla.stack.data(stk.1),
                                   control.predictor=list(A=inla.stack.A(stk.1), 
                                                          compute=TRUE),
                                   #  control.inla=list(strategy='laplace'), 
                                   control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                   control.inla = list(
                                     correct = TRUE,
                                     correct.factor = 10))

mod.base.spatial.restricted.12 <- inla(form_base_spatial_restricted.12, family='gaussian',
                        data=inla.stack.data(stk.1),
                        control.predictor=list(A=inla.stack.A(stk.1), 
                          compute=TRUE),
                        #  control.inla=list(strategy='laplace'), 
                        control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                        control.inla = list(
                          correct = TRUE,
                          correct.factor = 10))



##### 24 months ###########

form_base_nonspatial.24 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_24_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.24 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_24_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, covars$NOT_OWEB_OWRI.wq.TotalCash,
          covars$OWEB_Grant_All_24_WC,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_spatial_restricted.24 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_24_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.base.nonspatial.24 <- inla(form_base_nonspatial.24, 
                               data=data.frame(y=covars$l.owqi, covars,b0=1), 
                               control.predictor=list(compute=TRUE),
                               control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                               control.inla = list(
                                 correct = TRUE,
                                 correct.factor = 10))

mod.base.spatial.unrestricted.24 <- inla(form_base_spatial_unrestricted.24, family='gaussian',
                                         data=inla.stack.data(stk.1),
                                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                                compute=TRUE),
                                         #  control.inla=list(strategy='laplace'), 
                                         control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                         control.inla = list(
                                           correct = TRUE,
                                           correct.factor = 10))

mod.base.spatial.restricted.24 <- inla(form_base_spatial_restricted.24, family='gaussian',
                                       data=inla.stack.data(stk.1),
                                       control.predictor=list(A=inla.stack.A(stk.1), 
                                                              compute=TRUE),
                                       #  control.inla=list(strategy='laplace'), 
                                       control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                       control.inla = list(
                                         correct = TRUE,
                                         correct.factor = 10))

##### 36 months ###########

form_base_nonspatial.36 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_36_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.36 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_36_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash,
          covars$OWEB_Grant_All_36_WC,
          covars$HUC8, covars$total.period,
          covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_spatial_restricted.36 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_36_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.base.nonspatial.36 <- inla(form_base_nonspatial.36, 
                               data=data.frame(y=covars$l.owqi, covars,b0=1), 
                               control.predictor=list(compute=TRUE),
                               control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                               control.inla = list(
                                 correct = TRUE,
                                 correct.factor = 10))

mod.base.spatial.unrestricted.36 <- inla(form_base_spatial_unrestricted.36, family='gaussian',
                                         data=inla.stack.data(stk.1),
                                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                                compute=TRUE),
                                         #  control.inla=list(strategy='laplace'), 
                                         control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                         control.inla = list(
                                           correct = TRUE,
                                           correct.factor = 10))

mod.base.spatial.restricted.36 <- inla(form_base_spatial_restricted.36, family='gaussian',
                                       data=inla.stack.data(stk.1),
                                       control.predictor=list(A=inla.stack.A(stk.1), 
                                                              compute=TRUE),
                                       #  control.inla=list(strategy='laplace'), 
                                       control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                       control.inla = list(
                                         correct = TRUE,
                                         correct.factor = 10))

##### 48 months ###########

form_base_nonspatial.48 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_48_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.48 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_48_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, covars$NOT_OWEB_OWRI.wq.TotalCash,
          covars$OWEB_Grant_All_48_WC,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_spatial_restricted.48 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_48_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.base.nonspatial.48 <- inla(form_base_nonspatial.48, 
                               data=data.frame(y=covars$l.owqi, covars,b0=1), 
                               control.predictor=list(compute=TRUE),
                               control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                               control.inla = list(
                                 correct = TRUE,
                                 correct.factor = 10))

mod.base.spatial.unrestricted.48 <- inla(form_base_spatial_unrestricted.48, family='gaussian',
                                         data=inla.stack.data(stk.1),
                                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                                compute=TRUE),
                                         #  control.inla=list(strategy='laplace'), 
                                         control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                         control.inla = list(
                                           correct = TRUE,
                                           correct.factor = 10))

mod.base.spatial.restricted.48 <- inla(form_base_spatial_restricted.48, family='gaussian',
                                       data=inla.stack.data(stk.1),
                                       control.predictor=list(A=inla.stack.A(stk.1), 
                                                              compute=TRUE),
                                       #  control.inla=list(strategy='laplace'), 
                                       control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                       control.inla = list(
                                         correct = TRUE,
                                         correct.factor = 10))

##### 60 months ###########

form_base_nonspatial.60 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_60_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.60 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_60_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, covars$NOT_OWEB_OWRI.wq.TotalCash,
          covars$OWEB_Grant_All_60_WC,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_spatial_restricted.60 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_60_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + 
  f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod.base.nonspatial.60 <- inla(form_base_nonspatial.60, 
                               data=data.frame(y=covars$l.owqi, covars,b0=1), 
                               control.predictor=list(compute=TRUE),
                               control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                               control.inla = list(
                                 correct = TRUE,
                                 correct.factor = 10))

mod.base.spatial.unrestricted.60 <- inla(form_base_spatial_unrestricted.60, family='gaussian',
                                         data=inla.stack.data(stk.1),
                                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                                compute=TRUE),
                                         #  control.inla=list(strategy='laplace'), 
                                         control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                         control.inla = list(
                                           correct = TRUE,
                                           correct.factor = 10))

mod.base.spatial.restricted.60 <- inla(form_base_spatial_restricted.60, family='gaussian',
                                       data=inla.stack.data(stk.1),
                                       control.predictor=list(A=inla.stack.A(stk.1), 
                                                              compute=TRUE),
                                       #  control.inla=list(strategy='laplace'), 
                                       control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                       control.inla = list(
                                         correct = TRUE,
                                         correct.factor = 10))

##### All months ###########

form_base_nonspatial.All <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_All_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)

form_base_spatial_unrestricted.All <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_All_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + 
  f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a)

# put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
X = cbind(rep(1,n.data),
          covars$Ag, covars$Forst,
          covars$Dev, covars$dev.huc8,
          covars$ag.huc8, covars$forst.huc8,
          covars$seaDist, covars$elevation,
          covars$monthly.precip.median, covars$NOT_OWEB_OWRI.wq.TotalCash,
          covars$OWEB_Grant_All_All_WC,
          covars$HUC8, covars$total.period,covars$seasonal)
n.covariates = ncol(X)
Q = qr.Q(qr(X))

form_base_spatial_restricted.All <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash + 
  OWEB_Grant_All_All_WC + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))

mod.base.nonspatial.All <- inla(form_base_nonspatial.All, 
                                data=data.frame(y=covars$l.owqi, covars,b0=1), 
                                control.predictor=list(compute=TRUE),
                                control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                control.inla = list(
                                  correct = TRUE,
                                  correct.factor = 10))

mod.base.spatial.unrestricted.All <- inla(form_base_spatial_unrestricted.All, family='gaussian',
                                          data=inla.stack.data(stk.1),
                                          control.predictor=list(A=inla.stack.A(stk.1), 
                                                                 compute=TRUE),
                                          #  control.inla=list(strategy='laplace'), 
                                          control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                          control.inla = list(
                                            correct = TRUE,
                                            correct.factor = 10))

mod.base.spatial.restricted.All <- inla(form_base_spatial_restricted.All, family='gaussian',
                                        data=inla.stack.data(stk.1),
                                        control.predictor=list(A=inla.stack.A(stk.1), 
                                                               compute=TRUE),
                                        #  control.inla=list(strategy='laplace'), 
                                        control.compute=list(dic=TRUE, cpo=TRUE),verbose=T,
                                        control.inla = list(
                                          correct = TRUE,
                                          correct.factor = 10))
                                          
tempcoefnonspatial12 = data.frame(exp(mod.base.nonspatial.12$summary.fixed[-1,c(1,3,5)]))
tempcoefspatial12 = data.frame(exp(mod.base.spatial.unrestricted.12$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestricted12 = data.frame(exp(mod.base.spatial.restricted.12$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatial24 = data.frame(exp(mod.base.nonspatial.24$summary.fixed[-1,c(1,3,5)]))
tempcoefspatial24 = data.frame(exp(mod.base.spatial.unrestricted.24$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestricted24 = data.frame(exp(mod.base.spatial.restricted.24$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatial36 = data.frame(exp(mod.base.nonspatial.36$summary.fixed[-1,c(1,3,5)]))
tempcoefspatial36 = data.frame(exp(mod.base.spatial.unrestricted.36$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestricted36 = data.frame(exp(mod.base.spatial.restricted.36$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatial48 = data.frame(exp(mod.base.nonspatial.48$summary.fixed[-1,c(1,3,5)]))
tempcoefspatial48 = data.frame(exp(mod.base.spatial.unrestricted.48$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestricted48 = data.frame(exp(mod.base.spatial.restricted.48$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatial60 = data.frame(exp(mod.base.nonspatial.60$summary.fixed[-1,c(1,3,5)]))
tempcoefspatial60 = data.frame(exp(mod.base.spatial.unrestricted.60$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestricted60 = data.frame(exp(mod.base.spatial.restricted.60$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatialAll = data.frame(exp(mod.base.nonspatial.All$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialAll = data.frame(exp(mod.base.spatial.unrestricted.All$summary.fixed[-1,c(1,3,5)]))
tempcoefspatialrestrictedAll = data.frame(exp(mod.base.spatial.restricted.All$summary.fixed[-1,c(1,3,5)]))

tempcoefnonspatial12$ID = rownames(tempcoefnonspatial12)
tempcoefnonspatial12$Model = 'tempcoefnonspatial12'
tempcoefnonspatial24$ID = rownames(tempcoefnonspatial24)
tempcoefnonspatial24$Model = 'tempcoefnonspatial24'
tempcoefnonspatial36$ID = rownames(tempcoefnonspatial36)
tempcoefnonspatial36$Model = 'tempcoefnonspatial36'
tempcoefnonspatial48$ID = rownames(tempcoefnonspatial48)
tempcoefnonspatial48$Model = 'tempcoefnonspatial48'
tempcoefnonspatial60$ID = rownames(tempcoefnonspatial60)
tempcoefnonspatial60$Model = 'tempcoefnonspatial60'
tempcoefnonspatialAll$ID = rownames(tempcoefnonspatialAll)
tempcoefnonspatialAll$Model = 'tempcoefnonspatialAll'
coefnonspatial = join_all(list(melt(tempcoefnonspatial12,id.vars='ID'),
     melt(tempcoefnonspatial24,id.vars='ID'),
     melt(tempcoefnonspatial36,id.vars='ID'),
     melt(tempcoefnonspatial48,id.vars='ID'),
     melt(tempcoefnonspatial60,id.vars='ID'),
     melt(tempcoefnonspatialAll,id.vars='ID')))
     
tempcoefspatial12$ID = rownames(tempcoefspatial12)
tempcoefspatial12$Model = 'tempcoefspatial12'
tempcoefspatial24$ID = rownames(tempcoefspatial24)
tempcoefspatial24$Model = 'tempcoefspatial24'
tempcoefspatial36$ID = rownames(tempcoefspatial36)
tempcoefspatial36$Model = 'tempcoefspatial36'
tempcoefspatial48$ID = rownames(tempcoefspatial48)
tempcoefspatial48$Model = 'tempcoefspatial48'
tempcoefspatial60$ID = rownames(tempcoefspatial60)
tempcoefspatial60$Model = 'tempcoefspatial60'
tempcoefspatialAll$ID = rownames(tempcoefspatialAll)
tempcoefspatialAll$Model = 'tempcoefspatialAll'
coefspatial = join_all(list(melt(tempcoefspatial12,id.vars='ID'),
                      melt(tempcoefspatial24,id.vars='ID'),
                      melt(tempcoefspatial36,id.vars='ID'),
                      melt(tempcoefspatial48,id.vars='ID'),
                      melt(tempcoefspatial60,id.vars='ID'),
                      melt(tempcoefspatialAll,id.vars='ID')))


tempcoefspatialrestricted12$ID = rownames(tempcoefspatialrestricted12)
tempcoefspatialrestricted12$Model = 'tempcoefspatialrestricted12'
tempcoefspatialrestricted24$ID = rownames(tempcoefspatialrestricted24)
tempcoefspatialrestricted24$Model = 'tempcoefspatialrestricted24'
tempcoefspatialrestricted36$ID = rownames(tempcoefspatialrestricted36)
tempcoefspatialrestricted36$Model = 'tempcoefspatialrestricted36'
tempcoefspatialrestricted48$ID = rownames(tempcoefspatialrestricted48)
tempcoefspatialrestricted48$Model = 'tempcoefspatialrestricted48'
tempcoefspatialrestricted60$ID = rownames(tempcoefspatialrestricted60)
tempcoefspatialrestricted60$Model = 'tempcoefspatialrestricted60'
tempcoefspatialrestrictedAll$ID = rownames(tempcoefspatialrestrictedAll)
tempcoefspatialrestrictedAll$Model = 'tempcoefspatialrestrictedAll'
coefspatialrestricted = join_all(list(melt(tempcoefspatialrestricted12,id.vars='ID'),
                   melt(tempcoefspatialrestricted24,id.vars='ID'),
                   melt(tempcoefspatialrestricted36,id.vars='ID'),
                   melt(tempcoefspatialrestricted48,id.vars='ID'),
                   melt(tempcoefspatialrestricted60,id.vars='ID'),
                   melt(tempcoefspatialrestrictedAll,id.vars='ID')))

allbasecoefs = join_all(list(coefnonspatial,coefspatial,coefspatialrestricted))









tempcoefnonpatial24 = data.frame(exp(mod.base.nonspatial.24$summary.fixed[-1,c(1,3,5)]))
tempcoefnonpatial36 = data.frame(exp(mod.base.nonspatial.36$summary.fixed[-1,c(1,3,5)]))
tempcoefnonpatial48 = data.frame(exp(mod.base.nonspatial.48$summary.fixed[-1,c(1,3,5)]))
tempcoefnonpatial60 = data.frame(exp(mod.base.nonspatial.60$summary.fixed[-1,c(1,3,5)]))
tempcoefnonpatialAll = data.frame(exp(mod.base.nonspatial.All$summary.fixed[-1,c(1,3,5)]))

rowname.vector = c(
  "$\\%$  Agric. (100m buffer)",
  '$\\%$  Forest (100m buffer)',
  '$\\%$  Devel. (100m buffer)',
  '$\\%$  Devel. in HUC8',
  "$\\%$  Agric. in HUC8",
  '$\\%$  Forest in HUC8',
  'Elevation (10m)',
  'Dist. from coast (10km)',
  'Monthly precip.',
  'Non-OWEB Restoration',
  'OWEB funds to WC')

library(lme4)
library(texreg)

modbase.spatial.restricted.12 = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestricted12[,1],
  ci.low = tempcoefspatialrestricted12[,2],
  ci.up = tempcoefspatialrestricted12[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.12$dic$dic)

modbase.spatial.restricted.24 = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestricted24[,1],
  ci.low = tempcoefspatialrestricted24[,2],
  ci.up = tempcoefspatialrestricted24[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.24$dic$dic)


modbase.spatial.restricted.36 = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestricted36[,1],
  ci.low = tempcoefspatialrestricted36[,2],
  ci.up = tempcoefspatialrestricted36[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.36$dic$dic)

modbase.spatial.restricted.48 = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestricted48[,1],
  ci.low = tempcoefspatialrestricted48[,2],
  ci.up = tempcoefspatialrestricted48[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.48$dic$dic)

modbase.spatial.restricted.60 = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestricted60[,1],
  ci.low = tempcoefspatialrestricted60[,2],
  ci.up = tempcoefspatialrestricted60[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.60$dic$dic)

modbase.spatial.restricted.All = texreg::createTexreg(
  coef.names = rowname.vector,
  coef = tempcoefspatialrestrictedAll[,1],
  ci.low = tempcoefspatialrestrictedAll[,2],
  ci.up = tempcoefspatialrestrictedAll[,3],
  gof.names = 'DIC',
  gof = mod.base.spatial.restricted.All$dic$dic)

texreg(l = list(modbase.spatial.restricted.12,
                modbase.spatial.restricted.24,
                modbase.spatial.restricted.36,
                modbase.spatial.restricted.48,
                modbase.spatial.restricted.60,
                modbase.spatial.restricted.All),
       stars=numeric(0),ci.test = 1,digits = 3,
       caption = "Baseline models with only OWEB funding to Watershed Councils", caption.above = TRUE, 
       custom.model.names = c('Past 12 months','Past 24 months','Past 36 months','Past 48 months',"All prior funds"),
       label = c('table:basemods'),
       custom.note = "$^* 1$ outside the credible interval",
       custom.coef.names = rowname.vector,
       file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/basemods.tex')

save.image('results_basefunding.RData')
mail::sendmail('tyler.andrew.scott@gmail.com','run_models_basefunding.R finished','nori has base model fitting')
