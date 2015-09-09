rm(list=ls())


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
mod.data = read.csv('Input/temp_data.csv')
mod.data = mod.data %>% mutate(altName = as.character(altName))

mod.data$which.wc[grep('Middle Deschutes',mod.data$which.wc)] <- 'Willow Creek WC'


library(mosaic)
council.dat = fetchGoogle('https://docs.google.com/spreadsheets/d/1OXdC54OK8BwXAbIzMDudAKMvdblMhQQBJZ82_LUijFc/pub?output=csv')

temp = dplyr::select(mod.data, which.wc)

test = left_join(temp,council.dat,by=c('which.wc' = 'altName'))


unique(test$which.wc[is.na(test$Federal.Support)])



class(temp)



#load('temp_workspace_precip.RData')
station.repl = data.frame(cbind(unique(mod.data$Station),1:length(unique(mod.data$Station))))
colnames(station.repl) = c('Station','Station.Repl')
mod.data = join(mod.data,station.repl)

huc8.repl = data.frame(cbind(unique(mod.data$HUC8),1:length(unique(mod.data$HUC8))))
colnames(huc8.repl) = c('HUC8','HUC8.Repl')
mod.data = join(mod.data,huc8.repl)

#test = readOGR(dsn='government_units','state_nrcs_a_or')

INLA::inla.setOption(num.threads=8) 

#mod.data = all.params.spdf@data

mod.data$seasonal = mod.data$Abs.Month
mod.data$total.period = mod.data$Abs.Month
#mod.data$sq.owqi = ((as.numeric(as.character(mod.data$owqi)))^2)
#mod.data$l.owqi = log(as.numeric(as.character(mod.data$owqi)))
#mod.data = filter(mod.data,YEAR>=1995)
mod.data$HUC8 = as.character(mod.data$HUC8)



covars = mod.data[,c('Station','elevation','seaDist','HUC8','total.period','YEAR','uq.tid',
                     'ag.huc8','dev.huc8','wet.huc8','forst.huc8','owqi',
                     'owqi','monthly.precip.median',
                     'Decimal_Lat',
                     'Decimal_long',
                     'seasonal','Ag','Dev','Wetl','Forst','Station.Repl','HUC8.Repl',
                     grep('OWEB',names(mod.data),value=T))]

covars[is.na(covars)] = 0


#covars$OWEB_HUC8_Grant_Capacity_PriorTo12 = #covars$OWEB_HUC8_Grant_Capacity_All_WC - #covars$OWEB_HUC8_Grant_Capacity.WC_12
#covars$OWEB_HUC8_Grant_Capacity_PriorTo36 = #covars$OWEB_HUC8_Grant_Capacity_All_WC - #covars$OWEB_HUC8_Grant_Capacity.WC_36
#covars$OWEB_HUC8_Grant_Capacity_PriorTo60 = #covars$OWEB_HUC8_Grant_Capacity_All_WC - #covars$OWEB_HUC8_Grant_Capacity_60_WC


covars = mutate(covars,OWEB_HUC8_Grant_All.WC_12 = OWEB_HUC8_Grant_Restoration.WC_12+
                  OWEB_HUC8_Grant_Capacity.WC_12 +
                  OWEB_HUC8_Grant_Tech.WC_12 +
                  OWEB_HUC8_Grant_Outreach.WC_12,
                OWEB_HUC8_Grant_All.WC_24 = OWEB_HUC8_Grant_Restoration.WC_24+
                  OWEB_HUC8_Grant_Capacity.WC_24 +
                  OWEB_HUC8_Grant_Tech.WC_24 +
                  OWEB_HUC8_Grant_Outreach.WC_24,
                OWEB_HUC8_Grant_All.WC_36 = OWEB_HUC8_Grant_Restoration.WC_36+
                  OWEB_HUC8_Grant_Capacity.WC_36 +
                  OWEB_HUC8_Grant_Tech.WC_36 +
                  OWEB_HUC8_Grant_Outreach.WC_36,
#                 OWEB_HUC8_Grant_All_48_WC = OWEB_HUC8_Grant_Restoration_48_WC+
#                   OWEB_HUC8_Grant_Capacity_48_WC +
#                   OWEB_HUC8_Grant_Tech_48_WC +
#                   OWEB_HUC8_Grant_Outreach_48_WC,
#                 OWEB_HUC8_Grant_All_60_WC = OWEB_HUC8_Grant_Restoration_60_WC+
#                   OWEB_HUC8_Grant_Capacity_60_WC +
#                   OWEB_HUC8_Grant_Tech_60_WC +
#                   OWEB_HUC8_Grant_Outreach_60_WC,
#                 OWEB_HUC8_Grant_All_All_WC = OWEB_HUC8_Grant_Restoration_All_WC+
#                   OWEB_HUC8_Grant_Capacity_All_WC +
#                   OWEB_HUC8_Grant_Tech_All_WC +
#                   OWEB_HUC8_Grant_Outreach_All_WC,

                OWEB_HUC8_Grant_All.SWCD_12 = OWEB_HUC8_Grant_Restoration.SWCD_12+
                  OWEB_HUC8_Grant_Capacity.SWCD_12 +
                  OWEB_HUC8_Grant_Tech.SWCD_12 +
                  OWEB_HUC8_Grant_Outreach.SWCD_12,
                
                OWEB_HUC8_Grant_All.SWCD_24 = OWEB_HUC8_Grant_Restoration.SWCD_24+
                  OWEB_HUC8_Grant_Capacity.SWCD_24 +
                  OWEB_HUC8_Grant_Tech.SWCD_24 +
                  OWEB_HUC8_Grant_Outreach.SWCD_24,
                OWEB_HUC8_Grant_All.SWCD_36 = OWEB_HUC8_Grant_Restoration.SWCD_36+
                  OWEB_HUC8_Grant_Capacity.SWCD_36 +
                  OWEB_HUC8_Grant_Tech.SWCD_36 +
                  OWEB_HUC8_Grant_Outreach.SWCD_36,

OWEB_HUC8_Grant_Tech.Both_12 = OWEB_HUC8_Grant_Tech.SWCD_12 + OWEB_HUC8_Grant_Tech.WC_12,
OWEB_HUC8_Grant_Restoration.Both_12 = OWEB_HUC8_Grant_Restoration.SWCD_12 + OWEB_HUC8_Grant_Restoration.WC_12,
OWEB_HUC8_Grant_Outreach.Both_12 = OWEB_HUC8_Grant_Outreach.SWCD_12 + OWEB_HUC8_Grant_Outreach.WC_12,
OWEB_HUC8_Grant_Capacity.Both_12 = OWEB_HUC8_Grant_Capacity.SWCD_12 + OWEB_HUC8_Grant_Capacity.WC_12,
OWEB_HUC8_Grant_All.Both_12 = OWEB_HUC8_Grant_All.WC_12 + OWEB_HUC8_Grant_All.SWCD_12,



OWEB_HUC8_Grant_Tech.Both_24 = OWEB_HUC8_Grant_Tech.SWCD_24 + OWEB_HUC8_Grant_Tech.WC_24,
OWEB_HUC8_Grant_Restoration.Both_24 = OWEB_HUC8_Grant_Restoration.SWCD_24 + OWEB_HUC8_Grant_Restoration.WC_24,
OWEB_HUC8_Grant_Outreach.Both_24 = OWEB_HUC8_Grant_Outreach.SWCD_24 + OWEB_HUC8_Grant_Outreach.WC_24,
OWEB_HUC8_Grant_Capacity.Both_24 = OWEB_HUC8_Grant_Capacity.SWCD_24 + OWEB_HUC8_Grant_Capacity.WC_24,
OWEB_HUC8_Grant_All.Both_24 = OWEB_HUC8_Grant_All.WC_24 + OWEB_HUC8_Grant_All.SWCD_24,

OWEB_HUC8_Grant_Tech.Both_36 = OWEB_HUC8_Grant_Tech.SWCD_36 + OWEB_HUC8_Grant_Tech.WC_36,
OWEB_HUC8_Grant_Restoration.Both_36 = OWEB_HUC8_Grant_Restoration.SWCD_36 + OWEB_HUC8_Grant_Restoration.WC_36,
OWEB_HUC8_Grant_Outreach.Both_36 = OWEB_HUC8_Grant_Outreach.SWCD_36 + OWEB_HUC8_Grant_Outreach.WC_36,
OWEB_HUC8_Grant_Capacity.Both_36 = OWEB_HUC8_Grant_Capacity.SWCD_36 + OWEB_HUC8_Grant_Capacity.WC_36,
OWEB_HUC8_Grant_All.Both_36 = OWEB_HUC8_Grant_All.WC_24 + OWEB_HUC8_Grant_All.SWCD_24


#                 OWEB_HUC8_Grant_All_48_SWCD = OWEB_HUC8_Grant_Restoration_48_SWCD+
#                   OWEB_HUC8_Grant_Capacity_48_SWCD +
#                   OWEB_HUC8_Grant_Tech_48_SWCD +
#                   OWEB_HUC8_Grant_Outreach_48_SWCD,
#                 OWEB_HUC8_Grant_All_60_SWCD = OWEB_HUC8_Grant_Restoration_60_SWCD+
#                   OWEB_HUC8_Grant_Capacity_60_SWCD +
#                   OWEB_HUC8_Grant_Tech_60_SWCD +
#                   OWEB_HUC8_Grant_Outreach_60_SWCD,
#                 OWEB_HUC8_Grant_All_All_SWCD = OWEB_HUC8_Grant_Restoration_All_SWCD+
#                   OWEB_HUC8_Grant_Capacity_All_SWCD +
#                   OWEB_HUC8_Grant_Tech_All_SWCD +
#                   OWEB_HUC8_Grant_Outreach_All_SWCD
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
covars$Decimal_Lat = covars$Decimal_Lat - mean(covars$Decimal_Lat)
covars$Decimal_long = covars$Decimal_long - mean(covars$Decimal_long)

# 
# covars[,c('elev100m','seaDist10km','ag.huc8','dev.huc8',
#           'forst.huc8','Ag','Dev','Forst','monthly.precip.median','owqi')] = 
#   log(covars[,c('elev100m','seaDist10km','ag.huc8','dev.huc8',
#             'forst.huc8','Ag','Dev','Forst','monthly.precip.median','owqi')] + 0.001)


binary.funding.indicator = data.frame(ifelse(covars[,grep('OWEB_HUC8',colnames(covars))] >0, 1,0))
colnames(binary.funding.indicator) = paste0(colnames(covars)[grep('OWEB_HUC8',colnames(covars))],'.ind')
covars = cbind(covars,binary.funding.indicator)

k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k

covars$OWEB_HUC8_Grant_All.Both_24.ind <-  as.integer((covars$OWEB_HUC8_Grant_All.WC_24.ind + covars$OWEB_HUC8_Grant_All.SWCD_24.ind)>0)
covars$OWEB_HUC8_Grant_All.Both_12.ind <-  as.integer((covars$OWEB_HUC8_Grant_All.WC_12.ind + covars$OWEB_HUC8_Grant_All.SWCD_12.ind)>0)
covars$OWEB_HUC8_Grant_All.Both_36.ind <-  as.integer((covars$OWEB_HUC8_Grant_All.WC_36.ind + covars$OWEB_HUC8_Grant_All.SWCD_36.ind)>0)

covars$OWEB_HUC8_Grant_Restoration.Both_24.ind <-  as.integer((covars$OWEB_HUC8_Grant_Restoration.WC_24.ind + covars$OWEB_HUC8_Grant_Restoration.SWCD_24.ind)>0)
covars$OWEB_HUC8_Grant_Restoration.Both_12.ind <-  as.integer((covars$OWEB_HUC8_Grant_Restoration.WC_12.ind + covars$OWEB_HUC8_Grant_Restoration.SWCD_12.ind)>0)
covars$OWEB_HUC8_Grant_Restoration.Both_36.ind <-  as.integer((covars$OWEB_HUC8_Grant_Restoration.WC_36.ind + covars$OWEB_HUC8_Grant_Restoration.SWCD_36.ind)>0)


covars$OWEB_HUC8_Grant_Tech.Both_24.ind <-  as.integer((covars$OWEB_HUC8_Grant_Tech.WC_24.ind + covars$OWEB_HUC8_Grant_Tech.SWCD_24.ind)>0)
covars$OWEB_HUC8_Grant_Tech.Both_12.ind <-  as.integer((covars$OWEB_HUC8_Grant_Tech.WC_12.ind + covars$OWEB_HUC8_Grant_Tech.SWCD_12.ind)>0)
covars$OWEB_HUC8_Grant_Tech.Both_36.ind <-  as.integer((covars$OWEB_HUC8_Grant_Tech.WC_36.ind + covars$OWEB_HUC8_Grant_Tech.SWCD_36.ind)>0)

covars$OWEB_HUC8_Grant_Outreach.Both_24.ind <-  as.integer((covars$OWEB_HUC8_Grant_Outreach.WC_24.ind + covars$OWEB_HUC8_Grant_Outreach.SWCD_24.ind)>0)
covars$OWEB_HUC8_Grant_Outreach.Both_12.ind <-  as.integer((covars$OWEB_HUC8_Grant_Outreach.WC_12.ind + covars$OWEB_HUC8_Grant_Outreach.SWCD_12.ind)>0)
covars$OWEB_HUC8_Grant_Outreach.Both_36.ind <-  as.integer((covars$OWEB_HUC8_Grant_Outreach.WC_36.ind + covars$OWEB_HUC8_Grant_Outreach.SWCD_36.ind)>0)

covars$OWEB_HUC8_Grant_Capacity.Both_24.ind <-  as.integer((covars$OWEB_HUC8_Grant_Capacity.WC_24.ind + covars$OWEB_HUC8_Grant_Capacity.SWCD_24.ind)>0)
covars$OWEB_HUC8_Grant_Capacity.Both_12.ind <-  as.integer((covars$OWEB_HUC8_Grant_Capacity.WC_12.ind + covars$OWEB_HUC8_Grant_Capacity.SWCD_12.ind)>0)
covars$OWEB_HUC8_Grant_Capacity.Both_36.ind <-  as.integer((covars$OWEB_HUC8_Grant_Capacity.WC_36.ind + covars$OWEB_HUC8_Grant_Capacity.SWCD_36.ind)>0)

#covars[,grep('OWEB',names(covars))] = log(covars[,grep('OWEB',names(covars))]+0.001)


covars[,intersect(grep('OWEB',names(covars)),grep('.ind',names(covars),invert = T))] <-
apply(covars[,intersect(grep('OWEB',names(covars)),grep('.ind',names(covars),invert = T))],2,scale,center=TRUE,scale=FALSE)

covars[colnames(covars) %in% c('Decimal_Lat','Decimal_long','ag.huc8','dev.huc8','forst.huc8','NOT_OWEB_OWRI.wq.TotalCash_24',
                               'monthly.precip.median')] <-
  apply(covars[colnames(covars) %in% c('Decimal_Lat','Decimal_long','ag.huc8','dev.huc8','forst.huc8','NOT_OWEB_OWRI.wq.TotalCash_24',
  'monthly.precip.median')],2,scale,center=TRUE,scale=FALSE)

# some book keeping
n.data = length(covars$owqi)

require(INLA)
require(fields)
require(abind)
library(maptools)
library(splancs)
library(rgdal);library(rgeos);library(ggplot2)
#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
border <- unionSpatialPolygons(oregon.huc8, rep(1,nrow(oregon.huc8)),threshold=11000)

oregon.huc8.points = fortify(border, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")
oregon.huc8.df = filter(oregon.huc8.df,hole==FALSE)


#or.bond = inla.nonconvex.hull(cbind(covars$DECIMAL_LONG,covars$DECIMAL_LAT),2,2)
# (mesh.a <- inla.mesh.2d(
#   cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
#   max.edge=c(40, 40),cut=.25,min.angle=c(26, 21)))$n
# plot(mesh.a)

mesh.a =
  inla.mesh.create.helper(points=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
                          points.domain=oregon.huc8.df[,c('long','lat')],
                          offset=c(1, 3),
                          max.edge=c(10,100),
                          min.angle=c(26, 21),
                          cutoff=.3,
                          plot.delay=NULL)



spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)
stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE

# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           covars$ag.huc8, covars$forst.huc8,
#           covars$dev.huc8,
#           covars$elev100m,
#           covars$monthly.precip.median,
#           covars$NOT_OWEB_OWRI.wq.TotalCash_12,
#           covars$OWEB_HUC8_Grant_All.WC_12,
#           covars$OWEB_HUC8_Grant_All.SWCD_12)
# 
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_base_12 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   # Ag + Forst + Dev  + 
#   ag.huc8+ forst.huc8 +dev.huc8 + 
#   elev100m + 
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_12 + 
#   OWEB_HUC8_Grant_All.WC_12+
#   OWEB_HUC8_Grant_All.SWCD_12+
#   OWEB_HUC8_Grant_All.WC_12:OWEB_HUC8_Grant_All.SWCD_12+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   #f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),
#     replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),
#     season.length=12)+ 
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_base_12 <- inla(form_base_12, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))
# 
# 
# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_12,
# 
# covars$OWEB_HUC8_Grant_Outreach.WC_12, 
# covars$OWEB_HUC8_Grant_Outreach.SWCD_12,
# 
# covars$OWEB_HUC8_Grant_Tech.WC_12, 
# covars$OWEB_HUC8_Grant_Tech.SWCD_12,
# 
# covars$OWEB_HUC8_Grant_Restoration.WC_12, 
# covars$OWEB_HUC8_Grant_Restoration.SWCD_12
# )
# 
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_project_12 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_12 + 
#   
#   OWEB_HUC8_Grant_Outreach.WC_12+
#   OWEB_HUC8_Grant_Outreach.SWCD_12 + 
#   OWEB_HUC8_Grant_Tech.WC_12+
#   OWEB_HUC8_Grant_Tech.SWCD_12 + 
# 
#   OWEB_HUC8_Grant_Restoration.WC_12 +
#   OWEB_HUC8_Grant_Restoration.SWCD_12 + 
#   OWEB_HUC8_Grant_Restoration.WC_12:OWEB_HUC8_Grant_Restoration.SWCD_12 + 
#   OWEB_HUC8_Grant_Outreach.WC_12:OWEB_HUC8_Grant_Tech.WC_12:OWEB_HUC8_Grant_Restoration.WC_12:
#   OWEB_HUC8_Grant_Outreach.SWCD_12:OWEB_HUC8_Grant_Tech.SWCD_12:OWEB_HUC8_Grant_Restoration.SWCD_12 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),   
#     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# 
# mod_project_12 <- inla(form_project_12, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept),
#                        verbose=T,
#                        control.inla = list(
#                          correct = TRUE,
#                          correct.factor = correctionfactor))
# 
# 
# 
# # put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_12,
# covars$OWEB_HUC8_Grant_Outreach.WC_12, 
# covars$OWEB_HUC8_Grant_Tech.WC_12,
# covars$OWEB_HUC8_Grant_Restoration.WC_12,
# covars$OWEB_HUC8_Grant_Capacity.WC_12)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_capacity_12 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_12+
#   OWEB_HUC8_Grant_Outreach.WC_12+
#   OWEB_HUC8_Grant_Tech.WC_12+
#   OWEB_HUC8_Grant_Restoration.WC_12+
#   OWEB_HUC8_Grant_Capacity.WC_12 + 
#   OWEB_HUC8_Grant_Outreach.WC_12:OWEB_HUC8_Grant_Capacity.WC_12+
#   OWEB_HUC8_Grant_Tech.WC_12:OWEB_HUC8_Grant_Capacity.WC_12+
#   OWEB_HUC8_Grant_Restoration.WC_12:OWEB_HUC8_Grant_Capacity.WC_12+
#   OWEB_HUC8_Grant_Capacity.WC_12:OWEB_HUC8_Grant_Outreach.WC_12:OWEB_HUC8_Grant_Tech.WC_12:OWEB_HUC8_Grant_Capacity.WC_12:OWEB_HUC8_Grant_Restoration.WC_12+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_capacity_12 <- inla(form_capacity_12, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept),
#                         verbose=T, control.inla = list(correct = TRUE, correct.factor = correctionfactor))
# 
# 
# sendmail('tyler.andrew.scott@gmail.com','12 models done',' 12 mods')
# 


####### 24 month ###########


X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
          #covars$Ag,
          #covars$Forst,
          #covars$Dev,  
          covars$ag.huc8, 
          covars$forst.huc8,
          covars$dev.huc8,
          covars$elev100m,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_24,
          covars$OWEB_HUC8_Grant_All.WC_24.ind,
          covars$OWEB_HUC8_Grant_All.SWCD_24.ind,
          covars$OWEB_HUC8_Grant_All.WC_24,
          covars$OWEB_HUC8_Grant_All.SWCD_24,
          covars$OWEB_HUC8_Grant_all.WC24* covars$OWEB_HUC8_Grant_All.SWCD_24)
n.covariates = ncol(X)
Q = qr.Q(qr(X))


form_base_24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
  #Ag + Forst + Dev  + 
  ag.huc8+
  forst.huc8 +dev.huc8 +  elev100m + monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_24 + 
  OWEB_HUC8_Grant_All.SWCD_24.ind+
  OWEB_HUC8_Grant_All.WC_24.ind+
 # OWEB_HUC8_Grant_All.WC_24.ind+
  #OWEB_HUC8_Grant_All.SWCD_24.ind+
  OWEB_HUC8_Grant_All.WC_24+
  OWEB_HUC8_Grant_All.SWCD_24+
  OWEB_HUC8_Grant_All.WC_24:OWEB_HUC8_Grant_All.SWCD_24+
#OWEB_HUC8_Grant_All.WC_24.ind:OWEB_HUC8_Grant_All.WC_24+
#OWEB_HUC8_Grant_All.SWCD_24.ind:OWEB_HUC8_Grant_All.SWCD_24+
#OWEB_HUC8_Grant_All.Both_24.ind:OWEB_HUC8_Grant_All.Both_24+
  f(uq.tid,model='iid',param=c(0.001,0.001))+ 
  #f(YEAR,model='iid',param=c(0.001,0.001))+ 
  # f(total.period,model='rw2') + 
  f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))

mod_base_24 <- inla(form_base_24, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

    
    
X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
          #covars$Ag, #covars$Forst,
          #covars$Dev, 
          covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
           covars$elev100m,
          covars$monthly.precip.median, 
          covars$NOT_OWEB_OWRI.wq.TotalCash_24,
          covars$OWEB_HUC8_Grant_All.Both_24.ind,
covars$OWEB_HUC8_Grant_Outreach.WC_24, 
covars$OWEB_HUC8_Grant_Outreach.SWCD_24,
covars$OWEB_HUC8_Grant_Tech.WC_24, 
covars$OWEB_HUC8_Grant_Tech.SWCD_24,
covars$OWEB_HUC8_Grant_Restoration.WC_24,
covars$OWEB_HUC8_Grant_Restoration.SWCD_24,
covars$OWEB_HUC8_Grant_Outreach.WC_24*
covars$OWEB_HUC8_Grant_Outreach.SWCD_24*
covars$OWEB_HUC8_Grant_Tech.WC_24*
covars$OWEB_HUC8_Grant_Tech.SWCD_24*
covars$OWEB_HUC8_Grant_Restoration.WC_24*
covars$OWEB_HUC8_Grant_Restoration.SWCD_24
)

n.covariates = ncol(X)
Q = qr.Q(qr(X))

form_project_24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
  ag.huc8+
  forst.huc8 + dev.huc8 + elev100m +  
  monthly.precip.median + 
  NOT_OWEB_OWRI.wq.TotalCash_24 + 
  OWEB_HUC8_Grant_All.Both_24.ind + 
  OWEB_HUC8_Grant_Outreach.WC_24+
  OWEB_HUC8_Grant_Outreach.SWCD_24 + 
  #OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Outreach.SWCD_24 + 
  OWEB_HUC8_Grant_Tech.WC_24+
  OWEB_HUC8_Grant_Tech.SWCD_24 + 
  #OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Tech.SWCD_24 + 
  OWEB_HUC8_Grant_Restoration.WC_24 +
  OWEB_HUC8_Grant_Restoration.SWCD_24 + 
  OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Restoration.WC_24:
  OWEB_HUC8_Grant_Outreach.SWCD_24:OWEB_HUC8_Grant_Tech.SWCD_24:OWEB_HUC8_Grant_Restoration.SWCD_24 + 
  f(HUC8,model='iid',param=c(0.001,0.001))+ 
 # f(YEAR,model='iid',param=c(0.001,0.001))+ 
  # f(total.period,model='rw2') + 
  f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
  f(s, model=spde.a,
    extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))


mod_project_24 <- inla(form_project_24, family='gaussian', data=inla.stack.data(stk.1),
                       control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                       #  control.inla=list(strategy='laplace'), 
                       control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                       control.fixed= list(prec.intercept = pintercept),
                       verbose=T,
                       control.inla = list(
                         correct = TRUE,
                         correct.factor = correctionfactor))

# 
# # put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_24,
# covars$OWEB_HUC8_Grant_Outreach.WC_24, 
# covars$OWEB_HUC8_Grant_Tech.WC_24,
# covars$OWEB_HUC8_Grant_Restoration.WC_24,
# covars$OWEB_HUC8_Grant_Capacity.WC_24)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_capacity_24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_24+
#   OWEB_HUC8_Grant_Outreach.WC_24+
#   OWEB_HUC8_Grant_Tech.WC_24+
#   OWEB_HUC8_Grant_Restoration.WC_24+
#   OWEB_HUC8_Grant_Capacity.WC_24 + 
#   OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Capacity.WC_24+
#   OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Capacity.WC_24+
#   OWEB_HUC8_Grant_Restoration.WC_24:OWEB_HUC8_Grant_Capacity.WC_24+
#   OWEB_HUC8_Grant_Capacity.WC_24:OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Capacity.WC_24:OWEB_HUC8_Grant_Restoration.WC_24+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#  # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_capacity_24 <- inla(form_capacity_24, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept),
#                         verbose=T, control.inla = list(correct = TRUE, correct.factor = correctionfactor))
# 
# sendmail('tyler.andrew.scott@gmail.com','24 models done',' 24 mods')

save.image('test.results.24m.RData')
######### 36 Month #########
# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag,
#           #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,
#           covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_36,
# covars$OWEB_HUC8_Grant_All.WC_36,
# covars$OWEB_HUC8_Grant_All.SWCD_36
# )
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form_base_36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 +dev.huc8 +  elev100m + monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_36 + 
#   OWEB_HUC8_Grant_All.WC_36+
#   OWEB_HUC8_Grant_All.SWCD_36+
#   OWEB_HUC8_Grant_All.WC_36:OWEB_HUC8_Grant_All.SWCD_36+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod_base_36 <- inla(form_base_36, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))
# 
# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_36,
# 
# covars$OWEB_HUC8_Grant_Outreach.WC_36, 
# covars$OWEB_HUC8_Grant_Outreach.SWCD_36,
# 
# covars$OWEB_HUC8_Grant_Tech.WC_36, 
# covars$OWEB_HUC8_Grant_Tech.SWCD_36,
# 
# covars$OWEB_HUC8_Grant_Restoration.WC_36, 
# covars$OWEB_HUC8_Grant_Restoration.SWCD_36
# )
# 
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_project_36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m + 
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_36 + 
#   
#   OWEB_HUC8_Grant_Outreach.WC_36+
#   OWEB_HUC8_Grant_Outreach.SWCD_36 + 
#   OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Outreach.SWCD_36 + 
#   OWEB_HUC8_Grant_Tech.WC_36+
#   OWEB_HUC8_Grant_Tech.SWCD_36 + 
#   OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Tech.SWCD_36 + 
#   OWEB_HUC8_Grant_Restoration.WC_36 +
#   OWEB_HUC8_Grant_Restoration.SWCD_36 + 
#   OWEB_HUC8_Grant_Restoration.WC_36:OWEB_HUC8_Grant_Restoration.SWCD_36 + 
#   OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Restoration.WC_36:
#   OWEB_HUC8_Grant_Outreach.SWCD_36:OWEB_HUC8_Grant_Tech.SWCD_36:OWEB_HUC8_Grant_Restoration.SWCD_36 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_project_36 <- inla(form_project_36, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept),
#                        verbose=T,
#                        control.inla = list(
#                          correct = TRUE,
#                          correct.factor = correctionfactor))
# 
# 
# 
# # put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT_OWEB_OWRI.wq.TotalCash_36,
# covars$OWEB_HUC8_Grant_Outreach.WC_36, 
# covars$OWEB_HUC8_Grant_Tech.WC_36,
# covars$OWEB_HUC8_Grant_Restoration.WC_36,
# covars$OWEB_HUC8_Grant_Capacity.WC_36)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form_capacity_36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT_OWEB_OWRI.wq.TotalCash_36+
#   OWEB_HUC8_Grant_Outreach.WC_36+
#   OWEB_HUC8_Grant_Tech.WC_36+
#   OWEB_HUC8_Grant_Restoration.WC_36+
#   OWEB_HUC8_Grant_Capacity.WC_36 + 
#   OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Capacity.WC_36+
#   OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Capacity.WC_36+
#   OWEB_HUC8_Grant_Restoration.WC_36:OWEB_HUC8_Grant_Capacity.WC_36+
#   OWEB_HUC8_Grant_Capacity.WC_36:OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Capacity.WC_36:OWEB_HUC8_Grant_Restoration.WC_36+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod_capacity_36 <- inla(form_capacity_36, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                       
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept),
#                         verbose=T, control.inla = list(correct = TRUE, correct.factor = correctionfactor))
# 
# sendmail('tyler.andrew.scott@gmail.com','36 models done','36 mods')
# 
# save.image('temp_results2.RData')
# # library(mail)
# # sendmail('tyler.andrew.scott@gmail.com','temp mod 1992 results available','models done')
# # rm(list=ls())
# # load('temp_results.RData')
# 
# 
# tempcoef12 = data.frame((mod_base_12$summary.fixed[-1,c(1,3,5)]))
# tempcoef24 = data.frame((mod_base_24$summary.fixed[-1,c(1,3,5)]))
# tempcoef36 = data.frame((mod_base_36$summary.fixed[-1,c(1,3,5)]))
# rownames(tempcoef12) = gsub('_12','',rownames(tempcoef12))
# rownames(tempcoef24) = gsub('_24','',rownames(tempcoef24))
# rownames(tempcoef36) = gsub('_36','',rownames(tempcoef36))
# tempcoef12$Covar = rownames(tempcoef12)
# tempcoef24$Covar = rownames(tempcoef24)
# tempcoef36$Covar = rownames(tempcoef36)
# tempcoef12$Lag = 'Past 12 months\' funding'
# tempcoef24$Lag = 'Past 24 months\' funding'
# tempcoef36$Lag = 'Past 36 months\' funding'
# tempcoef12$Order = nrow(tempcoef12):1
# tempcoef24$Order = nrow(tempcoef24):1
# tempcoef36$Order = nrow(tempcoef36):1
# 
# 
# plottemp = join_all(list(tempcoef12,tempcoef24,tempcoef36),type='full')
# 
# 
# name.vec = c( 'WC * SWCD',
#               'SWCD grants',
#               'WC grants',
#               'OWRI projects',
#               'Month precip.',
#               
#               'Elevation',
#               'Dev. in basin',
#               'For. in basin',
#               'Ag. in basin',
#               'Long','Lat')
# 
# 
# library(ggplot2)
# library(ggthemes)
# basecoefplot = ggplot(data=plottemp) + 
#   geom_segment(aes(x=X0.025quant,xend=X0.975quant,y=as.factor(Order),yend=as.factor(Order)),
#                lwd=3,lineend='round') + 
#   facet_wrap(~Lag) + 
#   scale_x_continuous(name='Posterior credible interval (0.025 to 0.975)')+
#   #                                     limits=c(-0.05,0.11))+ 
#   theme_bw() +
#   scale_y_discrete(name='',labels= name.vec) + 
#   theme(
#     axis.ticks = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title = element_text(size=14),
#     axis.text = element_text(size=12),
#     strip.text = element_text(size=14))  +
#   geom_vline(xintercept = 0,lty=2)
# 
# 
# ggsave('JPART_Submission/Version2/basecoefplot2.png',basecoefplot)
# 
# 
# 
# tempcoef12$ID = rownames(tempcoef12)
# tempcoef12$Model = 'tempcoef12'
# tempcoef24$ID = rownames(tempcoef24)
# tempcoef24$Model = 'tempcoef24'
# tempcoef36$ID = rownames(tempcoef36)
# tempcoef36$Model = 'tempcoef36'
# # tempcoef48$ID = rownames(tempcoef48)
# # tempcoef48$Model = 'tempcoef48'
# # tempcoef60$ID = rownames(tempcoef60)
# # tempcoef60$Model = 'tempcoef60'
# # tempcoefAll$ID = rownames(tempcoefAll)
# # tempcoefAll$Model = 'tempcoefAll'
# 
# 
# rowname.vector = c(
#   #   "$\\%$  Agric. (100m buffer)",
#   #   '$\\%$  Forest (100m buffer)',
#   #   '$\\%$  Devel. (100m buffer)',
#   '$\\%$  Devel. in HUC8',
#   "$\\%$  Agric. in HUC8",
#   '$\\%$  Forest in HUC8',
#   'Elevation (10m)',
#   
#   'Monthly precip. (10cm)',
#   'Non-OWEB Rest. (\\$100k)',
#   'OWEB funds to WC (\\$100k)',
#   'OWEB funds to SWCD (\\$100k)',
#   'WC * SWCD')
# 
# library(lme4)
# library(texreg)
# 
# 
# mod_base_tex_12 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef12[,1],
#   ci.low = tempcoef12[,2],
#   ci.up = tempcoef12[,3],
#   gof.names = 'DIC',
#   gof = mod_base_12$dic$dic)
# 
# 
# mod_base_tex_24 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef24[,1],
#   ci.low = tempcoef24[,2],
#   ci.up = tempcoef24[,3],
#   gof.names = 'DIC',
#   gof = mod_base_24$dic$dic)
# 
# 
# mod_base_tex_36 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef36[,1],
#   ci.low = tempcoef36[,2],
#   ci.up = tempcoef36[,3],
#   gof.names = 'DIC',
#   gof = mod_base_36$dic$dic)
# 
# texreg(l = list(mod_base_tex_12,
#                 mod_base_tex_24,
#                 mod_base_tex_36),
#        stars=numeric(0),ci.test = 0,digits = 3,
#        caption = "Baseline models with only OWEB funding to Watershed Councils", caption.above = TRUE, 
#        custom.model.names = c('Past 12 months\' funding','Past 24 months\' funding','Past 36 months\' funding'),
#        label = c('table:basemods'),
#        custom.note = "$^* 0$ outside the credible interval\\\
#        OWRI: Spending on non-OWEB Restoration projects as reported in Oregon Watershed Restoration Inventory database",
#        custom.coef.names = rev(name.vec),
#        file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/basemods2.tex')
# 
# 
# library(mail)
# 
# 
# 
# tempcoef12 = data.frame((mod_project_12$summary.fixed[-1,c(1,3,5)]))
# tempcoef24 = data.frame((mod_project_24$summary.fixed[-1,c(1,3,5)]))
# tempcoef36 = data.frame((mod_project_36$summary.fixed[-1,c(1,3,5)]))
# rownames(tempcoef12) = gsub('_12','',rownames(tempcoef12))
# rownames(tempcoef24) = gsub('_24','',rownames(tempcoef24))
# rownames(tempcoef36) = gsub('_36','',rownames(tempcoef36))
# tempcoef12$Covar = rownames(tempcoef12)
# tempcoef24$Covar = rownames(tempcoef24)
# tempcoef36$Covar = rownames(tempcoef36)
# tempcoef12$Lag = 'Past 12 months\' funding'
# tempcoef24$Lag = 'Past 24 months\' funding'
# tempcoef36$Lag = 'Past 36 months\' funding'
# tempcoef12$Order = nrow(tempcoef12):1
# tempcoef24$Order = nrow(tempcoef24):1
# tempcoef36$Order = nrow(tempcoef36):1
# 
# #plottemp = rbind(tempcoef12,tempcoef24,tempcoef36)
# 
# plottemp = join_all(list(tempcoef12,tempcoef24,tempcoef36),type='full')
# 
# 
# name.vec = c( 'WC * SWCD All',
#               'WC * SWCD Rest.',
#               'WC * SWCD Tech',
#               'WC * SWCD Outreach',
#               'SWCD Rest.',
#               'WC Rest.',
#               'SWCD Tech.',
#               'WC Tech.',
#               'SWCD Outreach',
#               'WC Outreach',
#               'OWRI projects',
#               'Month precip.',
#               'Elevation',
#               'Dev. in basin',
#               'For. in basin',
#               'Ag. in basin',
#               'Long.','Lat')
# 
# 
# library(ggplot2)
# library(ggthemes)
# projectcoefplot = ggplot(data=plottemp) + 
#   geom_segment(aes(x=X0.025quant,xend=X0.975quant,y=as.factor(Order),yend=as.factor(Order)),
#                lwd=3,lineend='round') + 
#   facet_wrap(~Lag) + 
#   scale_x_continuous(name='Posterior credible interval (0.025 to 0.975)')+
#   #                                     limits=c(-0.05,0.11))+ 
#   theme_bw() +
#   scale_y_discrete(name='',labels= name.vec) + 
#   theme(
#     axis.ticks = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title = element_text(size=14),
#     axis.text = element_text(size=12),
#     strip.text = element_text(size=14))  +
#   geom_vline(xintercept = 0,lty=2)
# 
# ggsave('JPART_Submission/Version2/projectcoefplot2.png',projectcoefplot)
# 
# 
# 
# tempcoef12$ID = rownames(tempcoef12)
# tempcoef12$Model = 'tempcoef12'
# tempcoef24$ID = rownames(tempcoef24)
# tempcoef24$Model = 'tempcoef24'
# tempcoef36$ID = rownames(tempcoef36)
# tempcoef36$Model = 'tempcoef36'
# # tempcoef48$ID = rownames(tempcoef48)
# # tempcoef48$Model = 'tempcoef48'
# # tempcoef60$ID = rownames(tempcoef60)
# # tempcoef60$Model = 'tempcoef60'
# # tempcoefAll$ID = rownames(tempcoefAll)
# # tempcoefAll$Model = 'tempcoefAll'
# 
# 
# rowname.vector = c(
#   'Lat','Long',
#   '$\\%$  Devel. in HUC8',
#   "$\\%$  Agric. in HUC8",
#   '$\\%$  Forest in HUC8',
#   'Elevation (10m)',
#   
#   'Monthly precip. (10cm)',
#   'Non-OWEB Rest. (\\$100k)',
#   'OWEB funds to WC (\\$100k)',
#   'OWEB funds to SWCD (\\$100k)',
#   'WC * SWCD')
# 
# library(lme4)
# library(texreg)
# 
# 
# mod_project_tex_12 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef12[,1],
#   ci.low = tempcoef12[,2],
#   ci.up = tempcoef12[,3],
#   gof.names = 'DIC',
#   gof = mod_project_12$dic$dic)
# 
# 
# mod_project_tex_24 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef24[,1],
#   ci.low = tempcoef24[,2],
#   ci.up = tempcoef24[,3],
#   gof.names = 'DIC',
#   gof = mod_project_24$dic$dic)
# 
# 
# mod_project_tex_36 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef36[,1],
#   ci.low = tempcoef36[,2],
#   ci.up = tempcoef36[,3],
#   gof.names = 'DIC',
#   gof = mod_project_36$dic$dic)
# 
# texreg(l = list(mod_project_tex_12,
#                 mod_project_tex_24,
#                 mod_project_tex_36),
#        stars=numeric(0),ci.test = 0,digits = 3,
#        caption = "projectline models with only OWEB funding to Watershed Councils", caption.above = TRUE, 
#        custom.model.names = c('Past 12 months\' funding','Past 24 months\' funding','Past 36 months\' funding'),
#        label = c('table:projectmods'),
#        custom.note = "$^* 0$ outside the credible interval\\\
#        OWRI: Spending on non-OWEB Restoration projects as reported in Oregon Watershed Restoration Inventory dataproject",
#        custom.coef.names = rev(name.vec),
#        file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/projectmods2.tex')
# 
# 
# ## Capacity ###
# 
# 
# 
# tempcoef12 = data.frame((mod_capacity_12$summary.fixed[-1,c(1,3,5)]))
# tempcoef24 = data.frame((mod_capacity_24$summary.fixed[-1,c(1,3,5)]))
# tempcoef36 = data.frame((mod_capacity_36$summary.fixed[-1,c(1,3,5)]))
# rownames(tempcoef12) = gsub('_12','',rownames(tempcoef12))
# rownames(tempcoef24) = gsub('_24','',rownames(tempcoef24))
# rownames(tempcoef36) = gsub('_36','',rownames(tempcoef36))
# tempcoef12$Covar = rownames(tempcoef12)
# tempcoef24$Covar = rownames(tempcoef24)
# tempcoef36$Covar = rownames(tempcoef36)
# tempcoef12$Lag = 'Past 12 months\' funding'
# tempcoef24$Lag = 'Past 24 months\' funding'
# tempcoef36$Lag = 'Past 36 months\' funding'
# tempcoef12$Order = nrow(tempcoef12):1
# tempcoef24$Order = nrow(tempcoef24):1
# tempcoef36$Order = nrow(tempcoef36):1
# 
# tempcoef12
# #plottemp = rbind(tempcoef12,tempcoef24,tempcoef36)
# 
# plottemp = join_all(list(tempcoef12,tempcoef24,tempcoef36),type='full')
# 
# name.vec = c( 'All projects',
#               'Rest. * Capacity',
#               'Tech. * Capacity',
#               'Outreach * Capacity',
#               'WC Capacity',
#               'WC Rest.',
#               'WC Tech.',
#               'WC Outreach',
#               'OWRI restoration',
#               'Month precip.',
#               
#               'Elevation',
#               'Dev. in basin',
#               'For. in basin',
#               'Ag. in basin',
#               'Long.','Lat')
# 
# 
# library(ggplot2)
# library(ggthemes)
# 
# capacitycoefplot = ggplot(data=plottemp) + 
#   geom_segment(aes(x=X0.025quant,xend=X0.975quant,y=as.factor(Order),yend=as.factor(Order)),
#                lwd=3,lineend='round') + 
#   facet_wrap(~Lag) + 
#   scale_x_continuous(name='Posterior credible interval (0.025 to 0.975)')+
#   #                                     limits=c(-0.05,0.11))+ 
#   theme_bw() +
#   scale_y_discrete(name='',labels= name.vec) + 
#   theme(
#     axis.ticks = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title = element_text(size=14),
#     axis.text = element_text(size=12),
#     strip.text = element_text(size=14))  +
#   geom_vline(xintercept = 0,lty=2)
# 
# ggsave('JPART_Submission/Version2/capacitycoefplot2.png',capacitycoefplot)
# 
# 
# 
# tempcoef12$ID = rownames(tempcoef12)
# tempcoef12$Model = 'tempcoef12'
# tempcoef24$ID = rownames(tempcoef24)
# tempcoef24$Model = 'tempcoef24'
# tempcoef36$ID = rownames(tempcoef36)
# tempcoef36$Model = 'tempcoef36'
# # tempcoef48$ID = rownames(tempcoef48)
# # tempcoef48$Model = 'tempcoef48'
# # tempcoef60$ID = rownames(tempcoef60)
# # tempcoef60$Model = 'tempcoef60'
# # tempcoefAll$ID = rownames(tempcoefAll)
# # tempcoefAll$Model = 'tempcoefAll'
# 
# 
# rowname.vector = c(
#   'Lat.','Long',
#   '$\\%$  Devel. in HUC8',
#   "$\\%$  Agric. in HUC8",
#   '$\\%$  Forest in HUC8',
#   'Elevation (10m)',
#   
#   'Monthly precip. (10cm)',
#   'Non-OWEB Rest. (\\$100k)',
#   'OWEB funds to WC (\\$100k)',
#   'OWEB funds to SWCD (\\$100k)',
#   'WC * SWCD')
# 
# library(lme4)
# library(texreg)
# 
# 
# mod_capacity_tex_12 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef12[,1],
#   ci.low = tempcoef12[,2],
#   ci.up = tempcoef12[,3],
#   gof.names = 'DIC',
#   gof = mod_capacity_12$dic$dic)
# 
# 
# mod_capacity_tex_24 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef24[,1],
#   ci.low = tempcoef24[,2],
#   ci.up = tempcoef24[,3],
#   gof.names = 'DIC',
#   gof = mod_capacity_24$dic$dic)
# 
# 
# mod_capacity_tex_36 = texreg::createTexreg(
#   coef.names = rev(name.vec),
#   coef = tempcoef36[,1],
#   ci.low = tempcoef36[,2],
#   ci.up = tempcoef36[,3],
#   gof.names = 'DIC',
#   gof = mod_capacity_36$dic$dic)
# 
# texreg(l = list(mod_capacity_tex_12,
#                 mod_capacity_tex_24,
#                 mod_capacity_tex_36),
#        stars=numeric(0),ci.test = 0,digits = 3,
#        caption = "capacityline models with only OWEB funding to Watershed Councils", caption.above = TRUE, 
#        custom.model.names = c('Past 12 months\' funding','Past 24 months\' funding','Past 36 months\' funding'),
#        label = c('table:capacitymods'),
#        custom.note = "$^* 0$ outside the credible interval\\\
#        OWRI: Spending on non-OWEB Restoration capacitys as reported in Oregon Watershed Restoration Inventory datacapacity",
#        custom.coef.names = rev(name.vec),
#        file='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/capacitymods2.tex')

library(mail)
sendmail('tyler.andrew.scott@gmail.com','INLA model done')
