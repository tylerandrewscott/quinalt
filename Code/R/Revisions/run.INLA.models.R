rm(list=ls())

run.owqi.only = TRUE

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
mod.data = read.csv('../../../Input/update_data.csv')

for (i in 1:nrow(mod.data))
{
mod.data$hist.avg.owqi[i] = 
  ifelse(is.na(mean(mod.data$owqi[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$owqi[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$owqi[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
}


if(!run.owqi.only)
{
for (i in 1:nrow(mod.data))
{
mod.data$hist.avg.temp_si[i] = 
  ifelse(is.na(mean(mod.data$temp_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$temp_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$temp_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
  
mod.data$hist.avg.bod_si[i] = 
  ifelse(is.na(mean(mod.data$bod_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$bod_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$bod_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.ph_si[i] = 
  ifelse(is.na(mean(mod.data$ph_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$ph_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$ph_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.p_si[i] = 
  ifelse(is.na(mean(mod.data$p_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$p_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$p_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.n_si[i] =   
  ifelse(is.na(mean(mod.data$n_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
                                     mean(mod.data$n_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
                                     mean(mod.data$n_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.ts_si[i] = 
  ifelse(is.na(mean(mod.data$ts_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$ts_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$ts_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.do_si[i] = 
  ifelse(is.na(mean(mod.data$do_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$do_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$do_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.ecoli_si[i] = 
  ifelse(is.na(mean(mod.data$ecoli_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$ecoli_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$ecoli_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.fec_si[i] = 
  ifelse(is.na(mean(mod.data$fec_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$fec_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$fec_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))

mod.data$hist.avg.bact_si[i] = 
  ifelse(is.na(mean(mod.data$bact_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
         mean(mod.data$bact_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
         mean(mod.data$bact_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
}
}


mod.data = mod.data %>% mutate(which.wc = as.character(which.wc))

mod.data$which.wc[grep('Middle Deschutes',mod.data$which.wc)] <- 'Willow Creek WC'

library(mosaic)
council.dat = fetchGoogle('https://docs.google.com/spreadsheets/d/1OXdC54OK8BwXAbIzMDudAKMvdblMhQQBJZ82_LUijFc/pub?output=csv')


council.dat <- dplyr::select(council.dat,altName,OPERATING.BUDGET,TOTAL.BUDGET,YEAR.FOUNDED,COORD.TYPE,STAFF.FTE,Federal.Support,Foundation.Support,Donors.Support,Membership.Support)

council.dat$OPERATING.BUDGET = ordered(council.dat$OPERATING.BUDGET,c('$0 - $50,000','$50,001 - $100,000','$100,001 - $150,000',
                                                                      '$150,001 - $200,000', '$200,001 - $400,000',
                                                                      '$400,001 - $600,000','$600,001 - $800,000',
                                                                      '$1,000,001 - $5,000,000'
))

council.dat$TOTAL.BUDGET = ordered(council.dat$TOTAL.BUDGET,c('$0 - $100,000', '$100,001 - $250,000', '$250,001 - $500,000',
                                                              '$500,001 - $1,000,000', '$1,000,001 - $5,000,000'))


mod.data = left_join(mod.data,council.dat,by=c('which.wc' = 'altName'))

mod.data$OWRI.proj.in.past.3yr <- mod.data$OWRI.proj.in.p1.wy + mod.data$OWRI.proj.in.p2.wy + mod.data$OWRI.proj.in.p3.wy
mod.data$OWRI.proj.in.past.2yr <- mod.data$OWRI.proj.in.p1.wy + mod.data$OWRI.proj.in.p2.wy
mod.data$OWRI.proj.in.past.1yr <- mod.data$OWRI.proj.in.p1.wy 

mod.data$YEARS.ACTIVE <- NA
mod.data$YEARS.ACTIVE[is.na(mod.data$which.wc)] <- 0
mod.data$YEARS.ACTIVE[!is.na(mod.data$YEAR.FOUNDED)] <- mod.data$YEAR[!is.na(mod.data$YEAR.FOUNDED)] - mod.data$YEAR.FOUNDED[!is.na(mod.data$YEAR.FOUNDED)]
mod.data$YEARS.ACTIVE[mod.data$YEARS.ACTIVE<0] <- 0 

mod.data[,grep('OWEB.',colnames(mod.data))][is.na(mod.data[,grep('OWEB.',colnames(mod.data))])] <- 0

mod.data = filter(mod.data, Water.Year >=2012)


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

#mod.data[,grep('proj.in|OWEB.p',names(mod.data))] <- log(mod.data[,grep('proj.in|OWEB.p',names(mod.data))]+0.01)
k = 100000
mod.data[,grep('proj.in|OWEB.p',names(mod.data))] <- mod.data[,grep('proj.in|OWEB.p',names(mod.data))] / k


mod.data <- mod.data %>% mutate(OWEB.proj.in.last.3yr.WC  = OWEB.p1.WC + OWEB.p2.WC + OWEB.p3.WC,
                              
                                OWEB.proj.in.last.3yr.SWCD  = OWEB.p1.SWCD + OWEB.p2.SWCD + OWEB.p3.SWCD,
                                
                                
  OWEB.proj.in.last.3yr.WC.Tech = OWEB.p1.WC.Tech + OWEB.p2.WC.Tech + OWEB.p3.WC.Tech,
  OWEB.proj.in.last.3yr.WC.Capacity = OWEB.p1.WC.Capacity + OWEB.p2.WC.Capacity + OWEB.p3.WC.Capacity,
  OWEB.proj.in.last.3yr.WC.Restoration = OWEB.p1.WC.Restoration + OWEB.p2.WC.Restoration + OWEB.p3.WC.Restoration,
  OWEB.proj.in.last.3yr.WC.Outreach = OWEB.p1.WC.Outreach + OWEB.p2.WC.Outreach + OWEB.p3.WC.Outreach,
  OWEB.proj.in.last.3yr.SWCD.Tech = OWEB.p1.SWCD.Tech + OWEB.p2.SWCD.Tech + OWEB.p3.SWCD.Tech,
  OWEB.proj.in.last.3yr.SWCD.Capacity = OWEB.p1.SWCD.Capacity + OWEB.p2.SWCD.Capacity + OWEB.p3.SWCD.Capacity,
  OWEB.proj.in.last.3yr.SWCD.Restoration = OWEB.p1.SWCD.Restoration + OWEB.p2.SWCD.Restoration + OWEB.p3.SWCD.Restoration,
  OWEB.proj.in.last.3yr.SWCD.Outreach = OWEB.p1.SWCD.Outreach + OWEB.p2.SWCD.Outreach + OWEB.p3.SWCD.Outreach,
  
  OWEB.proj.in.last.2yr.WC  = OWEB.p1.WC + OWEB.p2.WC ,
  OWEB.proj.in.last.2yr.SWCD  = OWEB.p1.SWCD + OWEB.p2.SWCD,
  
  OWEB.proj.in.last.2yr.WC.Tech = OWEB.p1.WC.Tech + OWEB.p2.WC.Tech,
  OWEB.proj.in.last.2yr.WC.Capacity = OWEB.p1.WC.Capacity + OWEB.p2.WC.Capacity,
  OWEB.proj.in.last.2yr.WC.Restoration = OWEB.p1.WC.Restoration + OWEB.p2.WC.Restoration , 
  OWEB.proj.in.last.2yr.WC.Outreach = OWEB.p1.WC.Outreach + OWEB.p2.WC.Outreach, 
  OWEB.proj.in.last.2yr.SWCD.Tech = OWEB.p1.SWCD.Tech + OWEB.p2.SWCD.Tech,
  OWEB.proj.in.last.2yr.SWCD.Capacity = OWEB.p1.SWCD.Capacity + OWEB.p2.SWCD.Capacity,
  OWEB.proj.in.last.2yr.SWCD.Restoration = OWEB.p1.SWCD.Restoration + OWEB.p2.SWCD.Restoration , 
  OWEB.proj.in.last.2yr.SWCD.Outreach = OWEB.p1.SWCD.Outreach + OWEB.p2.SWCD.Outreach, 
  
  OWEB.proj.in.last.1yr.WC  = OWEB.p1.WC,
  OWEB.proj.in.last.1yr.SWCD  = OWEB.p1.SWCD ,
  OWEB.proj.in.last.1yr.WC.Tech = OWEB.p1.WC.Tech ,
  OWEB.proj.in.last.1yr.WC.Capacity = OWEB.p1.WC.Capacity ,
  OWEB.proj.in.last.1yr.WC.Restoration = OWEB.p1.WC.Restoration ,
  OWEB.proj.in.last.1yr.WC.Outreach = OWEB.p1.WC.Outreach ,
  OWEB.proj.in.last.1yr.SWCD.Tech = OWEB.p1.SWCD.Tech ,
  OWEB.proj.in.last.1yr.SWCD.Capacity = OWEB.p1.SWCD.Capacity ,
  OWEB.proj.in.last.1yr.SWCD.Restoration = OWEB.p1.SWCD.Restoration ,
  OWEB.proj.in.last.1yr.SWCD.Outreach = OWEB.p1.SWCD.Outreach)

  
mod.data$elev100m = mod.data$elevation/100
mod.data$seaDist10km = mod.data$seaDist/10
mod.data$ag.huc8 = 100 * mod.data$ag.huc8
mod.data$dev.huc8 = 100 * mod.data$dev.huc8
mod.data$forst.huc8 = 100 * mod.data$forst.huc8
#mod.data$Ag = 100 * mod.data$Ag
#mod.data$Forst = 100 * mod.data$Forst
#mod.data$Dev = 100 * mod.data$Dev
mod.data$monthly.precip.median = mod.data$monthly.precip.median/100
mod.data$monthly.precip.median = mod.data$monthly.precip.median  - mean(mod.data$monthly.precip.median)
mod.data$Decimal_Lat = mod.data$Decimal_Lat - mean(mod.data$Decimal_Lat)
mod.data$Decimal_long = mod.data$Decimal_long - mean(mod.data$Decimal_long)
mod.data$hist.avg.owqi = mod.data$hist.avg.owqi - mean(mod.data$hist.avg.owqi)
mod.data$forst.huc8 = mod.data$forst.huc8 - mean(mod.data$forst.huc8)
mod.data$ag.huc8 = mod.data$ag.huc8 - mean(mod.data$ag.huc8)
mod.data$dev.huc8 = mod.data$dev.huc8 - mean(mod.data$dev.huc8)
mod.data$elev100m = mod.data$elev100m - mean(mod.data$elev100m)



bad = c('Fair','Poor','Very Poor')

mod.data = mod.data %>% mutate(n_cond_good = ifelse(n_cond %in% bad,0,1),
                    p_cond_good = ifelse(p_cond %in% bad,0,1),
                    do_cond_good = ifelse(do_cond %in% bad,0,1),
                    bod_cond_good = ifelse(bod_cond %in% bad,0,1),
                    n_cond_good = ifelse(n_cond %in% bad,0,1),
                    temp_cond_good = ifelse(temp_cond %in% bad,0,1),
                    ts_cond_good = ifelse(ts_cond %in% bad,0,1),
                    bact_cond_good = ifelse(bact_cond %in% bad,0,1),
                    owqi_cond_good = ifelse(owqi_cond %in% bad,0,1)
                    )

mod.data$STAFF.FTE <- ifelse(mod.data$STAFF.FTE=='less than 1',0.5,mod.data$STAFF.FTE)
mod.data$STAFF.FTE <- as.numeric(mod.data$STAFF.FTE)
mod.data$STAFF.FTE[is.na(mod.data$STAFF.FTE)] = 0

mod.data$COORD.TYPE[mod.data$COORD.TYPE==''] <- NA


low.op = levels(mod.data$OPERATING.BUDGET)[1:4]

mod.data$OP.BUDGET.200k <- ifelse(mod.data$OPERATING.BUDGET %in% low.op,0,1)




#mod.data$OWRI.proj.in.past.1yr = mod.data$OWRI.proj.in.past.1yr - mean(mod.data$OWRI.proj.in.past.1yr)
#mod.data$OWRI.proj.in.past.2yr = mod.data$OWRI.proj.in.past.2yr - mean(mod.data$OWRI.proj.in.past.2yr)
#mod.data$OWRI.proj.in.past.3yr = mod.data$OWRI.proj.in.past.3yr - mean(mod.data$OWRI.proj.in.past.3yr)




mod.data = mod.data %>% mutate(OWEB.proj.in.last.3yr.WC.Interaction = OWEB.proj.in.last.3yr.WC.Tech*
  OWEB.proj.in.last.3yr.WC.Restoration*
  OWEB.proj.in.last.3yr.WC.Outreach*
  OWEB.proj.in.last.3yr.WC.Capacity,
  OWEB.proj.in.last.2yr.WC.Interaction = OWEB.proj.in.last.2yr.WC.Tech*
    OWEB.proj.in.last.2yr.WC.Restoration*
    OWEB.proj.in.last.2yr.WC.Outreach*
    OWEB.proj.in.last.2yr.WC.Capacity,
  OWEB.proj.in.last.1yr.WC.Interaction = OWEB.proj.in.last.1yr.WC.Tech*
    OWEB.proj.in.last.1yr.WC.Restoration*
    OWEB.proj.in.last.1yr.WC.Outreach*
    OWEB.proj.in.last.1yr.WC.Capacity
  )


temp = mod.data %>% dplyr::filter(!duplicated(which.wc)) %>% dplyr::select(OP.BUDGET.200k,STAFF.FTE,YEARS.ACTIVE)


#stargazer(temp,summary = FALSE)


covars = mod.data[,c('Station','elevation','seaDist','HUC8','total.period','YEAR','uq.tid',
                     'ag.huc8','dev.huc8','wet.huc8','forst.huc8','owqi',
                     'owqi','monthly.precip.median','YEARS.ACTIVE','TOTAL.BUDGET','OPERATING.BUDGET','OP.BUDGET.200k',
                     'STAFF.FTE','COORD.TYPE',
                     'which.wc',
                     'Decimal_Lat',
                     'Decimal_long','elev100m','seaDist10km',
                     'seasonal',
                    # 'Ag','Dev','Wetl','Forst',
                     'Station.Repl','HUC8.Repl',grep('proj.in|grants.in',colnames(mod.data),value=T),
                     grep('OWEB',names(mod.data),value=T),
                     grep('cond_good',names(mod.data),value=T),
                     grep('hist.avg',names(mod.data),value=T),
                     grep('_si',names(mod.data),value=T))]

dep.var.list = c(grep('_si',names(mod.data),value=T),'owqi')

# some book keeping
n.data = length(covars$owqi)

require(INLA)
require(fields)
require(abind)
library(maptools)
library(splancs)
library(rgdal);library(rgeos);library(ggplot2)
#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="../../../SpatialData/hydrologic_units", layer="wbdhu8_a_or")
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


#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE

length.of.season = 12


form.base.p1 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.1yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC + 
  OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC:STAFF.FTE + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
 # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p1)%*%A.1), e= rep(0,n.covariates.base.p1)))

form.base.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.2yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC + 
  OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC:STAFF.FTE + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p2)%*%A.1), e= rep(0,n.covariates.base.p2)))

form.base.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.3yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC + 
  OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC:STAFF.FTE +   
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p3)%*%A.1), e= rep(0,n.covariates.base.p3)))

form.swcd.p1 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.1yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC + 
  OWEB.proj.in.last.1yr.SWCD + 
  OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p1)%*%A.1), e= rep(0,n.covariates.base.p1)))

form.swcd.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.2yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC + 
  OWEB.proj.in.last.2yr.SWCD + 
  OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p2)%*%A.1), e= rep(0,n.covariates.base.p2)))

form.swcd.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.3yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC + 
  OWEB.proj.in.last.3yr.SWCD + 
  OWEB.proj.in.last.3yr.WC:OWEB.proj.in.last.3yr.SWCD + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p3)%*%A.1), e= rep(0,n.covariates.base.p3)))


form.project.p1 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
elev100m +  monthly.precip.median + OWRI.proj.in.past.1yr +
YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC.Tech +
  OWEB.proj.in.last.1yr.WC.Restoration +
  OWEB.proj.in.last.1yr.WC.Outreach +
  OWEB.proj.in.last.1yr.WC.Capacity +
  OWEB.proj.in.last.1yr.WC.Capacity:OWEB.proj.in.last.1yr.WC.Tech +
  OWEB.proj.in.last.1yr.WC.Capacity:OWEB.proj.in.last.1yr.WC.Restoration +
  OWEB.proj.in.last.1yr.WC.Capacity:OWEB.proj.in.last.1yr.WC.Outreach +
  OWEB.proj.in.last.1yr.WC.Interaction +
  f(total.period,model='iid',param=c(0.001,0.001)) +
f(HUC8,model='iid',param=c(0.001,0.001)) + 
# f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
 #  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
   f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.p1)%*%A.1), e= rep(0,n.covariates.project.p1)))

# 
form.project.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + 
  OWRI.proj.in.past.2yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC.Tech +
  OWEB.proj.in.last.2yr.WC.Restoration +
  OWEB.proj.in.last.2yr.WC.Outreach +
  OWEB.proj.in.last.2yr.WC.Capacity +
  OWEB.proj.in.last.2yr.WC.Capacity:OWEB.proj.in.last.2yr.WC.Tech +
  OWEB.proj.in.last.2yr.WC.Capacity:OWEB.proj.in.last.2yr.WC.Restoration +
  OWEB.proj.in.last.2yr.WC.Capacity:OWEB.proj.in.last.2yr.WC.Outreach +
  OWEB.proj.in.last.2yr.WC.Interaction +
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.p2)%*%A.1), e= rep(0,n.covariates.project.p2)))

# 
form.project.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + 
  OWRI.proj.in.past.3yr +
  YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC.Tech +
  OWEB.proj.in.last.3yr.WC.Restoration +
  OWEB.proj.in.last.3yr.WC.Outreach +
  OWEB.proj.in.last.3yr.WC.Capacity +
  OWEB.proj.in.last.3yr.WC.Capacity:OWEB.proj.in.last.3yr.WC.Tech +
  OWEB.proj.in.last.3yr.WC.Capacity:OWEB.proj.in.last.3yr.WC.Restoration +
  OWEB.proj.in.last.3yr.WC.Capacity:OWEB.proj.in.last.3yr.WC.Outreach +
  OWEB.proj.in.last.3yr.WC.Interaction +
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.p3)%*%A.1), e= rep(0,n.covariates.project.p3)))



X.base.p1 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,covars$hist.avg.owqi,
                  covars$monthly.precip.median,
                  covars$OWRI.proj.in.past.1yr)
                  #covars$YEARS.ACTIVE)
n.covariates.base.p1 = ncol(X.base.p1)
Q.base.p1 = qr.Q(qr(X.base.p1))


X.base.p2 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,covars$hist.avg.owqi,
                  covars$monthly.precip.median,
                  covars$OWRI.proj.in.past.2yr)
                  #covars$YEARS.ACTIVE)

n.covariates.base.p2 = ncol(X.base.p2)
Q.base.p2 = qr.Q(qr(X.base.p2))


X.base.p3 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,covars$hist.avg.owqi,
                  covars$monthly.precip.median,
                  covars$OWRI.proj.in.past.3yr)
                  #covars$YEARS.ACTIVE)

n.covariates.base.p3 = ncol(X.base.p3)
Q.base.p3 = qr.Q(qr(X.base.p3))
# 
# 

X.project.p1 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                      covars$OWRI.proj.in.past.1yr)
                      #covars$YEARS.ACTIVE)

n.covariates.project.p1 = ncol(X.project.p1)
Q.project.p1 = qr.Q(qr(X.project.p1))

# 
X.project.p2 <- cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8, covars$forst.huc8,
                      covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,
                      covars$monthly.precip.median,
                      covars$OWRI.proj.in.past.2yr)

n.covariates.project.p2 = ncol(X.project.p2)
Q.project.p2 = qr.Q(qr(X.project.p2))
# 
X.project.p3 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                      covars$OWRI.proj.in.past.3yr)
                      #covars$YEARS.ACTIVE)
n.covariates.project.p3 = ncol(X.project.p3)
Q.project.p3 = qr.Q(qr(X.project.p3))



X.swcd.p1 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.1yr)
#covars$YEARS.ACTIVE)

n.covariates.swcd.p1 = ncol(X.swcd.p1)
Q.swcd.p1 = qr.Q(qr(X.swcd.p1))

# 
X.swcd.p2 <- cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8, covars$forst.huc8,
                      covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,
                      covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.2yr)

n.covariates.swcd.p2 = ncol(X.swcd.p2)
Q.swcd.p2 = qr.Q(qr(X.swcd.p2))
# 
X.swcd.p3 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.3yr)
#covars$YEARS.ACTIVE)
n.covariates.swcd.p3 = ncol(X.swcd.p3)
Q.swcd.p3 = qr.Q(qr(X.swcd.p3))


(mesh.a <- inla.mesh.2d(
  cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
  max.edge=c(5, 40),cut=.25))$n

spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

empty.list = as.list(NULL)


###
if (!run.owqi.only)
{
for (i in dep.var.list)
{
stk.1 <- inla.stack(data=list(y=covars[,i]), A=list(A.1,1),
                    effects=list(ind.1, 
                                 list(data.frame(b0=1,covars[,colnames(covars)%in% dep.var.list==FALSE]))))

mod.base.p1 <- inla(form.base.p1, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))
library(tidyr)
temp = mod.base.p1$summary.fixed[,c('mean','0.025quant','0.975quant')]
temp$Variable = rownames(temp)
temp$Dep.Var = i
temp$Model = 'base.p1'
empty.list[[paste('p1',i)]] = temp

mod.base.p2 <- inla(form.base.p2, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

temp = mod.base.p2$summary.fixed[,c('mean','0.025quant','0.975quant')]
temp$Variable = rownames(temp)
temp$Dep.Var = i
temp$Model = 'base.p2'
empty.list[[paste('p2',i)]] = temp

mod.base.p3 <- inla(form.base.p3, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))
temp = mod.base.p3$summary.fixed[,c('mean','0.025quant','0.975quant')]
temp$Variable = rownames(temp)
temp$Dep.Var = i
temp$Model = 'base.p3'
empty.list[[paste('p3',i)]] = temp
}


  
base.coef.df = join_all(empty.list,type='full')
names(base.coef.df) = c("mean"  ,  "upper" ,"lower", "Variable"  , "Dep.Var"  ,  "Model" )

base.coef.df$Variable <- factor(base.coef.df$Variable)

}

if (run.owqi.only)
{
  stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
                      effects=list(ind.1, 
                                   list(data.frame(b0=1,covars[,colnames(covars)%in% dep.var.list==FALSE]))))
  
  mod.base.p1 <- inla(form.base.p1, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  
  
  mod.base.p2 <- inla(form.base.p2, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  mod.base.p3 <- inla(form.base.p3, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  mod.project.p1 <- inla(form.project.p1, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  mod.project.p2 <- inla(form.project.p2, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  
  
  mod.project.p3 <- inla(form.project.p3, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  
  
  mod.swcd.p1 <- inla(form.swcd.p1, family='gaussian',
                         data=inla.stack.data(stk.1),
                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                compute=TRUE),
                         #  control.inla=list(strategy='laplace'), 
                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                         verbose=T,
                         control.inla = list(
                           correct = TRUE,
                           correct.factor = correctionfactor))
  mod.swcd.p2 <- inla(form.swcd.p2, family='gaussian',
                         data=inla.stack.data(stk.1),
                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                compute=TRUE),
                         #  control.inla=list(strategy='laplace'), 
                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                         verbose=T,
                         control.inla = list(
                           correct = TRUE,
                           correct.factor = correctionfactor))
  
  
  mod.swcd.p3 <- inla(form.swcd.p3, family='gaussian',
                         data=inla.stack.data(stk.1),
                         control.predictor=list(A=inla.stack.A(stk.1), 
                                                compute=TRUE),
                         #  control.inla=list(strategy='laplace'), 
                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                         verbose=T,
                         control.inla = list(
                           correct = TRUE,
                           correct.factor = correctionfactor))
}




# mesh.a =
#   inla.mesh.create.helper(points=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
#                           points.domain=oregon.huc8.df[,c('long','lat')],
#                           offset=c(1, 3),
#                           max.edge=c(10,100),
#                           min.angle=c(26, 21),
#                           cutoff=.3,
#                           plot.delay=NULL)
# plot(mesh.a)


# spde.a <- inla.spde2.matern(mesh.a)
# 
# # Model 1: constant spatial effect
# A.1 <- inla.spde.make.A(mesh.a, 
#                         loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
# ind.1 <- inla.spde.make.index('s', mesh.a$n)
# 
# stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
#                     effects=list(ind.1, list(data.frame(b0=1,covars))))


# 
# mod.project.p1 <- inla(form.project.p1, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))
# 
# mod.project.p2 <- inla(form.project.p2, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))
# 
# mod.project.p3 <- inla(form.project.p3, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))


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
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_capacity_12 <- inla(form_capacity_12, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                         verbose=T, control.inla = list(correct = TRUE, correct.factor = correctionfactor))
# 
# 
# sendmail('tyler.andrew.scott@gmail.com','12 models done',' 12 mods')
# 


####### 24 month ###########

# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag,
#           #covars$Forst,
#           #covars$Dev,  
#           covars$ag.huc8, 
#           covars$forst.huc8,
#           covars$dev.huc8,
#           covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$YEARS.ACTIVE,
#           covars$NOT_OWEB_OWRI.wq.TotalCash_24,
#         #  covars$OWEB_HUC8_Grant_All.WC_24.ind,
#         #  covars$OWEB_HUC8_Grant_All.SWCD_24.ind,
#           covars$OWEB_HUC8_Grant_All.WC_24,
#           covars$OWEB_HUC8_Grant_All.SWCD_24,
#           covars$OWEB_HUC8_Grant_all.WC24* covars$OWEB_HUC8_Grant_All.SWCD_24)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form_base_24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 +dev.huc8 +  elev100m + monthly.precip.median + 
#   YEARS.ACTIVE +
#   NOT_OWEB_OWRI.wq.TotalCash_24 + 
#  # OWEB_HUC8_Grant_All.SWCD_24.ind+
# #  OWEB_HUC8_Grant_All.WC_24.ind+
#  # OWEB_HUC8_Grant_All.WC_24.ind+
#   #OWEB_HUC8_Grant_All.SWCD_24.ind+
#   OWEB_HUC8_Grant_All.WC_24+
#   OWEB_HUC8_Grant_All.SWCD_24+
#   OWEB_HUC8_Grant_All.WC_24:OWEB_HUC8_Grant_All.SWCD_24+
# #OWEB_HUC8_Grant_All.WC_24.ind:OWEB_HUC8_Grant_All.WC_24+
# #OWEB_HUC8_Grant_All.SWCD_24.ind:OWEB_HUC8_Grant_All.SWCD_24+
# #OWEB_HUC8_Grant_All.Both_24.ind:OWEB_HUC8_Grant_All.Both_24+
#   f(which.wc,model='iid',param=c(0.001,0.001))+
#   #f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod_base_24 <- inla(form_base_24, family='gaussian',
#                     data=inla.stack.data(stk.1),
#                     control.predictor=list(A=inla.stack.A(stk.1), 
#                                            compute=TRUE),
#                     #  control.inla=list(strategy='laplace'), 
#                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
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
#           covars$YEARS.ACTIVE,
#           covars$NOT_OWEB_OWRI.wq.TotalCash_24,
#           covars$OWEB_HUC8_Grant_All.Both_24.ind,
# covars$OWEB_HUC8_Grant_Outreach.WC_24, 
# covars$OWEB_HUC8_Grant_Outreach.SWCD_24,
# covars$OWEB_HUC8_Grant_Tech.WC_24, 
# covars$OWEB_HUC8_Grant_Tech.SWCD_24,
# covars$OWEB_HUC8_Grant_Restoration.WC_24,
# covars$OWEB_HUC8_Grant_Restoration.SWCD_24,
# covars$OWEB_HUC8_Grant_Outreach.WC_24*
# covars$OWEB_HUC8_Grant_Outreach.SWCD_24*
# covars$OWEB_HUC8_Grant_Tech.WC_24*
# covars$OWEB_HUC8_Grant_Tech.SWCD_24*
# covars$OWEB_HUC8_Grant_Restoration.WC_24*
# covars$OWEB_HUC8_Grant_Restoration.SWCD_24
# )
# 
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# 
# 
# 
# form_project_24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   YEARS.ACTIVE + 
#   NOT_OWEB_OWRI.wq.TotalCash_24 + 
#   OWEB_HUC8_Grant_Outreach.WC_24+
#   OWEB_HUC8_Grant_Outreach.SWCD_24 + 
#   #OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Outreach.SWCD_24 + 
#   OWEB_HUC8_Grant_Tech.WC_24+
#   OWEB_HUC8_Grant_Tech.SWCD_24 + 
#   #OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Tech.SWCD_24 + 
#   OWEB_HUC8_Grant_Restoration.WC_24 +
#   OWEB_HUC8_Grant_Restoration.SWCD_24 + 
#   OWEB_HUC8_Grant_Outreach.WC_24:OWEB_HUC8_Grant_Tech.WC_24:OWEB_HUC8_Grant_Restoration.WC_24:
#   OWEB_HUC8_Grant_Outreach.SWCD_24:OWEB_HUC8_Grant_Tech.SWCD_24:OWEB_HUC8_Grant_Restoration.SWCD_24 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#  # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_project_24 <- inla(form_project_24, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                        verbose=T,
#                        control.inla = list(
#                          correct = TRUE,
#                          correct.factor = correctionfactor))
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
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_capacity_24 <- inla(form_capacity_24, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                         verbose=T, control.inla = list(correct = TRUE, correct.factor = correctionfactor))
# 
# sendmail('tyler.andrew.scott@gmail.com','24 models done',' 24 mods')


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
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
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
#                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                     verbose=T,
#                     control.inla = list(
#                       correct = TRUE,
#                       correct.factor = correctionfactor))
# 
# 
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag,
#           #covars$Forst,
#           #covars$Dev,  
#           covars$ag.huc8, 
#           covars$forst.huc8,
#           covars$dev.huc8,
#           covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$YEARS.ACTIVE,
#           covars$NOT_OWEB_OWRI.wq.TotalCash_36,
#           #  covars$OWEB_HUC8_Grant_All.WC_36.ind,
#           #  covars$OWEB_HUC8_Grant_All.SWCD_36.ind,
#           covars$OWEB_HUC8_Grant_All.WC_36,
#           covars$OWEB_HUC8_Grant_All.SWCD_36,
#           covars$OWEB_HUC8_Grant_all.WC36* covars$OWEB_HUC8_Grant_All.SWCD_36)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form_project_36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   YEARS.ACTIVE + 
#   NOT_OWEB_OWRI.wq.TotalCash_36 + 
#   OWEB_HUC8_Grant_All.Both_36.ind + 
#   OWEB_HUC8_Grant_Outreach.WC_36+
#   OWEB_HUC8_Grant_Outreach.SWCD_36 + 
#   #OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Outreach.SWCD_36 + 
#   OWEB_HUC8_Grant_Tech.WC_36+
#   OWEB_HUC8_Grant_Tech.SWCD_36 + 
#   #OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Tech.SWCD_36 + 
#   OWEB_HUC8_Grant_Restoration.WC_36 +
#   OWEB_HUC8_Grant_Restoration.SWCD_36 + 
#   OWEB_HUC8_Grant_Outreach.WC_36:OWEB_HUC8_Grant_Tech.WC_36:OWEB_HUC8_Grant_Restoration.WC_36:
#   OWEB_HUC8_Grant_Outreach.SWCD_36:OWEB_HUC8_Grant_Tech.SWCD_36:OWEB_HUC8_Grant_Restoration.SWCD_36 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod_project_36 <- inla(form_project_36, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
#                        verbose=T,
#                        control.inla = list(
#                          correct = TRUE,
#                          correct.factor = correctionfactor))
# # 
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
#   f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod_capacity_36 <- inla(form_capacity_36, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                       
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
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



library(texreg)

tex.base.p1 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p1$summary.lincomb.derived$mean,
  ci.low = mod.base.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p1$dic$dic,mod.base.p1$waic$waic))

tex.base.p2 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p2$summary.lincomb.derived$mean,
  ci.low = mod.base.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p2$dic$dic,mod.base.p2$waic$waic))    

tex.base.p3 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p3$summary.lincomb.derived$mean,
  ci.low = mod.base.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p3$dic$dic,mod.base.p3$waic$waic))


htmlreg(l=list(tex.base.p1,tex.base.p2,tex.base.p3),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
        file = '../../../Deliverables/JPART/Version2/basemodtable.html')


tex.project.p1 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p1$summary.lincomb.derived$mean,
  ci.low = mod.project.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p1$dic$dic,mod.project.p1$waic$waic))

tex.project.p2 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p2$summary.lincomb.derived$mean,
  ci.low = mod.project.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p2$dic$dic,mod.project.p2$waic$waic))

tex.project.p3 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p3$summary.lincomb.derived$mean,
  ci.low = mod.project.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p3$dic$dic,mod.project.p3$waic$waic))



htmlreg(l=list(tex.project.p1,tex.project.p2,tex.project.p3),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
        file = '../../../Deliverables/JPART/Version2/projectmodtable.html')


tex.swcd.p1 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p1$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p1$dic$dic,mod.swcd.p1$waic$waic))

tex.swcd.p2 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p2$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p2$dic$dic,mod.swcd.p2$waic$waic))

tex.swcd.p3 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p3$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p3$dic$dic,mod.swcd.p3$waic$waic))

htmlreg(l=list(tex.swcd.p1,tex.swcd.p2,tex.swcd.p3),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
        file = '../../../Deliverables/JPART/Version2/swcdmodtable.html')



save.image('test.results.RData')

library(mail)
sendmail('tyler.andrew.scott@gmail.com','INLA model done')
