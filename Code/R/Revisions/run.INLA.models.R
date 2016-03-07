rm(list=ls())

remote = FALSE
nlog = FALSE
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
if(remote){mod.data = read.csv('../../../Input/update_data.csv')}
if(!remote){mod.data = read.csv('Input/update_data.csv')}

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
library(googlesheets)

council.dat = read.csv('https://docs.google.com/spreadsheets/d/1OXdC54OK8BwXAbIzMDudAKMvdblMhQQBJZ82_LUijFc/pub?output=csv')
#council.dat = fetchGoogle('https://docs.google.com/spreadsheets/d/1OXdC54OK8BwXAbIzMDudAKMvdblMhQQBJZ82_LUijFc/pub?output=csv')


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
mod.data$lat = mod.data$Decimal_Lat
mod.data$long = mod.data$Decimal_long
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
                     'seasonal','water_yr',
                    # 'Ag','Dev','Wetl','Forst',
                     'Station.Repl','HUC8.Repl',grep('proj.in|grants.in',colnames(mod.data),value=T),
                     grep('OWEB',names(mod.data),value=T),
                     grep('cond_good',names(mod.data),value=T),
                     grep('hist.avg',names(mod.data),value=T),
                     grep('_si',names(mod.data),value=T))]

if(nlog)
{
covars[,grep('OWEB',names(covars))] <- log(covars[,grep('OWEB',names(covars))] + 0.001)
}

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

if(remote) {oregon.huc8 = readOGR(dsn="../../../SpatialData/hydrologic_units", layer="wbdhu8_a_or")}
if(!remote) {oregon.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")}
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
  #YEARS.ACTIVE + 
  #OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC*YEARS.ACTIVE + 
  #OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC*STAFF.FTE + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
 # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p1)%*%A.1), e= rep(0,n.covariates.base.p1)))

form.base.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.2yr +
  #YEARS.ACTIVE + 
  #OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC*YEARS.ACTIVE + 
  #OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC*STAFF.FTE + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p2)%*%A.1), e= rep(0,n.covariates.base.p2)))

form.base.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.3yr +
  #YEARS.ACTIVE + 
  #OP.BUDGET.200k +  STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC*YEARS.ACTIVE + 
  #OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC*STAFF.FTE +  
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p3)%*%A.1), e= rep(0,n.covariates.base.p3)))

form.swcd.p1 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.1yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.1yr.WC + 
  OWEB.proj.in.last.1yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC*STAFF.FTE + 
 # OWEB.proj.in.last.1yr.SWCD + 
  OWEB.proj.in.last.1yr.WC*OWEB.proj.in.last.1yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p1)%*%A.1), e= rep(0,n.covariates.base.p1)))

form.swcd.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.2yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.2yr.WC + 
  OWEB.proj.in.last.2yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC*STAFF.FTE + 
  # OWEB.proj.in.last.2yr.SWCD + 
  OWEB.proj.in.last.2yr.WC*OWEB.proj.in.last.2yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p2)%*%A.1), e= rep(0,n.covariates.base.p2)))

form.swcd.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.3yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.3yr.WC + 
  OWEB.proj.in.last.3yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC*STAFF.FTE + 
  # OWEB.proj.in.last.3yr.SWCD + 
  OWEB.proj.in.last.3yr.WC*OWEB.proj.in.last.3yr.SWCD +
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
  OWEB.proj.in.last.1yr.WC.Tech:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC.Tech:OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC.Tech:STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC.Restoration:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC.Restoration:OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC.Restoration:STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC.Outreach:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC.Outreach:OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC.Outreach:STAFF.FTE + 
  OWEB.proj.in.last.1yr.WC.Capacity:YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC.Capacity:OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC.Capacity:STAFF.FTE + 
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
  OWEB.proj.in.last.2yr.WC.Tech:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC.Tech:OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC.Tech:STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC.Restoration:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC.Restoration:OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC.Restoration:STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC.Outreach:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC.Outreach:OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC.Outreach:STAFF.FTE + 
  OWEB.proj.in.last.2yr.WC.Capacity:YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC.Capacity:OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC.Capacity:STAFF.FTE + 
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
  OWEB.proj.in.last.3yr.WC.Tech:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC.Tech:OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC.Tech:STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC.Restoration:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC.Restoration:OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC.Restoration:STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC.Outreach:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC.Outreach:OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC.Outreach:STAFF.FTE + 
  OWEB.proj.in.last.3yr.WC.Capacity:YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC.Capacity:OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC.Capacity:STAFF.FTE + 
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
  cbind(mod.data$long,mod.data$lat),
  max.edge=c(5, 40),cut=.25))$n

spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$long,mod.data$lat))
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

load('Code/R/Revisions/test.results.RData')

draws = 100000
oweb.p1 = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC)
oweb.p1.staff = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)
oweb.p1.budg = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)
oweb.p1.years = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`)
oweb.p1.all = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`)

oweb.p2 = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC)
oweb.p2.staff = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)
oweb.p2.budg = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)
oweb.p2.years = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`)
oweb.p2.all = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`)

oweb.p3 = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC)
oweb.p3.staff = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)
oweb.p3.budg = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)
oweb.p3.years = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`)
oweb.p3.all = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`)

library(tidyr)
margs = data.frame(oweb.p1,oweb.p1.all,
                   oweb.p2,oweb.p2.all,
                   oweb.p3,oweb.p3.all)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3',margs.lon$key)] <- 'p3'
margs.lon$which = 'Linear'
margs.lon$which[grep('all',margs.lon$key)] <- 'Interaction'

margs.lon$uq <- paste(margs.lon$which,margs.lon$window)
margs.lon$uq = as.factor(margs.lon$uq)
levels(margs.lon$uq)
levels(margs.lon$uq) = c('Linear 1yr ','Linear 2yr ','Linear 3yr ','Interaction 1yr ','Interaction 2yr','Interaction 3yr')
library(ggthemes)
library(ggplot2)
bas.col = 'black'
alt.col = '#E69F00'
alt.col2 = "#56B4E9"
p1 = ggplot(subset(margs.lon))+ 
  geom_density(aes(x=value,
                   color=uq,linetype=uq),lwd=1)+
  scale_linetype_manual(values=c(1,2,3,1,2,3)) +
  scale_color_manual(values=c(rep(alt.col,3),rep('black',3))) +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Linear only vs. Interaction w/ council attributes') + ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=16)) 
w = 5
he = 534/973 * w
ggsave(filename = 'Deliverables/JPART/Final/figure1.eps',p1,width=w,height=he,units = 'in',dpi = 500,fonts=c("serif", "Palatino"))



draws = 100000
oweb.wc.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.swcd.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.wc.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.swcd.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.wc.p3.all = inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OWEB.proj.in.last.3yr.SWCD`)
oweb.swcd.p3.all = inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$OWEB.proj.in.last.3yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OWEB.proj.in.last.3yr.SWCD`)

margs = data.frame(oweb.wc.p1.all,oweb.swcd.p1.all,
                   oweb.wc.p2.all,oweb.swcd.p2.all,
                   oweb.wc.p3.all,oweb.swcd.p3.all)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1.all',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2.all',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3.all',margs.lon$key)] <- 'p3'
margs.lon$which = NA
margs.lon$which[grep('wc',margs.lon$key)] <- 'WC'
margs.lon$which[grep('swcd',margs.lon$key)] <- 'SWCD'
margs.lon$uq <- paste(margs.lon$which,margs.lon$window)



library(ggplot2)

margs.lon$uq = as.factor(margs.lon$uq)

levels(margs.lon$uq) = c("SWCD 1yr ", "SWCD 2yr " ,"SWCD 3yr ", "WC 1yr",  "WC 2yr"  , "WC 3yr"  )


p.gone = ggplot(subset(margs.lon))+ 
  geom_density(aes(x=value,
                   color=uq,linetype=uq),lwd=1)+
  scale_linetype_manual(values=c(1,2,3,1,2,3)) +
  scale_color_manual(values=c(rep(alt.col,3),rep('black',3))) +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Funds to WC vs. SWCD') + ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=18)) 

#ggsave(filename = 'Deliverables/JPART/Final/figure2.eps',p2,width=5,dpi = 500,fonts=c("serif", "Palatino"))


draws = 100000
oweb.project.p1.all.Tech = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Tech:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.all.Restoration = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Restoration:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.all.Outreach = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Outreach:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p2.all.Tech = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Tech:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.all.Restoration = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Restoration:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.all.Outreach = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Outreach:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p3.all.Tech = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Tech:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.all.Restoration = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Restoration:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.all.Outreach = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Outreach:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p1.fixed.Tech = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Tech) + 
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Tech`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Tech`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Tech:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.fixed.Restoration = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Restoration) + 
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Restoration`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Restoration`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Restoration:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.fixed.Outreach = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Outreach) + 
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Outreach`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Outreach`)+
  #  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Outreach:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p2.fixed.Tech = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Tech) + 
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Tech`)+
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Tech`)+
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Tech:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.fixed.Restoration = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Restoration) + 
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Restoration`)+
  # inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Restoration`)+
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Restoration:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.fixed.Outreach = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Outreach) + 
  # inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Outreach`)+
  #  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Outreach:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p3.fixed.Tech = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Tech) + 
  # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Tech`)+
  #  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Tech`)+
  # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Tech:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.fixed.Restoration = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Restoration) + 
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Restoration`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Restoration`)+
  # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Restoration:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.fixed.Outreach = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Outreach) + 
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Outreach:OWEB.proj.in.last.3yr.WC.Capacity`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)


margs = data.frame(
  oweb.project.p1.all.Tech,
  oweb.project.p1.all.Restoration,
  oweb.project.p1.all.Outreach,
  oweb.project.p2.all.Tech,
  oweb.project.p2.all.Restoration,
  oweb.project.p2.all.Outreach,
  oweb.project.p3.all.Tech,
  oweb.project.p3.all.Restoration,
  oweb.project.p3.all.Outreach,
  oweb.project.p1.fixed.Tech,
  oweb.project.p1.fixed.Restoration,
  oweb.project.p1.fixed.Outreach,
  oweb.project.p2.fixed.Tech,
  oweb.project.p2.fixed.Restoration,
  oweb.project.p2.fixed.Outreach,
  oweb.project.p3.fixed.Tech,
  oweb.project.p3.fixed.Restoration,
  oweb.project.p3.fixed.Outreach)
library(tidyr)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1.',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2.',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3.',margs.lon$key)] <- 'p3'

margs.lon$which = NA
margs.lon$which[grep('Outreach',margs.lon$key)] <- 'Outreach'
margs.lon$which[grep('Tech',margs.lon$key)] <- 'Tech'
margs.lon$which[grep('Restoration',margs.lon$key)] <- 'Restoration'

margs.lon$fix = NA
margs.lon$fix[grep('all',margs.lon$key)] <- 'All'
margs.lon$fix[grep('fixed',margs.lon$key)] <- 'Fixed'

margs.lon$uq <- paste(margs.lon$which,margs.lon$fix,margs.lon$window)
margs.lon$uq = as.factor(margs.lon$uq)

bas.col = 'black'
alt.col = '#E69F00'
alt.col2 = "#56B4E9"
library(dplyr)
library(ggplot2)


short.labs1 <- c('1yr w/ ','2yr w/','3yr w/',
                 '1yr w/out','2yr w/out','3yr w/out')
short.labs2 <- c('1yr w/ ','2yr w/','3yr w/',
                 '1yr w/out','2yr w/out','3yr w/out')
p2 = ggplot(filter(margs.lon,which=='Outreach'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#E69F00','black'),each=3),
                     name='Outreach Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Outreach Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.85,.6),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Outreach + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))
library(scales)

p3 = ggplot(filter(margs.lon,which=='Tech'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#56B4E9','black'),each=3),
                     name='Tech. Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Tech. Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.20,.6),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Tech. + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))

p4 = ggplot(filter(margs.lon,which=='Restoration'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#009E73','black'),each=3),
                     name='Restoration Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Restoration Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.30,.6),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Rest. + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))

ggsave(filename = 'Deliverables/JPART/Final/figure2.eps',p2,width=w,height=he,dpi = 500,fonts=c("serif", "Palatino"),units = 'in')
ggsave(filename = 'Deliverables/JPART/Final/figure3.eps',p3,width=w,height=he,dpi = 500,fonts=c("serif", "Palatino"),units = 'in')
ggsave(filename = 'Deliverables/JPART/Final/figure4.eps',p4,width=w,height=he,dpi = 500,fonts=c("serif", "Palatino"),units = 'in')


temp.or = readOGR(dsn='SpatialData/government_units','state_nrcs_a_or')

par(mar=c(3, 2, 2, 1))
plot(temp.or,ylim=c(min(mesh.a$loc[,2]),max(mesh.a$loc[,2])),col='grey80',border='grey80')
plot(mesh.a,add=TRUE)
points(mod.data$long[!duplicated(mod.data$Station)],
       mod.data$lat[!duplicated(mod.data$Station)],col='blue',pch=19,cex=.5)

covars$owqi2 <- covars$owqi + 4
covars$owqi2 <- ifelse(covars$owqi2>100,100,covars$owqi2)

covars$Qual = 'Excellent'
covars$Qual[covars$owqi < 90] <- 'Good'
covars$Qual[covars$owqi < 85] <- 'Fair'
covars$Qual[covars$owqi < 80] <- 'Poor'
covars$Qual[covars$owqi < 60] <- 'Very Poor'

covars$Qual = ordered(as.factor(covars$Qual),levels = c("Very Poor","Poor",'Fair','Good','Excellent'))

covars$Qual2 = 'Excellent'
covars$Qual2[covars$owqi2 < 90] <- 'Good'
covars$Qual2[covars$owqi2 < 85] <- 'Fair'
covars$Qual2[covars$owqi2 < 80] <- 'Poor'
covars$Qual2[covars$owqi2 < 60] <- 'Very Poor'
covars$Qual2 = ordered(as.factor(covars$Qual2),levels = c("Very Poor","Poor",'Fair','Good','Excellent'))

both.covars = c(covars$owqi,covars$owqi2)
which.owqi = rep(c('Observed','Observed + 4'),each=length(covars$owqi))



library(ggthemes)
p5 = ggplot() +
  geom_rect(aes(xmin=c(15,60,80,85,90),xmax=c(60,80,85,90,100),ymin=0,ymax=0.08),
           # fill = c("#D55E00", "#E69F00","#999999", "#56B4E9", "#009E73"),
           fill = NA,
            alpha = 0.2)+
  stat_density(aes(x=owqi,fill='grey'),data=covars,trim=TRUE,alpha=0.5,colour='black') +
  stat_density(aes(x=owqi2,fill='blue'),data=covars,trim=TRUE,alpha=0.5,colour='black') +
  geom_vline(xintercept=60,lty=2,col='grey50')+ geom_vline(xintercept=80,lty=2,col='grey50')+
  geom_vline(xintercept=85,lty=2,col='grey50') +geom_vline(xintercept=90,lty=2,col='grey50')+
  theme_bw() +
  scale_x_continuous('OWQI',expand=c(0,0),breaks=c(seq(20,90,10))) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.08)) +
  theme_tufte(ticks=F) +
  theme(
    legend.position = c(0.25,.5),
    axis.ticks=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=16),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=18),
    legend.text=element_text(size=16),
    legend.title=element_text(size=18)
  )  +
  scale_fill_colorblind(labels=c('Observed+4','Observed'),name='Distribution of Scores')+
  geom_text(aes(x=c(30,70,82.5,87.5,95),label=c("Very Poor","Poor",'Fair','Good','Excellent'),
                y=c(0.07,0.07,0.075,0.07,0.075)),size=8)+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))
#color = guide_legend(override.aes = list(linetype = 0)))
ggsave(filename = 'Deliverables/JPART/Final/figure5.tiff',p5,width=w,height=he,dpi = 500,fonts=c("serif", "Palatino"))

library(mail)
sendmail('tyler.andrew.scott@gmail.com','INLA model done')
