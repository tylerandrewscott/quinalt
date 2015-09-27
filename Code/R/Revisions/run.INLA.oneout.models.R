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

mod.data$YEARS.ACTIVE <- NA
mod.data$YEARS.ACTIVE[is.na(mod.data$which.wc)] <- 0
mod.data$YEARS.ACTIVE[!is.na(mod.data$YEAR.FOUNDED)] <- mod.data$YEAR[!is.na(mod.data$YEAR.FOUNDED)] - mod.data$YEAR.FOUNDED[!is.na(mod.data$YEAR.FOUNDED)]
mod.data$YEARS.ACTIVE[mod.data$YEARS.ACTIVE<0] <- 0 



#load('temp.workspace.precip.RData')
station.repl = data.frame(cbind(unique(mod.data$Station),1:length(unique(mod.data$Station))))
colnames(station.repl) = c('Station','Station.Repl')
mod.data = join(mod.data,station.repl)

huc8.repl = data.frame(cbind(unique(mod.data$HUC8),1:length(unique(mod.data$HUC8))))
colnames(huc8.repl) = c('HUC8','HUC8.Repl')
mod.data = join(mod.data,huc8.repl)

#test = readOGR(dsn='government.units','state.nrcs.a.or')

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
                     'owqi','monthly.precip.median','YEARS.ACTIVE','TOTAL.BUDGET','OPERATING.BUDGET','which.wc',
                     'Decimal_Lat',
                     'Decimal_long',
                     'seasonal','Ag','Dev','Wetl','Forst','Station.Repl','HUC8.Repl',
                     grep('OWEB',names(mod.data),value=T))]

colnames(covars)[grep('OWEB',colnames(covars))]  <- gsub('_','.',colnames(covars)[grep('OWEB',colnames(covars))])


covars[is.na(covars)] = 0


#covars$OWEB.HUC8.Grant.Capacity.PriorTo12 = #covars$OWEB.HUC8.Grant.Capacity.All.WC - #covars$OWEB.HUC8.Grant.Capacity.WC.12
#covars$OWEB.HUC8.Grant.Capacity.PriorTo36 = #covars$OWEB.HUC8.Grant.Capacity.All.WC - #covars$OWEB.HUC8.Grant.Capacity.WC.36
#covars$OWEB.HUC8.Grant.Capacity.PriorTo60 = #covars$OWEB.HUC8.Grant.Capacity.All.WC - #covars$OWEB.HUC8.Grant.Capacity.60.WC


covars = mutate(covars,OWEB.HUC8.Grant.All.WC.12 = OWEB.HUC8.Grant.Restoration.WC.12+
                  OWEB.HUC8.Grant.Capacity.WC.12 +
                  OWEB.HUC8.Grant.Tech.WC.12 +
                  OWEB.HUC8.Grant.Outreach.WC.12,
                OWEB.HUC8.Grant.All.WC.24 = OWEB.HUC8.Grant.Restoration.WC.24+
                  OWEB.HUC8.Grant.Capacity.WC.24 +
                  OWEB.HUC8.Grant.Tech.WC.24 +
                  OWEB.HUC8.Grant.Outreach.WC.24,
                OWEB.HUC8.Grant.All.WC.36 = OWEB.HUC8.Grant.Restoration.WC.36+
                  OWEB.HUC8.Grant.Capacity.WC.36 +
                  OWEB.HUC8.Grant.Tech.WC.36 +
                  OWEB.HUC8.Grant.Outreach.WC.36,
                #                 OWEB.HUC8.Grant.All.48.WC = OWEB.HUC8.Grant.Restoration.48.WC+
                #                   OWEB.HUC8.Grant.Capacity.48.WC +
                #                   OWEB.HUC8.Grant.Tech.48.WC +
                #                   OWEB.HUC8.Grant.Outreach.48.WC,
                #                 OWEB.HUC8.Grant.All.60.WC = OWEB.HUC8.Grant.Restoration.60.WC+
                #                   OWEB.HUC8.Grant.Capacity.60.WC +
                #                   OWEB.HUC8.Grant.Tech.60.WC +
                #                   OWEB.HUC8.Grant.Outreach.60.WC,
                #                 OWEB.HUC8.Grant.All.All.WC = OWEB.HUC8.Grant.Restoration.All.WC+
                #                   OWEB.HUC8.Grant.Capacity.All.WC +
                #                   OWEB.HUC8.Grant.Tech.All.WC +
                #                   OWEB.HUC8.Grant.Outreach.All.WC,
                
                OWEB.HUC8.Grant.All.SWCD.12 = OWEB.HUC8.Grant.Restoration.SWCD.12+
                  OWEB.HUC8.Grant.Capacity.SWCD.12 +
                  OWEB.HUC8.Grant.Tech.SWCD.12 +
                  OWEB.HUC8.Grant.Outreach.SWCD.12,
                
                OWEB.HUC8.Grant.All.SWCD.24 = OWEB.HUC8.Grant.Restoration.SWCD.24+
                  OWEB.HUC8.Grant.Capacity.SWCD.24 +
                  OWEB.HUC8.Grant.Tech.SWCD.24 +
                  OWEB.HUC8.Grant.Outreach.SWCD.24,
                
                
                OWEB.HUC8.Grant.All.SWCD.36 = OWEB.HUC8.Grant.Restoration.SWCD.36+
                  OWEB.HUC8.Grant.Capacity.SWCD.36 +
                  OWEB.HUC8.Grant.Tech.SWCD.36 +
                  OWEB.HUC8.Grant.Outreach.SWCD.36,
                
                OWEB.HUC8.Grant.Tech.Both.12 = OWEB.HUC8.Grant.Tech.SWCD.12 + OWEB.HUC8.Grant.Tech.WC.12,
                OWEB.HUC8.Grant.Restoration.Both.12 = OWEB.HUC8.Grant.Restoration.SWCD.12 + OWEB.HUC8.Grant.Restoration.WC.12,
                OWEB.HUC8.Grant.Outreach.Both.12 = OWEB.HUC8.Grant.Outreach.SWCD.12 + OWEB.HUC8.Grant.Outreach.WC.12,
                OWEB.HUC8.Grant.Capacity.Both.12 = OWEB.HUC8.Grant.Capacity.SWCD.12 + OWEB.HUC8.Grant.Capacity.WC.12,
                OWEB.HUC8.Grant.All.Both.12 = OWEB.HUC8.Grant.All.WC.12 + OWEB.HUC8.Grant.All.SWCD.12,
                
                
                
                OWEB.HUC8.Grant.Tech.Both.24 = OWEB.HUC8.Grant.Tech.SWCD.24 + OWEB.HUC8.Grant.Tech.WC.24,
                OWEB.HUC8.Grant.Restoration.Both.24 = OWEB.HUC8.Grant.Restoration.SWCD.24 + OWEB.HUC8.Grant.Restoration.WC.24,
                OWEB.HUC8.Grant.Outreach.Both.24 = OWEB.HUC8.Grant.Outreach.SWCD.24 + OWEB.HUC8.Grant.Outreach.WC.24,
                OWEB.HUC8.Grant.Capacity.Both.24 = OWEB.HUC8.Grant.Capacity.SWCD.24 + OWEB.HUC8.Grant.Capacity.WC.24,
                OWEB.HUC8.Grant.All.Both.24 = OWEB.HUC8.Grant.All.WC.24 + OWEB.HUC8.Grant.All.SWCD.24,
                
                OWEB.HUC8.Grant.Tech.Both.36 = OWEB.HUC8.Grant.Tech.SWCD.36 + OWEB.HUC8.Grant.Tech.WC.36,
                OWEB.HUC8.Grant.Restoration.Both.36 = OWEB.HUC8.Grant.Restoration.SWCD.36 + OWEB.HUC8.Grant.Restoration.WC.36,
                OWEB.HUC8.Grant.Outreach.Both.36 = OWEB.HUC8.Grant.Outreach.SWCD.36 + OWEB.HUC8.Grant.Outreach.WC.36,
                OWEB.HUC8.Grant.Capacity.Both.36 = OWEB.HUC8.Grant.Capacity.SWCD.36 + OWEB.HUC8.Grant.Capacity.WC.36,
                OWEB.HUC8.Grant.All.Both.36 = OWEB.HUC8.Grant.All.WC.24 + OWEB.HUC8.Grant.All.SWCD.24,
                
                OWEB.HUC8.Grant.All.Public.12 = OWEB.HUC8.Grant.Restoration.Public.12+
                  OWEB.HUC8.Grant.Capacity.Public.12 +
                  OWEB.HUC8.Grant.Tech.Public.12 +
                  OWEB.HUC8.Grant.Outreach.Public.12,
                OWEB.HUC8.Grant.All.Public.24 = OWEB.HUC8.Grant.Restoration.Public.24+
                  OWEB.HUC8.Grant.Capacity.Public.24 +
                  OWEB.HUC8.Grant.Tech.Public.24 +
                  OWEB.HUC8.Grant.Outreach.Public.24,
                OWEB.HUC8.Grant.All.Public.36 = OWEB.HUC8.Grant.Restoration.Public.36+
                  OWEB.HUC8.Grant.Capacity.Public.36 +
                  OWEB.HUC8.Grant.Tech.Public.36 +
                  OWEB.HUC8.Grant.Outreach.Public.36,
                
                OWEB.HUC8.Grant.All.Other.12 = OWEB.HUC8.Grant.Restoration.Other.12+
                  OWEB.HUC8.Grant.Capacity.Other.12 +
                  OWEB.HUC8.Grant.Tech.Other.12 +
                  OWEB.HUC8.Grant.Outreach.Other.12,
                OWEB.HUC8.Grant.All.Other.24 = OWEB.HUC8.Grant.Restoration.Other.24+
                  OWEB.HUC8.Grant.Capacity.Other.24 +
                  OWEB.HUC8.Grant.Tech.Other.24 +
                  OWEB.HUC8.Grant.Outreach.Other.24,
                OWEB.HUC8.Grant.All.Other.36 = OWEB.HUC8.Grant.Restoration.Other.36+
                  OWEB.HUC8.Grant.Capacity.Other.36 +
                  OWEB.HUC8.Grant.Tech.Other.36 +
                  OWEB.HUC8.Grant.Outreach.Other.36,
                
                OWEB.HUC8.Grant.All.But.WC.All.12 = OWEB.HUC8.Grant.All.Public.12 + OWEB.HUC8.Grant.All.SWCD.12 + OWEB.HUC8.Grant.All.Other.12,
                OWEB.HUC8.Grant.All.But.WC.All.24 = OWEB.HUC8.Grant.All.Public.24 + OWEB.HUC8.Grant.All.SWCD.24 + OWEB.HUC8.Grant.All.Other.24,
                OWEB.HUC8.Grant.All.But.WC.All.36 = OWEB.HUC8.Grant.All.Public.36 + OWEB.HUC8.Grant.All.SWCD.36 + OWEB.HUC8.Grant.All.Other.36,
                
                OWEB.HUC8.Grant.All.12 = OWEB.HUC8.Grant.All.Public.12 + OWEB.HUC8.Grant.All.SWCD.12 + OWEB.HUC8.Grant.All.Other.12 + OWEB.HUC8.Grant.All.WC.12,
                OWEB.HUC8.Grant.All.24 = OWEB.HUC8.Grant.All.Public.24 + OWEB.HUC8.Grant.All.SWCD.24 + OWEB.HUC8.Grant.All.Other.24 + OWEB.HUC8.Grant.All.WC.24,
                OWEB.HUC8.Grant.All.36 = OWEB.HUC8.Grant.All.Public.36 + OWEB.HUC8.Grant.All.SWCD.36 + OWEB.HUC8.Grant.All.Other.36 + OWEB.HUC8.Grant.All.WC.36,
                
  
                OWEB.HUC8.Grant.All.But.WC.Restoration.12 = OWEB.HUC8.Grant.All.12 - OWEB.HUC8.Grant.Restoration.WC.12,
                OWEB.HUC8.Grant.All.But.WC.Outreach.12= OWEB.HUC8.Grant.All.12 - OWEB.HUC8.Grant.Outreach.WC.12,
                OWEB.HUC8.Grant.All.But.WC.Tech.12= OWEB.HUC8.Grant.All.12 - OWEB.HUC8.Grant.Tech.WC.12,
                OWEB.HUC8.Grant.All.But.WC.Capacity.12= OWEB.HUC8.Grant.All.12 - OWEB.HUC8.Grant.Capacity.WC.12,
                
                OWEB.HUC8.Grant.All.But.WC.Restoration.24 = OWEB.HUC8.Grant.All.24 - OWEB.HUC8.Grant.Restoration.WC.24,
                OWEB.HUC8.Grant.All.But.WC.Outreach.24= OWEB.HUC8.Grant.All.24 - OWEB.HUC8.Grant.Outreach.WC.24,
                OWEB.HUC8.Grant.All.But.WC.Tech.24= OWEB.HUC8.Grant.All.24 - OWEB.HUC8.Grant.Tech.WC.24,
                OWEB.HUC8.Grant.All.But.WC.Capacity.24= OWEB.HUC8.Grant.All.24 - OWEB.HUC8.Grant.Capacity.WC.24,
                
              
                OWEB.HUC8.Grant.All.But.WC.Restoration.36 = OWEB.HUC8.Grant.All.36 - OWEB.HUC8.Grant.Restoration.WC.36,
                OWEB.HUC8.Grant.All.But.WC.Outreach.36= OWEB.HUC8.Grant.All.36 - OWEB.HUC8.Grant.Outreach.WC.36,
                OWEB.HUC8.Grant.All.But.WC.Tech.36= OWEB.HUC8.Grant.All.36 - OWEB.HUC8.Grant.Tech.WC.36,
                OWEB.HUC8.Grant.All.But.WC.Capacity.36= OWEB.HUC8.Grant.All.36 - OWEB.HUC8.Grant.Capacity.WC.36,
                
                
                OWEB.HUC8.Grant.All.WC.But.WC.Restoration.12 = OWEB.HUC8.Grant.All.But.WC.Restoration.12 - OWEB.HUC8.Grant.Restoration.WC.12,
                OWEB.HUC8.Grant.All.WC.But.WC.Outreach.12 = OWEB.HUC8.Grant.All.But.WC.Outreach.12 - OWEB.HUC8.Grant.Outreach.WC.12,
                OWEB.HUC8.Grant.All.WC.But.WC.Tech.12 = OWEB.HUC8.Grant.All.But.WC.Tech.12 - OWEB.HUC8.Grant.Tech.WC.12,
                OWEB.HUC8.Grant.All.WC.But.WC.Capacity.12 = OWEB.HUC8.Grant.All.But.WC.Capacity.12 - OWEB.HUC8.Grant.Capacity.WC.12,
                
                
                OWEB.HUC8.Grant.All.WC.But.WC.Restoration.24 = OWEB.HUC8.Grant.All.But.WC.Restoration.24 - OWEB.HUC8.Grant.Restoration.WC.24,
                OWEB.HUC8.Grant.All.WC.But.WC.Outreach.24 = OWEB.HUC8.Grant.All.But.WC.Outreach.24 - OWEB.HUC8.Grant.Outreach.WC.24,
                OWEB.HUC8.Grant.All.WC.But.WC.Tech.24 = OWEB.HUC8.Grant.All.But.WC.Tech.24 - OWEB.HUC8.Grant.Tech.WC.24,
                OWEB.HUC8.Grant.All.WC.But.WC.Capacity.24 = OWEB.HUC8.Grant.All.But.WC.Capacity.24 - OWEB.HUC8.Grant.Capacity.WC.24,
                
                
                OWEB.HUC8.Grant.All.WC.But.WC.Restoration.36 = OWEB.HUC8.Grant.All.But.WC.Restoration.36 - OWEB.HUC8.Grant.Restoration.WC.36,
                OWEB.HUC8.Grant.All.WC.But.WC.Outreach.36 = OWEB.HUC8.Grant.All.But.WC.Outreach.36 - OWEB.HUC8.Grant.Outreach.WC.36,
                OWEB.HUC8.Grant.All.WC.But.WC.Tech.36 = OWEB.HUC8.Grant.All.But.WC.Tech.36 - OWEB.HUC8.Grant.Tech.WC.36,
                OWEB.HUC8.Grant.All.WC.But.WC.Capacity.36 = OWEB.HUC8.Grant.All.But.WC.Capacity.36 - OWEB.HUC8.Grant.Capacity.WC.36
                #                 OWEB.HUC8.Grant.All.48.SWCD = OWEB.HUC8.Grant.Restoration.48.SWCD+
                #                   OWEB.HUC8.Grant.Capacity.48.SWCD +
                #                   OWEB.HUC8.Grant.Tech.48.SWCD +
                #                   OWEB.HUC8.Grant.Outreach.48.SWCD,
                #                 OWEB.HUC8.Grant.All.60.SWCD = OWEB.HUC8.Grant.Restoration.60.SWCD+
                #                   OWEB.HUC8.Grant.Capacity.60.SWCD +
                #                   OWEB.HUC8.Grant.Tech.60.SWCD +
                #                   OWEB.HUC8.Grant.Outreach.60.SWCD,
                #                 OWEB.HUC8.Grant.All.All.SWCD = OWEB.HUC8.Grant.Restoration.All.SWCD+
                #                   OWEB.HUC8.Grant.Capacity.All.SWCD +
                #                   OWEB.HUC8.Grant.Tech.All.SWCD +
                #                   OWEB.HUC8.Grant.Outreach.All.SWCD
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


binary.funding.indicator = data.frame(ifelse(covars[,grep('OWEB.HUC8',colnames(covars))] >0, 1,0))
colnames(binary.funding.indicator) = paste0(colnames(covars)[grep('OWEB.HUC8',colnames(covars))],'.ind')
covars = cbind(covars,binary.funding.indicator)

k = 1000000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k

covars$OWEB.HUC8.Grant.All.Both.24.ind <-  as.integer((covars$OWEB.HUC8.Grant.All.WC.24.ind + covars$OWEB.HUC8.Grant.All.SWCD.24.ind)>0)
covars$OWEB.HUC8.Grant.All.Both.12.ind <-  as.integer((covars$OWEB.HUC8.Grant.All.WC.12.ind + covars$OWEB.HUC8.Grant.All.SWCD.12.ind)>0)
covars$OWEB.HUC8.Grant.All.Both.36.ind <-  as.integer((covars$OWEB.HUC8.Grant.All.WC.36.ind + covars$OWEB.HUC8.Grant.All.SWCD.36.ind)>0)

covars$OWEB.HUC8.Grant.Restoration.Both.24.ind <-  as.integer((covars$OWEB.HUC8.Grant.Restoration.WC.24.ind + covars$OWEB.HUC8.Grant.Restoration.SWCD.24.ind)>0)
covars$OWEB.HUC8.Grant.Restoration.Both.12.ind <-  as.integer((covars$OWEB.HUC8.Grant.Restoration.WC.12.ind + covars$OWEB.HUC8.Grant.Restoration.SWCD.12.ind)>0)
covars$OWEB.HUC8.Grant.Restoration.Both.36.ind <-  as.integer((covars$OWEB.HUC8.Grant.Restoration.WC.36.ind + covars$OWEB.HUC8.Grant.Restoration.SWCD.36.ind)>0)


covars$OWEB.HUC8.Grant.Tech.Both.24.ind <-  as.integer((covars$OWEB.HUC8.Grant.Tech.WC.24.ind + covars$OWEB.HUC8.Grant.Tech.SWCD.24.ind)>0)
covars$OWEB.HUC8.Grant.Tech.Both.12.ind <-  as.integer((covars$OWEB.HUC8.Grant.Tech.WC.12.ind + covars$OWEB.HUC8.Grant.Tech.SWCD.12.ind)>0)
covars$OWEB.HUC8.Grant.Tech.Both.36.ind <-  as.integer((covars$OWEB.HUC8.Grant.Tech.WC.36.ind + covars$OWEB.HUC8.Grant.Tech.SWCD.36.ind)>0)

covars$OWEB.HUC8.Grant.Outreach.Both.24.ind <-  as.integer((covars$OWEB.HUC8.Grant.Outreach.WC.24.ind + covars$OWEB.HUC8.Grant.Outreach.SWCD.24.ind)>0)
covars$OWEB.HUC8.Grant.Outreach.Both.12.ind <-  as.integer((covars$OWEB.HUC8.Grant.Outreach.WC.12.ind + covars$OWEB.HUC8.Grant.Outreach.SWCD.12.ind)>0)
covars$OWEB.HUC8.Grant.Outreach.Both.36.ind <-  as.integer((covars$OWEB.HUC8.Grant.Outreach.WC.36.ind + covars$OWEB.HUC8.Grant.Outreach.SWCD.36.ind)>0)

covars$OWEB.HUC8.Grant.Capacity.Both.24.ind <-  as.integer((covars$OWEB.HUC8.Grant.Capacity.WC.24.ind + covars$OWEB.HUC8.Grant.Capacity.SWCD.24.ind)>0)
covars$OWEB.HUC8.Grant.Capacity.Both.12.ind <-  as.integer((covars$OWEB.HUC8.Grant.Capacity.WC.12.ind + covars$OWEB.HUC8.Grant.Capacity.SWCD.12.ind)>0)
covars$OWEB.HUC8.Grant.Capacity.Both.36.ind <-  as.integer((covars$OWEB.HUC8.Grant.Capacity.WC.36.ind + covars$OWEB.HUC8.Grant.Capacity.SWCD.36.ind)>0)

#covars[,grep('OWEB',names(covars))] = log(covars[,grep('OWEB',names(covars))]+0.001)


covars[,intersect(grep('OWEB',names(covars)),grep('.ind',names(covars),invert = T))] <-
  apply(covars[,intersect(grep('OWEB',names(covars)),grep('.ind',names(covars),invert = T))],2,scale,center=TRUE,scale=TRUE)

covars[colnames(covars) %in% c('Decimal_Lat','Decimal_long','ag.huc8','dev.huc8','forst.huc8','NOT.OWEB.OWRI.wq.TotalCash.24',
                               'monthly.precip.median')] <-
  apply(covars[colnames(covars) %in% c('Decimal_Lat','Decimal_long','ag.huc8','dev.huc8','forst.huc8','NOT.OWEB.OWRI.wq.TotalCash.24',
                                       'monthly.precip.median')],2,scale,center=TRUE,scale=TRUE)

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


#or.bond = inla.nonconvex.hull(cbind(covars$Decimal_long,covars$Decimal_Lat),2,2)
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


form.base.12 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + 
  ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
  YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.12 +
  OWEB.HUC8.Grant.All.WC.12 +
  OWEB.HUC8.Grant.All.But.WC.All.12 + 
  OWEB.HUC8.Grant.All.WC.12:
  OWEB.HUC8.Grant.All.But.WC.All.12 + 
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.12)%*%A.1), e= rep(0,n.covariates.base.12)))


form.project.12 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
  YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.12 +
  OWEB.HUC8.Grant.Outreach.WC.12+
  OWEB.HUC8.Grant.Tech.WC.12+
  OWEB.HUC8.Grant.Restoration.WC.12 +
  OWEB.HUC8.Grant.All.But.WC.Outreach.12 +
  OWEB.HUC8.Grant.All.But.WC.Tech.12 +
  OWEB.HUC8.Grant.All.But.WC.Restoration.12 + 
  OWEB.HUC8.Grant.Outreach.WC.12:OWEB.HUC8.Grant.All.But.WC.Outreach.12 +
  OWEB.HUC8.Grant.Tech.WC.12:OWEB.HUC8.Grant.All.But.WC.Tech.12 +
  OWEB.HUC8.Grant.Restoration.WC.12:OWEB.HUC8.Grant.All.But.WC.Restoration.12+
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
  f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.12)%*%A.1), e= rep(0,n.covariates.project.12)))

  form.base.24 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + 
    ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
    YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.24 +
    OWEB.HUC8.Grant.All.WC.24 +
    OWEB.HUC8.Grant.All.But.WC.All.24 + 
    OWEB.HUC8.Grant.All.WC.24:
    OWEB.HUC8.Grant.All.But.WC.All.24 + 
    f(HUC8,model='iid',param=c(0.001,0.001)) + 
    f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
    f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=24) + 
    f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.24)%*%A.1), e= rep(0,n.covariates.base.24)))
  
  
  form.project.24 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
    YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.24 +
    OWEB.HUC8.Grant.Outreach.WC.24+
    OWEB.HUC8.Grant.Tech.WC.24+
    OWEB.HUC8.Grant.Restoration.WC.24 +
    OWEB.HUC8.Grant.All.But.WC.Outreach.24 +
    OWEB.HUC8.Grant.All.But.WC.Tech.24 +
    OWEB.HUC8.Grant.All.But.WC.Restoration.24 + 
    OWEB.HUC8.Grant.Outreach.WC.24:OWEB.HUC8.Grant.All.But.WC.Outreach.24 +
  OWEB.HUC8.Grant.Tech.WC.24:OWEB.HUC8.Grant.All.But.WC.Tech.24 +
  OWEB.HUC8.Grant.Restoration.WC.24:OWEB.HUC8.Grant.All.But.WC.Restoration.24+
    f(HUC8,model='iid',param=c(0.001,0.001)) + 
    f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
    f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
    f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.24)%*%A.1), e= rep(0,n.covariates.project.24)))
  

  form.base.36 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + 
    ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
    YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.36 +
    OWEB.HUC8.Grant.All.WC.36 +
    OWEB.HUC8.Grant.All.But.WC.All.36 + 
    OWEB.HUC8.Grant.All.WC.36:
    OWEB.HUC8.Grant.All.But.WC.All.36 + 
    f(HUC8,model='iid',param=c(0.001,0.001)) + 
    f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
    f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=36) + 
    f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.36)%*%A.1), e= rep(0,n.covariates.base.36)))
  
  
  form.project.36 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + elev100m +  monthly.precip.median + 
    YEARS.ACTIVE + NOT.OWEB.OWRI.wq.TotalCash.36 +
    OWEB.HUC8.Grant.Outreach.WC.36+
    OWEB.HUC8.Grant.Tech.WC.36+
    OWEB.HUC8.Grant.Restoration.WC.36 +
    OWEB.HUC8.Grant.All.But.WC.Outreach.36 +
    OWEB.HUC8.Grant.All.But.WC.Tech.36 +
    OWEB.HUC8.Grant.All.But.WC.Restoration.36 + 
    OWEB.HUC8.Grant.Outreach.WC.36:OWEB.HUC8.Grant.All.But.WC.Outreach.36 +
  OWEB.HUC8.Grant.Tech.WC.36:OWEB.HUC8.Grant.All.But.WC.Tech.36 +
  OWEB.HUC8.Grant.Restoration.WC.36:OWEB.HUC8.Grant.All.But.WC.Restoration.36+
    f(HUC8,model='iid',param=c(0.001,0.001)) + 
    f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
    f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
    f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.project.36)%*%A.1), e= rep(0,n.covariates.project.36)))
  

covars$Full.Base.Interaction.12 = covars$OWEB.HUC8.Grant.All.WC.12 * covars$OWEB.HUC8.Grant.All.SWCD.12
covars$Full.Project.Interaction.12 = covars$OWEB.HUC8.Grant.Tech.WC.12 * covars$OWEB.HUC8.Grant.Tech.SWCD.12*
  covars$OWEB.HUC8.Grant.Outreach.WC.12 * covars$OWEB.HUC8.Grant.Outreach.SWCD.12*
  covars$OWEB.HUC8.Grant.Restoration.WC.12 * covars$OWEB.HUC8.Grant.Restoration.SWCD.12



X.base.12 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,
                  covars$monthly.precip.median,
                  covars$YEARS.ACTIVE,
                  covars$NOT.OWEB.OWRI.wq.TotalCash.12,
                  covars$OWEB.HUC8.Grant.All.WC.12,
                  covars$OWEB.HUC8.Grant.All.But.WC.All.12,
                  covars$OWEB.HUC8.Grant.All.WC.12*
                    covars$OWEB.HUC8.Grant.All.But.WC.All.12)

n.covariates.base.12 = ncol(X.base.12)
Q.base.12 = qr.Q(qr(X.base.12))


X.project.12 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$monthly.precip.median,
                      covars$YEARS.ACTIVE,
                      covars$NOT.OWEB.OWRI.wq.TotalCash.12,
                      covars$OWEB.HUC8.Grant.Outreach.WC.12,
                        covars$OWEB.HUC8.Grant.Tech.WC.12,
                        covars$OWEB.HUC8.Grant.Restoration.WC.12 ,
                        covars$OWEB.HUC8.Grant.All.But.WC.Outreach.12 ,
                        covars$OWEB.HUC8.Grant.All.But.WC.Tech.12 ,
                        covars$OWEB.HUC8.Grant.All.But.WC.Restoration.12 , 
                        covars$OWEB.HUC8.Grant.Outreach.WC.12*covars$OWEB.HUC8.Grant.All.But.WC.Outreach.12, 
                      covars$OWEB.HUC8.Grant.Tech.WC.12*covars$OWEB.HUC8.Grant.All.But.WC.Tech.12 ,
                      covars$OWEB.HUC8.Grant.Restoration.WC.12*covars$OWEB.HUC8.Grant.All.But.WC.Restoration.12)


n.covariates.project.12 = ncol(X.project.12)
Q.project.12 = qr.Q(qr(X.project.12))


covars$Full.Base.Interaction.24 = covars$OWEB.HUC8.Grant.All.WC.24 * covars$OWEB.HUC8.Grant.All.SWCD.24
covars$Full.Project.Interaction.24 = covars$OWEB.HUC8.Grant.Tech.WC.24 * covars$OWEB.HUC8.Grant.Tech.SWCD.24*
  covars$OWEB.HUC8.Grant.Outreach.WC.24 * covars$OWEB.HUC8.Grant.Outreach.SWCD.24*
  covars$OWEB.HUC8.Grant.Restoration.WC.24 * covars$OWEB.HUC8.Grant.Restoration.SWCD.24


X.base.24 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,
                  covars$monthly.precip.median,
                  covars$YEARS.ACTIVE,
                  covars$NOT.OWEB.OWRI.wq.TotalCash.24,
                  covars$OWEB.HUC8.Grant.All.WC.24,
                  covars$OWEB.HUC8.Grant.All.But.WC.All.24,
                  covars$OWEB.HUC8.Grant.All.WC.24*
                    covars$OWEB.HUC8.Grant.All.But.WC.All.24)

n.covariates.base.24 = ncol(X.base.24)
Q.base.24 = qr.Q(qr(X.base.24))


X.project.24 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$monthly.precip.median,
                      covars$YEARS.ACTIVE,
                      covars$NOT.OWEB.OWRI.wq.TotalCash.24,
                      covars$OWEB.HUC8.Grant.Outreach.WC.24,
                      covars$OWEB.HUC8.Grant.Tech.WC.24,
                      covars$OWEB.HUC8.Grant.Restoration.WC.24 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Outreach.24 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Tech.24 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Restoration.24 , 
                      covars$OWEB.HUC8.Grant.Outreach.WC.24*covars$OWEB.HUC8.Grant.All.But.WC.Outreach.24 ,
                      covars$OWEB.HUC8.Grant.Tech.WC.24*covars$OWEB.HUC8.Grant.All.But.WC.Tech.24 ,
                      covars$OWEB.HUC8.Grant.Restoration.WC.24*covars$OWEB.HUC8.Grant.All.But.WC.Restoration.24)


n.covariates.project.24 = ncol(X.project.24)
Q.project.24 = qr.Q(qr(X.project.24))

covars$Full.Base.Interaction.36 = covars$OWEB.HUC8.Grant.All.WC.36 * covars$OWEB.HUC8.Grant.All.SWCD.36
covars$Full.Project.Interaction.36 = covars$OWEB.HUC8.Grant.Tech.WC.36 * covars$OWEB.HUC8.Grant.Tech.SWCD.36*
  covars$OWEB.HUC8.Grant.Outreach.WC.36 * covars$OWEB.HUC8.Grant.Outreach.SWCD.36*
  covars$OWEB.HUC8.Grant.Restoration.WC.36 * covars$OWEB.HUC8.Grant.Restoration.SWCD.36

X.base.36 = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                  covars$ag.huc8, covars$forst.huc8,
                  covars$dev.huc8,
                  covars$elev100m,
                  covars$monthly.precip.median,
                  covars$YEARS.ACTIVE,
                  covars$NOT.OWEB.OWRI.wq.TotalCash.36,
                  covars$OWEB.HUC8.Grant.All.WC.36,
                  covars$OWEB.HUC8.Grant.All.But.WC.All.36,
                  covars$OWEB.HUC8.Grant.All.WC.36*
                    covars$OWEB.HUC8.Grant.All.But.WC.All.36)

n.covariates.base.36 = ncol(X.base.36)
Q.base.36 = qr.Q(qr(X.base.36))


X.project.36 <- cbind(rep(1,n.data),
                      covars$Decimal_Lat, covars$Decimal_long,
                      covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                      covars$elev100m,covars$monthly.precip.median,
                      covars$YEARS.ACTIVE,
                      covars$NOT.OWEB.OWRI.wq.TotalCash.36,
                      covars$OWEB.HUC8.Grant.Outreach.WC.36,
                      covars$OWEB.HUC8.Grant.Tech.WC.36,
                      covars$OWEB.HUC8.Grant.Restoration.WC.36 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Outreach.36 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Tech.36 ,
                      covars$OWEB.HUC8.Grant.All.But.WC.Restoration.36 , 
                      covars$OWEB.HUC8.Grant.Outreach.WC.36*covars$OWEB.HUC8.Grant.All.But.WC.Outreach.36 ,
                      covars$OWEB.HUC8.Grant.Tech.WC.36*covars$OWEB.HUC8.Grant.All.But.WC.Tech.36 ,
                      covars$OWEB.HUC8.Grant.Restoration.WC.36*covars$OWEB.HUC8.Grant.All.But.WC.Restoration.36)


n.covariates.project.36 = ncol(X.project.36)
Q.project.36 = qr.Q(qr(X.project.36))



(mesh.a <- inla.mesh.2d(
  cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),
  max.edge=c(5, 40),cut=.25))$n

spde.a <- inla.spde2.matern(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, 
                        loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)
stk.1 <- inla.stack(data=list(y=covars$owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))


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

mod.base.12 <- inla(form.base.12, family='gaussian',
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


mod.base.24 <- inla(form.base.24, family='gaussian',
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


mod.base.36 <- inla(form.base.36, family='gaussian',
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

mod.project.12 <- inla(form.project.12, family='gaussian',
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


mod.project.24 <- inla(form.project.24, family='gaussian',
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


mod.project.36 <- inla(form.project.36, family='gaussian',
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
#           covars$NOT.OWEB.OWRI.wq.TotalCash.12,
#           covars$OWEB.HUC8.Grant.All.WC.12,
#           covars$OWEB.HUC8.Grant.All.SWCD.12,
#           covars$OWEB.HUC8.Grant.all.WC12* covars$OWEB.HUC8.Grant.All.SWCD.12)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# head(X)
# 
# form.project.12 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   YEARS.ACTIVE + 
#   NOT.OWEB.OWRI.wq.TotalCash.12 + 
#   OWEB.HUC8.Grant.Outreach.WC.12+
#   OWEB.HUC8.Grant.Outreach.SWCD.12 + 
#   #OWEB.HUC8.Grant.Outreach.WC.12:OWEB.HUC8.Grant.Outreach.SWCD.12 + 
#   OWEB.HUC8.Grant.Tech.WC.12+
#   OWEB.HUC8.Grant.Tech.SWCD.12 + 
#   #OWEB.HUC8.Grant.Tech.WC.12:OWEB.HUC8.Grant.Tech.SWCD.12 + 
#   OWEB.HUC8.Grant.Restoration.WC.12 +
#   OWEB.HUC8.Grant.Restoration.SWCD.12 + 
#   OWEB.HUC8.Grant.Outreach.WC.12:OWEB.HUC8.Grant.Tech.WC.12:OWEB.HUC8.Grant.Restoration.WC.12:
#   OWEB.HUC8.Grant.Outreach.SWCD.12:OWEB.HUC8.Grant.Tech.SWCD.12:OWEB.HUC8.Grant.Restoration.SWCD.12 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod.project.12 <- inla(form.project.12, family='gaussian', data=inla.stack.data(stk.1),
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
# 
# # put all the covariates (and the intercept) in a ``design matrix'' and make the matrix for the regression problem.  Using a QR factorisation for stability (don't worry!) the regression coefficients would be t(Q)%*%(spde)
# X = cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
#           #covars$Ag, #covars$Forst,
#           #covars$Dev, 
#           covars$ag.huc8, covars$forst.huc8,covars$dev.huc8,
#            covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$NOT.OWEB.OWRI.wq.TotalCash.12,
# covars$OWEB.HUC8.Grant.Outreach.WC.12, 
# covars$OWEB.HUC8.Grant.Tech.WC.12,
# covars$OWEB.HUC8.Grant.Restoration.WC.12,
# covars$OWEB.HUC8.Grant.Capacity.WC.12)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form.capacity.12 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT.OWEB.OWRI.wq.TotalCash.12+
#   OWEB.HUC8.Grant.Outreach.WC.12+
#   OWEB.HUC8.Grant.Tech.WC.12+
#   OWEB.HUC8.Grant.Restoration.WC.12+
#   OWEB.HUC8.Grant.Capacity.WC.12 + 
#   OWEB.HUC8.Grant.Outreach.WC.12:OWEB.HUC8.Grant.Capacity.WC.12+
#   OWEB.HUC8.Grant.Tech.WC.12:OWEB.HUC8.Grant.Capacity.WC.12+
#   OWEB.HUC8.Grant.Restoration.WC.12:OWEB.HUC8.Grant.Capacity.WC.12+
#   OWEB.HUC8.Grant.Capacity.WC.12:OWEB.HUC8.Grant.Outreach.WC.12:OWEB.HUC8.Grant.Tech.WC.12:OWEB.HUC8.Grant.Capacity.WC.12:OWEB.HUC8.Grant.Restoration.WC.12+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod.capacity.12 <- inla(form.capacity.12, family='gaussian', data=inla.stack.data(stk.1),
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
#           covars$NOT.OWEB.OWRI.wq.TotalCash.24,
#         #  covars$OWEB.HUC8.Grant.All.WC.24.ind,
#         #  covars$OWEB.HUC8.Grant.All.SWCD.24.ind,
#           covars$OWEB.HUC8.Grant.All.WC.24,
#           covars$OWEB.HUC8.Grant.All.SWCD.24,
#           covars$OWEB.HUC8.Grant.all.WC24* covars$OWEB.HUC8.Grant.All.SWCD.24)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form.base.24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 +dev.huc8 +  elev100m + monthly.precip.median + 
#   YEARS.ACTIVE +
#   NOT.OWEB.OWRI.wq.TotalCash.24 + 
#  # OWEB.HUC8.Grant.All.SWCD.24.ind+
# #  OWEB.HUC8.Grant.All.WC.24.ind+
#  # OWEB.HUC8.Grant.All.WC.24.ind+
#   #OWEB.HUC8.Grant.All.SWCD.24.ind+
#   OWEB.HUC8.Grant.All.WC.24+
#   OWEB.HUC8.Grant.All.SWCD.24+
#   OWEB.HUC8.Grant.All.WC.24:OWEB.HUC8.Grant.All.SWCD.24+
# #OWEB.HUC8.Grant.All.WC.24.ind:OWEB.HUC8.Grant.All.WC.24+
# #OWEB.HUC8.Grant.All.SWCD.24.ind:OWEB.HUC8.Grant.All.SWCD.24+
# #OWEB.HUC8.Grant.All.Both.24.ind:OWEB.HUC8.Grant.All.Both.24+
#   f(which.wc,model='iid',param=c(0.001,0.001))+
#   #f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod.base.24 <- inla(form.base.24, family='gaussian',
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
#           covars$YEARS.ACTIVE,
#           covars$NOT.OWEB.OWRI.wq.TotalCash.24,
#           covars$OWEB.HUC8.Grant.All.Both.24.ind,
# covars$OWEB.HUC8.Grant.Outreach.WC.24, 
# covars$OWEB.HUC8.Grant.Outreach.SWCD.24,
# covars$OWEB.HUC8.Grant.Tech.WC.24, 
# covars$OWEB.HUC8.Grant.Tech.SWCD.24,
# covars$OWEB.HUC8.Grant.Restoration.WC.24,
# covars$OWEB.HUC8.Grant.Restoration.SWCD.24,
# covars$OWEB.HUC8.Grant.Outreach.WC.24*
# covars$OWEB.HUC8.Grant.Outreach.SWCD.24*
# covars$OWEB.HUC8.Grant.Tech.WC.24*
# covars$OWEB.HUC8.Grant.Tech.SWCD.24*
# covars$OWEB.HUC8.Grant.Restoration.WC.24*
# covars$OWEB.HUC8.Grant.Restoration.SWCD.24
# )
# 
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# 
# 
# 
# form.project.24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   YEARS.ACTIVE + 
#   NOT.OWEB.OWRI.wq.TotalCash.24 + 
#   OWEB.HUC8.Grant.Outreach.WC.24+
#   OWEB.HUC8.Grant.Outreach.SWCD.24 + 
#   #OWEB.HUC8.Grant.Outreach.WC.24:OWEB.HUC8.Grant.Outreach.SWCD.24 + 
#   OWEB.HUC8.Grant.Tech.WC.24+
#   OWEB.HUC8.Grant.Tech.SWCD.24 + 
#   #OWEB.HUC8.Grant.Tech.WC.24:OWEB.HUC8.Grant.Tech.SWCD.24 + 
#   OWEB.HUC8.Grant.Restoration.WC.24 +
#   OWEB.HUC8.Grant.Restoration.SWCD.24 + 
#   OWEB.HUC8.Grant.Outreach.WC.24:OWEB.HUC8.Grant.Tech.WC.24:OWEB.HUC8.Grant.Restoration.WC.24:
#   OWEB.HUC8.Grant.Outreach.SWCD.24:OWEB.HUC8.Grant.Tech.SWCD.24:OWEB.HUC8.Grant.Restoration.SWCD.24 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#  # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod.project.24 <- inla(form.project.24, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept),
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
#           covars$NOT.OWEB.OWRI.wq.TotalCash.24,
# covars$OWEB.HUC8.Grant.Outreach.WC.24, 
# covars$OWEB.HUC8.Grant.Tech.WC.24,
# covars$OWEB.HUC8.Grant.Restoration.WC.24,
# covars$OWEB.HUC8.Grant.Capacity.WC.24)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form.capacity.24 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT.OWEB.OWRI.wq.TotalCash.24+
#   OWEB.HUC8.Grant.Outreach.WC.24+
#   OWEB.HUC8.Grant.Tech.WC.24+
#   OWEB.HUC8.Grant.Restoration.WC.24+
#   OWEB.HUC8.Grant.Capacity.WC.24 + 
#   OWEB.HUC8.Grant.Outreach.WC.24:OWEB.HUC8.Grant.Capacity.WC.24+
#   OWEB.HUC8.Grant.Tech.WC.24:OWEB.HUC8.Grant.Capacity.WC.24+
#   OWEB.HUC8.Grant.Restoration.WC.24:OWEB.HUC8.Grant.Capacity.WC.24+
#   OWEB.HUC8.Grant.Capacity.WC.24:OWEB.HUC8.Grant.Outreach.WC.24:OWEB.HUC8.Grant.Tech.WC.24:OWEB.HUC8.Grant.Capacity.WC.24:OWEB.HUC8.Grant.Restoration.WC.24+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#  # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod.capacity.24 <- inla(form.capacity.24, family='gaussian', data=inla.stack.data(stk.1),
#                         control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                         #  control.inla=list(strategy='laplace'), 
#                         control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                         control.fixed= list(prec.intercept = pintercept),
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
#           covars$NOT.OWEB.OWRI.wq.TotalCash.36,
# covars$OWEB.HUC8.Grant.All.WC.36,
# covars$OWEB.HUC8.Grant.All.SWCD.36
# )
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form.base.36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 +dev.huc8 +  elev100m + monthly.precip.median + 
#   NOT.OWEB.OWRI.wq.TotalCash.36 + 
#   OWEB.HUC8.Grant.All.WC.36+
#   OWEB.HUC8.Grant.All.SWCD.36+
#   OWEB.HUC8.Grant.All.WC.36:OWEB.HUC8.Grant.All.SWCD.36+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod.base.36 <- inla(form.base.36, family='gaussian',
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
#           #covars$Ag,
#           #covars$Forst,
#           #covars$Dev,  
#           covars$ag.huc8, 
#           covars$forst.huc8,
#           covars$dev.huc8,
#           covars$elev100m,
#           covars$monthly.precip.median, 
#           covars$YEARS.ACTIVE,
#           covars$NOT.OWEB.OWRI.wq.TotalCash.36,
#           #  covars$OWEB.HUC8.Grant.All.WC.36.ind,
#           #  covars$OWEB.HUC8.Grant.All.SWCD.36.ind,
#           covars$OWEB.HUC8.Grant.All.WC.36,
#           covars$OWEB.HUC8.Grant.All.SWCD.36,
#           covars$OWEB.HUC8.Grant.all.WC36* covars$OWEB.HUC8.Grant.All.SWCD.36)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# 
# form.project.36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + #Ag + Forst + Dev  + 
#   ag.huc8+
#   forst.huc8 + dev.huc8 + elev100m +  
#   monthly.precip.median + 
#   YEARS.ACTIVE + 
#   NOT.OWEB.OWRI.wq.TotalCash.36 + 
#   OWEB.HUC8.Grant.All.Both.36.ind + 
#   OWEB.HUC8.Grant.Outreach.WC.36+
#   OWEB.HUC8.Grant.Outreach.SWCD.36 + 
#   #OWEB.HUC8.Grant.Outreach.WC.36:OWEB.HUC8.Grant.Outreach.SWCD.36 + 
#   OWEB.HUC8.Grant.Tech.WC.36+
#   OWEB.HUC8.Grant.Tech.SWCD.36 + 
#   #OWEB.HUC8.Grant.Tech.WC.36:OWEB.HUC8.Grant.Tech.SWCD.36 + 
#   OWEB.HUC8.Grant.Restoration.WC.36 +
#   OWEB.HUC8.Grant.Restoration.SWCD.36 + 
#   OWEB.HUC8.Grant.Outreach.WC.36:OWEB.HUC8.Grant.Tech.WC.36:OWEB.HUC8.Grant.Restoration.WC.36:
#   OWEB.HUC8.Grant.Outreach.SWCD.36:OWEB.HUC8.Grant.Tech.SWCD.36:OWEB.HUC8.Grant.Restoration.SWCD.36 + 
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   # f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# 
# mod.project.36 <- inla(form.project.36, family='gaussian', data=inla.stack.data(stk.1),
#                        control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
#                        #  control.inla=list(strategy='laplace'), 
#                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
#                        control.fixed= list(prec.intercept = pintercept),
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
#           covars$NOT.OWEB.OWRI.wq.TotalCash.36,
# covars$OWEB.HUC8.Grant.Outreach.WC.36, 
# covars$OWEB.HUC8.Grant.Tech.WC.36,
# covars$OWEB.HUC8.Grant.Restoration.WC.36,
# covars$OWEB.HUC8.Grant.Capacity.WC.36)
# n.covariates = ncol(X)
# Q = qr.Q(qr(X))
# 
# form.capacity.36 <-  y ~ 0 + b0 + Decimal_Lat + Decimal_long + + 
#   #Ag + Forst + Dev  + 
#   dev.huc8 + ag.huc8+
#   forst.huc8 + elev100m +  
#   monthly.precip.median + 
#   NOT.OWEB.OWRI.wq.TotalCash.36+
#   OWEB.HUC8.Grant.Outreach.WC.36+
#   OWEB.HUC8.Grant.Tech.WC.36+
#   OWEB.HUC8.Grant.Restoration.WC.36+
#   OWEB.HUC8.Grant.Capacity.WC.36 + 
#   OWEB.HUC8.Grant.Outreach.WC.36:OWEB.HUC8.Grant.Capacity.WC.36+
#   OWEB.HUC8.Grant.Tech.WC.36:OWEB.HUC8.Grant.Capacity.WC.36+
#   OWEB.HUC8.Grant.Restoration.WC.36:OWEB.HUC8.Grant.Capacity.WC.36+
#   OWEB.HUC8.Grant.Capacity.WC.36:OWEB.HUC8.Grant.Outreach.WC.36:OWEB.HUC8.Grant.Tech.WC.36:OWEB.HUC8.Grant.Capacity.WC.36:OWEB.HUC8.Grant.Restoration.WC.36+
#   f(HUC8,model='iid',param=c(0.001,0.001))+ 
#   f(YEAR,model='iid',param=c(0.001,0.001))+ 
#   # f(total.period,model='rw2') + 
#   f(total.period,model='ar1',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) + 
#   f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=12)+  
#   f(s, model=spde.a,
#     extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.covariates)))
# 
# mod.capacity.36 <- inla(form.capacity.36, family='gaussian', data=inla.stack.data(stk.1),
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

save.image('test.results.oneout.RData')

library(mail)
sendmail('tyler.andrew.scott@gmail.com','INLA model done')
