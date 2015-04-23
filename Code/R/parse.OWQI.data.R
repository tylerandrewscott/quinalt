setwd("H:/quinalt")

dat = read.csv('Scott_OWQI_1980_2013.csv')
dat = dat %>% filter(water_yr>1994)
dat$HUC8 = temp$HUC8[match(dat$Station,temp$STATION_KEY)]
#dat$OWEB.Grants.InYear = temp$cum.oweb.huc8.All[match(dat$Station,temp$STATION_KEY)]
dat$DECIMAL_LAT = temp$DECIMAL_LAT[match(dat$Station,temp$STATION_KEY)]
dat$DECIMAL_LONG = temp$DECIMAL_LONG[match(dat$Station,temp$STATION_KEY)]

tab = table(dat$water_yr,dat$Station)

numobs = as.data.frame(colSums(tabtf))
colnames(numobs) = 'Freq'

numobs = subset(numobs,numobs$Freq>10)

dat = filter(dat,Station %in% rownames(numobs))

library(lubridate)

dat$Month = month(mdy(dat$Date))
dat$water_yr

n_stat = length(unique(dat$Station))
n_year = length(unique(dat$water_yr))

uq = paste(dat$Station,dat$Month,dat$water_yr)
dat = dat[!duplicated(uq),]

dat$yrmonth = paste(dat$water_yr,dat$Month)


ym = NULL

for (i in 1995:2013)
{
  ym = c(ym,paste(i,seq(1,12,1)))
}
obs_matrix = matrix(0,ncol=length(ym),nrow=n_stat)
colnames(obs_matrix) = ym
rownames(obs_matrix) = sort(unique(dat$Station))


for (i in 1:nrow(dat))
{
  obs_matrix[match(dat$Station[i],rownames(obs_matrix)),match(dat$yrmonth[i],colnames(obs_matrix))] = dat$owqi[i]
}  
  
obs_matrix[obs_matrix==0] = NA
#forward fill NA observations (all but first column)
for (i in 1:nrow(obs_matrix))
{
  for (j in 2:ncol(obs_matrix))
  {
    if(is.na(obs_matrix[i,j])){obs_matrix[i,j] = obs_matrix[i,(j-1)]}
  }
}


for (i in 1:nrow(obs_matrix))
{
  for (j in (ncol(obs_matrix)-1):1)
  {
    if(is.na(obs_matrix[i,j])){obs_matrix[i,j] = obs_matrix[i,(j+1)]}
  }
}

dat


#############################

x = dat$DECIMAL_LONG - min(dat$DECIMAL_LONG)
y = dat$DECIMAL_LAT - min(dat$DECIMAL_LAT)

summary(x)
summary(y)

require(spatstat)
win <- owin(c(0,4.2),c(0,7.6))

spatstat.options(npixel=300)

beta0 <- 3

exp(beta0) * diff(range(win$x)) * diff(range(win$y))

sigma2x <- 0.2; kappa <- 2

set.seed(1)
lg.s <- rLGCP('matern', beta0,c(0, variance=sigma2x, nugget=0, scale=1/kappa, nu=1), win=win)

help(package='spatstat')

(n <- nrow(xy <- cbind(lg.s$x, lg.s$y)[,2:1]))






Lam <- attr(lg.s,'Lambda')
summary(as.vector(rf.s <- log(Lam$v)))
par(mfrow=c(1,1))
require(fields)
image.plot(list(x=Lam$yrow, y=Lam$xcol, z=rf.s), main='log-Lambda' , asp=1)
points(xy, pch=19)


loc.d <- 3*t(matrix(c(0,0,1,0,1,1,0,1,0,0), 2))
loc.d
(nv <- (mesh <- inla.mesh.2d(loc.d=loc.d, off=.2, max.e=.5, cut=.1))$n)
plot(mesh)
points(x,y)
source("http://www.math.ntnu.no/inla/givemeINLA.R")

require(INLA)
library(zoo)
na.locf(obs_matrix)
?na.locf
#backfill first column
obs_matrix[,1][is.na(obs_matrix[,1])] = obs_matrix[,2][is.na(obs_matrix[,1])]

obs_matrix


help(package='dplyr')
?paste
sort(unique(dat$yrmonth))

colnames(obs_matrix) =
  
  
  
unique(dat$yrmonth)




table(dat$water_yr,dat$Month,dat$Station)


head(numobs)
tabtf = tab>0
dim(tabtf)
sum(colSums(tabtf)>=19)



head(yearly.oweb.huc8.all)
head(huc8.projects)
temp = all.params.spdf@data





