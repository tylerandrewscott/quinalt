install.packages('quantmod')
library(quantmod)

getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items

tail(CPIAUCSL)

devtools::install_github('FredR','jcizel')
library(FredR)


# make an `xts` object of prices
set.seed(1)
p <- xts(rnorm(63, mean=10, sd=3), seq(from=as.Date('1950-12-01'), by='years', length.out=63))
colnames(p) <- "price"

avg.cpi <- apply.yearly(CPIAUCSL, mean)

cf <- avg.cpi/as.numeric(avg.cpi['2008']) #using 2008 as the base year
dat <- merge(p, cf, all=FALSE)
dat$adj <- dat[, 1] * dat[, 2]



#Define the formula
formula  = sqrt(y) ~ belt + f(trend,model="rw2",param=c(1,0.0005)) +
  f(seasonal,model="seasonal",season.length=12,param=c(1,0.1))

#lounch the inla function 
mod = inla(formula, family="gaussian", data=Drivers,
           control.family=list(param=c(4,4)))



