
rm(list = ls(all = TRUE))
setwd('H:/quinalt')
load('midpoint.7.RData')
require(xtable)
library(INLA)
library(rgdal);library(rgeos);library(maptools)

test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 

temp = params.spdf@data

covars = temp[,c(c('ag.huc8','dev.huc8','forst.huc8','wet.huc8','elevation','seaDist','HUC8','total.period',
                 'seasonal','Ag','Dev','Wetl','Forst'),c(grep('rsum',colnames(temp),value=T)))]


#cREATE MESH, SPDE
#note: mesh, spde object used for models 1-7
(mesh.a <- inla.mesh.2d(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT), max.edge=c(40, 80),cut=.20))$n
spde.a <- inla.spde2.matern(mesh.a) 

###


####MODELS 3-4: RANDOM EFFECT CORRELATED BETWEEN YEARS
##MODELS 3-4 USE SAME STACK
table(gr.3 <- temp$YEAR-(min(temp$YEAR)-1))
dim(A.3 <- inla.spde.make.A(mesh.a, group=gr.3, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT)))
ind.3 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.group=length(unique(gr.3)))
stk.3 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.3,1),
                    effects=list(ind.3, list(data.frame(b0=1,covars))))

#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
form.rest.nonrest.1yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.1yr+ rsum.oweb.non.rest.1yr+
  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

form.rest.nonrest.3yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.3yr+ rsum.oweb.non.rest.3yr+
  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

form.rest.nonrest.5yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.5yr+ rsum.oweb.non.rest.5yr+
  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))


mod.1yr.rest.nonrest <- inla(form.rest.nonrest.1yr, family='gaussian', data=inla.stack.data(stk.3),
             control.predictor=list(A=inla.stack.A(stk.3), compute=T),
             # control.inla=list(strategy='laplace'), 
             control.compute=list(dic=TRUE, cpo=TRUE))

mod.3yr.rest.nonrest <- inla(form.rest.nonrest.3yr, family='gaussian', data=inla.stack.data(stk.3),
                         control.predictor=list(A=inla.stack.A(stk.3), compute=T),
                         # control.inla=list(strategy='laplace'), 
                         control.compute=list(dic=TRUE, cpo=TRUE))


mod.5yr.rest.nonrest <- inla(form.rest.nonrest.5yr, family='gaussian', data=inla.stack.data(stk.3),
                             control.predictor=list(A=inla.stack.A(stk.3), compute=T),
                             # control.inla=list(strategy='laplace'), 
                             control.compute=list(dic=TRUE, cpo=TRUE))

mod.1yr.rest.nonrest$summary.fixed


save.image('compare.rest.non.rest.results.RData')
