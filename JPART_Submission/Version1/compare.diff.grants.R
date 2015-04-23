
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

form.diff.grants.1yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.1yr+
  rsum.oweb.ed.1yr+
  rsum.oweb.tech.1yr+
  rsum.oweb.council.1yr+
  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

form.diff.grants.3yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.3yr+ 
  rsum.oweb.ed.3yr+
  rsum.oweb.tech.3yr+
  rsum.oweb.council.3yr+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

form.diff.grants.5yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.rest.5yr+ 
  rsum.oweb.ed.5yr+
  rsum.oweb.tech.5yr+
  rsum.oweb.council.5yr+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))


mod.1yr.diff.grants <- inla(form.diff.grants.1yr, family='gaussian', data=inla.stack.data(stk.3),
             control.predictor=list(A=inla.stack.A(stk.3), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
             # control.inla=list(strategy='laplace'), 
             control.compute=list(dic=TRUE, cpo=TRUE))

mod.3yr.diff.grants <- inla(form.diff.grants.3yr, family='gaussian', data=inla.stack.data(stk.3),
                         control.predictor=list(A=inla.stack.A(stk.3), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                         # control.inla=list(strategy='laplace'), 
                         control.compute=list(dic=TRUE, cpo=TRUE))


mod.5yr.diff.grants <- inla(form.diff.grants.5yr, family='gaussian', data=inla.stack.data(stk.3),
                             control.predictor=list(A=inla.stack.A(stk.3), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                             # control.inla=list(strategy='laplace'), 
                             control.compute=list(dic=TRUE, cpo=TRUE))

print(xtable(exp(mod.1yr.diff.grants$summary.fixed),digits=3),type='html','mod.diff.grants.1yr.table.html')
print(xtable(exp(mod.3yr.diff.grants$summary.fixed),digits=3),type='html','mod.diff.grants.3yr.table.html')
print(xtable(exp(mod.5yr.diff.grants$summary.fixed),digits=3),type='html','mod.diff.grants.5yr.table.html')

dic.scores.diff.grants = data.frame(mod.1yr.diff.grants$dic$dic,
                                   mod.3yr.diff.grants$dic$dic,
                                   mod.5yr.diff.grants$dic$dic)
colnames(dic.scores.diff.grants) = c(paste('Model ',c('1yr','3yr','5yr'),sep=''))
dic.scores.diff.grants[1,] = round(dic.scores.diff.grants[1,])

write.table(dic.scores.diff.grants,'H:/quinalt/dic.score.diff.grants.table.txt')

save.image('compare.diff.grants.results.RData')
