
rm(list = ls(all = TRUE))
setwd('H:/quinalt')
load('midpoint.7.RData')
require(xtable)
library(INLA)
library(rgdal);library(rgeos);library(maptools)

test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 

temp = params.spdf@data


covars = temp[,c('ag.huc8','dev.huc8','forst.huc8','wet.huc8','elevation','seaDist','HUC8','total.period',
                 'seasonal','Ag','Dev','Wetl','Forst','rsum.oweb.all.3yr','rsum.oweb.all.1yr','rsum.oweb.all.5yr')]

##
#plot(test,border='grey90',col='grey80',
#    xlim = range(mesh.a$loc[, 1]),
#   ylim = range(mesh.a$loc[, 2]))
#plot(mesh.a, asp=1,main=NULL,sub=NULL,add=T)
#points(temp$DECIMAL_LONG[!duplicated(temp$DECIMAL_LONG)],
#       temp$DECIMAL_LAT[!duplicated(temp$DECIMAL_LAT)],col='blue',pch=21)
#legend(x=-116.5,y=41.75,legend='Station',pch=21,col='blue',pt.cex=1.5)



#cREATE MESH, SPDE
#note: mesh, spde object used for models 1-7
(mesh.a <- inla.mesh.2d(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT), max.edge=c(40, 80),cut=.20))$n
spde.a <- inla.spde2.matern(mesh.a) 


####MODELS 3-4: RANDOM EFFECT CORRELATED BETWEEN YEARS
##MODELS 3-4 USE SAME STACK
table(gr.3 <- temp$YEAR-(min(temp$YEAR)-1))
dim(A.3 <- inla.spde.make.A(mesh.a, group=gr.3, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT)))
ind.3 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.group=length(unique(gr.3)))
stk.3 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.3,1),
                    effects=list(ind.3, list(data.frame(b0=1,covars))))

#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
form.all.1yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.all.1yr+  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))


form.all.3yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.all.3yr+  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))


form.all.5yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + rsum.oweb.all.5yr+  f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

mod.all.1yr <- inla(form.all.1yr, family='gaussian', data=inla.stack.data(stk.3),
                    control.predictor=list(A=inla.stack.A(stk.3), compute=T),
                    # control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.3yr <- inla(form.all.3yr, family='gaussian', data=inla.stack.data(stk.3),
             control.predictor=list(A=inla.stack.A(stk.3), compute=T),
             # control.inla=list(strategy='laplace'), 
             control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.5yr <- inla(form.all.5yr, family='gaussian', data=inla.stack.data(stk.3),
              control.predictor=list(A=inla.stack.A(stk.3), compute=T),
              # control.inla=list(strategy='laplace'), 
              control.compute=list(dic=TRUE, cpo=TRUE))


print(xtable(exp(mod.all.1yr$summary.fixed),digits=3),type='html','mod.all.1yr.table.html')
print(xtable(exp(mod.all.3yr$summary.fixed),digits=3),type='html','mod.all.3yr.table.html')
print(xtable(exp(mod.all.5yr$summary.fixed),digits=3),type='html','mod.all.5yr.table.html')

save.image('rolling.sum.comparison.results.RData')
