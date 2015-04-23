
rm(list = ls(all = TRUE))
setwd('H:/quinalt')
load('midpoint.7.RData')
require(xtable)
library(INLA)
#library(rgdal);library(rgeos)
library(maptools)

#test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 

temp = params.spdf@data


covars = temp[,c('ag.huc8','dev.huc8','forst.huc8','wet.huc8','elevation','seaDist','HUC8','total.period',
                 'seasonal','Ag','Dev','Wetl','Forst','rsum.oweb.all.3yr','rsum.oweb.all.1yr','rsum.oweb.all.5yr',
                'fund.bin.1yr','fund.bin.3yr','fund.bin.5yr')]

##
#plot(test,border='grey90',col='grey80',
#    xlim = range(mesh.a$loc[, 1]),
#   ylim = range(mesh.a$loc[, 2]))
#plot(mesh.a, asp=1,main=NULL,sub=NULL,add=T)
#points(temp$DECIMAL_LONG[!duplicated(temp$DECIMAL_LONG)],
#       temp$DECIMAL_LAT[!duplicated(temp$DECIMAL_LAT)],col='blue',pch=21)
#legend(x=-116.5,y=41.75,legend='Station',pch=21,col='blue',pt.cex=1.5)

#pts <- params.spdf@coords
#or.bond <- inla.nonconvex.hull(pts, 2, 2)
#or.bond1 <- inla.nonconvex.hull(pts,convex=.25)

#cREATE MESH, SPDE
#note: mesh, spde object used for models 1-7

#or.bond = inla.nonconvex.hull(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT),2,2)
(mesh.a <- inla.mesh.2d(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT),max.edge=c(5, 40),cut=.05))$n

spde.a <- inla.spde2.matern(mesh.a) 
plot(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, loc=cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

form1 <-  y ~ 0 + b0 + Ag + Forst + Dev  + elevation + seaDist + f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)
mod1 <- inla(form1, family='gaussian', data=inla.stack.data(stk.1),
             control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
             #  control.inla=list(strategy='laplace'), 
             control.compute=list(dic=TRUE, cpo=TRUE))

#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
# form.all.1yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
#   elevation + seaDist  + rsum.oweb.all.1yr +  f(HUC8,model='iid')+
#   f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
#   f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

form.all.1yr <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.1yr + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

form.all.3yr <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.3yr + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

form.all.5yr <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  rsum.oweb.all.5yr + 
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

# 
# form.all.3yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
#   elevation + seaDist  + rsum.oweb.all.3yr+  f(HUC8,model='iid')+
#   f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
#   f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))
# 
# 
# form.all.5yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
#   elevation + seaDist  + rsum.oweb.all.5yr+  f(HUC8,model='iid')+
#   f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
#   f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

mod.all.1yr <- inla(form.all.1yr, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.3yr <- inla(form.all.3yr, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.5yr <- inla(form.all.5yr,family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

print(xtable(exp(mod.all.1yr$summary.fixed),digits=4),type='html','mod.all.1yr.table.html')
print(xtable(exp(mod.all.3yr$summary.fixed),digits=4),type='html','mod.all.3yr.table.html')
print(xtable(exp(mod.all.5yr$summary.fixed),digits=4),type='html','mod.all.5yr.table.html')

dic.scores.all.grants = data.frame(mod.all.1yr$dic$dic,
                                   mod.all.3yr$dic$dic,
                                   mod.all.5yr$dic$dic)
colnames(dic.scores.all.grants) = c(paste('Model ',c('1yr','3yr','5yr'),sep=''))
dic.scores.all.grants[1,] = round(dic.scores.all.grants[1,])

write.table(dic.scores.all.grants,'H:/quinalt/dic.score.all.grants.table.txt')

library(stargazer)



coef.all.1yr = data.frame(value1 = c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.1yr$summary.fixed[,'mean']),3)),
              sprintf('%.3f',round(exp(mod.all.1yr$summary.fixed[,'0.025quant']),3)),sep=' ('),
      sprintf('%.3f',round(exp(mod.all.1yr$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
      round(mod.all.1yr$dic$dic,1))
      ,
      coefs = c(
        "\\% Agric. (1000m buffer)",
        '\\% Forest (1000m buffer)',
        '\\% Devel. (1000m buffer)',
        'Elevation (10m)',
        'Dist. from coast (10km)',
        '12 month total ($100k)','DIC:')
)



coef.all.3yr = data.frame(value2=c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.3yr$summary.fixed[,'mean']),3)),
                   sprintf('%.3f',round(exp(mod.all.3yr$summary.fixed[,'0.025quant']),3)),sep=' ('),
             sprintf('%.3f',round(exp(mod.all.3yr$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
             round(mod.all.3yr$dic$dic,1))
,
coefs = c(
  "\\% Agric. (1000m buffer)",
  '\\% Forest (1000m buffer)',
  '\\% Devel. (1000m buffer)',
  'Elevation (10m)',
  'Dist. from coast (10km)',
  '36 month total ($100k)','DIC:')
)

coef.all.5yr = data.frame(value3 = c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.5yr$summary.fixed[,'mean']),3)),
                   sprintf('%.3f',round(exp(mod.all.5yr$summary.fixed[,'0.025quant']),3)),sep=' ('),
             sprintf('%.3f',round(exp(mod.all.5yr$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
             round(mod.all.5yr$dic$dic,1))
,
coefs = c(
  "\\% Agric. (1000m buffer)",
  '\\% Forest (1000m buffer)',
  '\\% Devel. (1000m buffer)',
  'Elevation (10m)',
  'Dist. from coast (10km)',
  '60 month total ($100k)','DIC:')
)

coef.all.5yr
coef.names = data.frame(coefs = c(
"\\% Agric. (1000m buffer)",
'\\% Forest (1000m buffer)',
'\\% Devel. (1000m buffer)',
'Elevation (10m)',
'Dist. from coast (10km)',
'12 month total ($100k)',
'36 month total ($100k)',
'60 month total ($100k)',
'DIC:'))


coef.table = join_all(list(coef.names,coef.all.1yr,coef.all.3yr,coef.all.5yr))
rownames(coef.table) = coef.table$coefs
coef.table = coef.table[,-1]

colnames(coef.table) = c('1 year sum','3 year sum','5 year sum')
#c('$\\hat{\beta} (0.025,0.975)$','$\\hat{\beta} (0.025,0.975)$','$\\hat{\beta} (0.025,0.975)$')


stargazer(coef.table,label='table:basecoefs',out='modelcoefs.tex',type='latex',summary=F,
          title = 'Posterior parameter estimates, all grant types')


save.image('test.total.grants.results.RData')
