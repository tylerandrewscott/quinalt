
rm(list = ls(all = TRUE))
setwd('/homes/tscott1/win/user/quinalt')
load('midpoint.6.v2.RData');rm(list=ls()[ls()!='huc8.database'])
load('midpoint.7.v2.RData')
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
(mesh.a <- inla.mesh.2d(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT), max.edge=c(5, 40),cut=.05))$n
spde.a <- inla.spde2.matern(mesh.a) 
plot(mesh.a)
###
cor3mat = matrix(paste(
paste(
sprintf('%.2f',
round(cor(temp[,c('rsum.oweb.tech.1yr','rsum.oweb.ed.1yr',
                  'rsum.oweb.rest.1yr','rsum.oweb.council.1yr')]),2)),
sprintf('%.2f',round(cor(temp[,c('rsum.oweb.tech.3yr','rsum.oweb.ed.3yr',
                                 'rsum.oweb.rest.3yr','rsum.oweb.council.3yr')]),2)),
sep='/'),
sprintf('%.2f',round(cor(temp[,c('rsum.oweb.tech.5yr','rsum.oweb.ed.5yr',
                                 'rsum.oweb.rest.5yr','rsum.oweb.council.5yr')]),2)),
sep='/'),ncol=4)

cor3mat[upper.tri(cor3mat,diag=T)] <- NA
colnames(cor3mat) = c('Science/Tech.','Ed./Outreach','Rest.','Council')
rownames(cor3mat) = c('Science/Tech.','Ed./Outreach','Rest.','Council')
library(stargazer)
stargazer(
cor3mat,out='grantcorrelationtable.tex',
          title='Correlation amongst grant types (12/36/60 month values)',
          label='table:cormatrix',type='latex')



# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, loc=cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

form1 <-  y ~ 0 + b0 + Ag + Forst + Dev  + elevation + seaDist + f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)


form1 <-  y ~ 0 + b0 + Ag + Forst + Dev  + elevation + seaDist +
  
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)
mod1 <- inla(form1, family='gaussian', data=inla.stack.data(stk.1),
             control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
             #  control.inla=list(strategy='laplace'), 
             control.compute=list(dic=TRUE, cpo=TRUE))

#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
form.diff.grants.1yr.all <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.1yr+rsum.oweb.rest.1yr+rsum.oweb.ed.1yr+rsum.oweb.council.1yr+
  rsum.oweb.tech.1yr:rsum.oweb.rest.1yr:rsum.oweb.ed.1yr:rsum.oweb.council.1yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.1yr.tech <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.1yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.1yr.ed <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.ed.1yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.1yr.council <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.council.1yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.1yr.rest <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.rest.1yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)


mod.1yr.diff.grants.all <- inla(form.diff.grants.1yr.all, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))

mod.1yr.diff.grants.ed <- inla(form.diff.grants.1yr.ed, family='gaussian', data=inla.stack.data(stk.1),
                            control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                            # control.inla=list(strategy='laplace'), 
                            control.compute=list(dic=TRUE, cpo=TRUE))
mod.1yr.diff.grants.tech <- inla(form.diff.grants.1yr.tech, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))
mod.1yr.diff.grants.council <- inla(form.diff.grants.1yr.council, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))
mod.1yr.diff.grants.rest <- inla(form.diff.grants.1yr.rest, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))

form.diff.grants.3yr.all <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.3yr+rsum.oweb.rest.3yr+rsum.oweb.ed.3yr+rsum.oweb.council.3yr+
  rsum.oweb.tech.3yr:rsum.oweb.rest.3yr:rsum.oweb.ed.3yr:rsum.oweb.council.3yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.3yr.tech <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.3yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.3yr.ed <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.ed.3yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.3yr.council <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.council.3yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.3yr.rest <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.rest.3yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

mod.3yr.diff.grants.all <- inla(form.diff.grants.3yr.all, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))

mod.3yr.diff.grants.ed <- inla(form.diff.grants.3yr.ed, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))
mod.3yr.diff.grants.tech <- inla(form.diff.grants.3yr.tech, family='gaussian', data=inla.stack.data(stk.1),
                                 control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                 # control.inla=list(strategy='laplace'), 
                                 control.compute=list(dic=TRUE, cpo=TRUE))
mod.3yr.diff.grants.council <- inla(form.diff.grants.3yr.council, family='gaussian', data=inla.stack.data(stk.1),
                                    control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                    # control.inla=list(strategy='laplace'), 
                                    control.compute=list(dic=TRUE, cpo=TRUE))
mod.3yr.diff.grants.rest <- inla(form.diff.grants.3yr.rest, family='gaussian', data=inla.stack.data(stk.1),
                                 control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                 # control.inla=list(strategy='laplace'), 
                                 control.compute=list(dic=TRUE, cpo=TRUE))

form.diff.grants.5yr.all <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.5yr+rsum.oweb.rest.5yr+rsum.oweb.ed.5yr+rsum.oweb.council.5yr+
  rsum.oweb.tech.5yr:rsum.oweb.rest.5yr:rsum.oweb.ed.5yr:rsum.oweb.council.5yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.5yr.tech <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.tech.5yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.5yr.ed <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.ed.5yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.5yr.council <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.council.5yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

form.diff.grants.5yr.rest <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  + 
  rsum.oweb.rest.5yr+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') +
  f(seasonal,model='seasonal',season.length=12)+  f(s, model=spde.a)

mod.5yr.diff.grants.all <- inla(form.diff.grants.5yr.all, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))

mod.5yr.diff.grants.ed <- inla(form.diff.grants.5yr.ed, family='gaussian', data=inla.stack.data(stk.1),
                               control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                               # control.inla=list(strategy='laplace'), 
                               control.compute=list(dic=TRUE, cpo=TRUE))
mod.5yr.diff.grants.tech <- inla(form.diff.grants.5yr.tech, family='gaussian', data=inla.stack.data(stk.1),
                                 control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                 # control.inla=list(strategy='laplace'), 
                                 control.compute=list(dic=TRUE, cpo=TRUE))
mod.5yr.diff.grants.council <- inla(form.diff.grants.5yr.council, family='gaussian', data=inla.stack.data(stk.1),
                                    control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                    # control.inla=list(strategy='laplace'), 
                                    control.compute=list(dic=TRUE, cpo=TRUE))
mod.5yr.diff.grants.rest <- inla(form.diff.grants.5yr.rest, family='gaussian', data=inla.stack.data(stk.1),
                                 control.predictor=list(A=inla.stack.A(stk.1), compute=T),quantiles=c(0.025,0.05, 0.5,0.95, 0.975),
                                 # control.inla=list(strategy='laplace'), 
                                 control.compute=list(dic=TRUE, cpo=TRUE))


##############

coefs.1yr = c(
paste(paste(
paste(
  sprintf('%.3f',round(exp(mod.1yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.1yr',c('mean')],3)),
          sprintf('%.3f',round(exp(mod.1yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.1yr',c('0.025quant')],3)),sep=' ('),
          sprintf('%.3f',round(exp(mod.1yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.1yr',c('0.975quant')],3)),sep=', '
),')',sep='')
,
paste(paste(
  paste(
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.1yr',c('mean')],3)),
            sprintf('%.3f',round(exp(mod.1yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.1yr',c('0.025quant')],3)),sep=' ('),
            sprintf('%.3f',round(exp(mod.1yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.1yr',c('0.975quant')],3)),sep=', '
),')',sep='')
,
paste(paste(
  paste(
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.1yr',c('mean')],3)),
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.1yr',c('0.025quant')],3)),sep=' ('),
  sprintf('%.3f',round(exp(mod.1yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.1yr',c('0.975quant')],3)),sep=', '
),')',sep='')
,
paste(paste(
  paste(
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.council$summary.fixed)['rsum.oweb.council.1yr',c('mean')],3)),
            sprintf('%.3f',round(exp(mod.1yr.diff.grants.council$summary.fixed)['rsum.oweb.council.1yr',c('0.025quant')],3)),sep=' ('),
            sprintf('%.3f',round(exp(mod.1yr.diff.grants.council$summary.fixed)['rsum.oweb.council.1yr',c('0.975quant')],3)),sep=', '
),')',sep=''),
paste(paste(
  paste(
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.all$summary.fixed)[11,c('mean')],4)),
    sprintf('%.3f',round(exp(mod.1yr.diff.grants.all$summary.fixed)[11,c('0.025quant')],4)),sep=' ('),
  sprintf('%.3f',round(exp(mod.1yr.diff.grants.all$summary.fixed)[11,c('0.975quant')],4)),sep=', '
),')',sep='')
)




coefs.3yr = c(
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.3yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.3yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f', round(exp(mod.3yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.3yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.3yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.3yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.3yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.3yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.3yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.3yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.council$summary.fixed)['rsum.oweb.council.3yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.3yr.diff.grants.council$summary.fixed)['rsum.oweb.council.3yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f', round(exp(mod.3yr.diff.grants.council$summary.fixed)['rsum.oweb.council.3yr',c('0.975quant')],3)),sep=', '
  ),')',sep=''),
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.all$summary.fixed)[11,c('mean')],4)),
      sprintf('%.3f',round(exp(mod.3yr.diff.grants.all$summary.fixed)[11,c('0.025quant')],4)),sep=' ('),
    sprintf('%.3f', round(exp(mod.3yr.diff.grants.all$summary.fixed)[11,c('0.975quant')],4)),sep=', '
  ),')',sep='')
)


coefs.5yr = c(
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.5yr',c('mean')],3)),
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.5yr',c('0.025quant')],3)),sep=' ('),
    sprintf('%.3f',round(exp(mod.5yr.diff.grants.rest$summary.fixed)['rsum.oweb.rest.5yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.5yr',c('mean')],3)),
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.5yr',c('0.025quant')],3)),sep=' ('),
    sprintf('%.3f',round(exp(mod.5yr.diff.grants.ed$summary.fixed)['rsum.oweb.ed.5yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.5yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.5yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.5yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f',round(exp(mod.5yr.diff.grants.tech$summary.fixed)['rsum.oweb.tech.5yr',c('0.975quant')],3)),sep=', '
  ),')',sep='')
  ,
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.council$summary.fixed)['rsum.oweb.council.5yr',c('mean')],3)),
              sprintf('%.3f',round(exp(mod.5yr.diff.grants.council$summary.fixed)['rsum.oweb.council.5yr',c('0.025quant')],3)),sep=' ('),
              sprintf('%.3f',round(exp(mod.5yr.diff.grants.council$summary.fixed)['rsum.oweb.council.5yr',c('0.975quant')],3)),sep=', '
  ),')',sep=''),
  paste(paste(
    paste(
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.all$summary.fixed)[11,c('mean')],4)),
      sprintf('%.3f',round(exp(mod.5yr.diff.grants.all$summary.fixed)[11,c('0.025quant')],4)),sep=' ('),
    sprintf('%.3f',round(exp(mod.5yr.diff.grants.all$summary.fixed)[11,c('0.975quant')],4)),sep=', '
  ),')',sep='')
)


tt = data.frame(coefs.1yr,coefs.3yr,coefs.5yr)
colnames(tt) = c('12 months (0.025,0.975)','36 months (0.025,0.975)','60 months (0.025,0.975)')
rownames(tt) = c('Restoration ($100k)','Education/Outreach ($100k)','Scientific/Technical ($100k)',
                 'Council Support ($100k)','Interaction ($100k)')
library(xtable)
library(stargazer)



stargazer(tt,type='latex',title='Comparing Grant Effects by Type',table.placement='!hbtp',
          label='table:diffgrants',
          out='diffgrantscoeftable.tex',summary=F)


#mean grant values:
tech 41972
council 104154
ed 23515
rest 69498
all 65345


save.image('compare.diff.grants.results.RData')


