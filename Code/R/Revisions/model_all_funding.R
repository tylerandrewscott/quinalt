rm(list=ls())

setwd('/homes/tscott1/win/user/quinalt/')

load("temp_workspace_noprecip.RData")

library(plyr)
library(dplyr)
library(INLA)
require(xtable)
library(maptools)
library(mail)

#test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 


mod.data = all.params.spdf@data
mod.data$seasonal = mod.data$Abs.Month
mod.data$total.period = mod.data$Abs.Month
mod.data$sq.owqi = ((as.numeric(as.character(mod.data$owqi)))^2)
mod.data$l.owqi = log(as.numeric(as.character(mod.data$owqi)))
mod.data = filter(mod.data,YEAR>=1995)
mod.data$HUC8 = as.character(mod.data$HUC8)

covars = mod.data[,c('elevation','seaDist','HUC8','total.period','YEAR',
                    'ag.huc8','dev.huc8','wet.huc8','forst.huc8','l.owqi',
                    'county.pop.growthrate','owqi',
                 'seasonal','Ag','Dev','Wetl','Forst',
                 grep('OWEB',names(mod.data),value=T))]

#covars[,grep('OWEB',names(covars))] = log(covars[,grep('OWEB',names(covars))])


# non-oweb restoration funding to date
# oweb restoration funding prior to number
#

covars = mutate(covars,OWEB_Grant_All_12_WC = OWEB_Grant_Restoration_12_WC+
  OWEB_Grant_Capacity_12_WC +
  OWEB_Grant_Tech_12_WC +
  OWEB_Grant_Outreach_12_WC +
  OWEB_Grant_Education_12_WC,
  OWEB_Grant_All_36_WC = OWEB_Grant_Restoration_36_WC+
    OWEB_Grant_Capacity_36_WC +
    OWEB_Grant_Tech_36_WC +
    OWEB_Grant_Outreach_36_WC +
    OWEB_Grant_Education_36_WC,
  OWEB_Grant_All_60_WC = OWEB_Grant_Restoration_60_WC+
    OWEB_Grant_Capacity_60_WC +
    OWEB_Grant_Tech_60_WC +
    OWEB_Grant_Outreach_60_WC +
    OWEB_Grant_Education_60_WC,
  OWEB_Grant_All_12_SWCD = OWEB_Grant_Restoration_12_SWCD+
    OWEB_Grant_Capacity_12_SWCD +
    OWEB_Grant_Tech_12_SWCD +
    OWEB_Grant_Outreach_12_SWCD +
    OWEB_Grant_Education_12_SWCD,
  OWEB_Grant_All_36_SWCD = OWEB_Grant_Restoration_36_SWCD+
    OWEB_Grant_Capacity_36_SWCD +
    OWEB_Grant_Tech_36_SWCD +
    OWEB_Grant_Outreach_36_SWCD +
    OWEB_Grant_Education_36_SWCD,
  OWEB_Grant_All_60_SWCD = OWEB_Grant_Restoration_60_SWCD+
    OWEB_Grant_Capacity_60_SWCD +
    OWEB_Grant_Tech_60_SWCD +
    OWEB_Grant_Outreach_60_SWCD +
    OWEB_Grant_Education_60_SWCD
  )

k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k
covars[is.na(covars)] = 0 

#or.bond = inla.nonconvex.hull(cbind(mod.data$DECIMAL_LONG,mod.data$DECIMAL_LAT),2,2)
(mesh.a <- inla.mesh.2d(cbind(mod.data$Decimal_long,mod.data$Decimal_Lat),max.edge=c(5, 40),cut=.05))$n

spde.a <- inla.spde2.matern(mesh.a) 
plot(mesh.a)

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=covars$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

form.all.12 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  NOT_OWEB.wq.TotalCash_All + 
  OWEB_Grant_All_12_WC+
  OWEB_Grant_All_12_SWCD+
  OWEB_Grant_All_12_WC:OWEB_Grant_All_12_SWCD+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


form.all.36 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  NOT_OWEB.wq.TotalCash_All + 
  OWEB_Grant_All_36_WC+
  OWEB_Grant_All_36_SWCD+
  OWEB_Grant_All_36_WC:OWEB_Grant_All_36_SWCD+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)


form.all.60 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  NOT_OWEB.wq.TotalCash_All + 
  OWEB_Grant_All_60_WC+
  OWEB_Grant_All_60_SWCD+
  OWEB_Grant_All_60_WC:OWEB_Grant_All_60_SWCD+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

mod.all.12 <- inla(form.all.12, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.36 <- inla(form.all.36, family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

mod.all.60 <- inla(form.all.60,family='gaussian', data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=TRUE, cpo=TRUE))

print(xtable(exp(mod.all.12$summary.fixed),digits=4),type='html','JPART_Submission/Version2/mod.all.12.table.html')
print(xtable(exp(mod.all.36$summary.fixed),digits=4),type='html','JPART_Submission/Version2/mod.all.36.table.html')
print(xtable(exp(mod.all.60$summary.fixed),digits=4),type='html','JPART_Submission/Version2/mod.all.60.table.html')

dic.scores.all.grants = data.frame(mod.all.12$dic$dic,
                                   mod.all.36$dic$dic,
                                   mod.all.60$dic$dic)
colnames(dic.scores.all.grants) = c(paste('Model ',c('1yr','3yr','5yr'),sep=''))
dic.scores.all.grants[1,] = round(dic.scores.all.grants[1,])

write.table(dic.scores.all.grants,'JPART_Submission/Version2/dic.score.all.grants.table.txt')

library(stargazer)

coef.all.12 = data.frame(value1 = c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.12$summary.fixed[,'mean']),3)),
                                                        sprintf('%.3f',round(exp(mod.all.12$summary.fixed[,'0.025quant']),3)),sep=' ('),
                                                  sprintf('%.3f',round(exp(mod.all.12$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
                                     round(mod.all.12$dic$dic,1))
                          ,
                          coefs = c(
                            "\\% Agric. (1000m buffer)",
                            '\\% Forest (1000m buffer)',
                            '\\% Devel. (1000m buffer)',
                            '\\% Devel. in HUC8',
                            "\\% Agric. in HUC8",
                            '\\% Forest in HUC8',
                            'Elevation (10m)',
                            'Dist. from coast (10km)',
                            'Non-OWEB Restoration ($100k)',
                            'OWEB to WC ($100k)',
                            'OWEB to SWCD ($100k)',
                            'OWEB to WC * OWEB to SWCD',
                            'DIC:')
)



coef.all.36 = data.frame(value2=c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.36$summary.fixed[,'mean']),3)),
                                                      sprintf('%.3f',round(exp(mod.all.36$summary.fixed[,'0.025quant']),3)),sep=' ('),
                                                sprintf('%.3f',round(exp(mod.all.36$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
                                   round(mod.all.36$dic$dic,1))
                          ,
                         coefs = c(
                           "\\% Agric. (1000m buffer)",
                           '\\% Forest (1000m buffer)',
                           '\\% Devel. (1000m buffer)',
                           '\\% Devel. in HUC8',
                           "\\% Agric. in HUC8",
                           '\\% Forest in HUC8',
                           'Elevation (10m)',
                           'Dist. from coast (10km)',
                           'Non-OWEB Restoration ($100k)',
                           'OWEB to WC ($100k)',
                           'OWEB to SWCD ($100k)',
                           'OWEB to WC * OWEB to SWCD',
                           'DIC:')
)

coef.all.60 = data.frame(value3 = c(paste0(paste(paste(sprintf('%.3f',round(exp(mod.all.60$summary.fixed[,'mean']),3)),
                                                        sprintf('%.3f',round(exp(mod.all.60$summary.fixed[,'0.025quant']),3)),sep=' ('),
                                                  sprintf('%.3f',round(exp(mod.all.60$summary.fixed[,'0.975quant']),3)),sep=', '),')')[-1],
                                     round(mod.all.60$dic$dic,1))
                          ,
                         coefs = c(
                           "\\% Agric. (1000m buffer)",
                           '\\% Forest (1000m buffer)',
                           '\\% Devel. (1000m buffer)',
                           '\\% Devel. in HUC8',
                           "\\% Agric. in HUC8",
                           '\\% Forest in HUC8',
                           'Elevation (10m)',
                           'Dist. from coast (10km)',
                           'Non-OWEB Restoration ($100k)',
                           'OWEB to WC ($100k)',
                           'OWEB to SWCD ($100k)',
                           'OWEB to WC * OWEB to SWCD',
                           'DIC:')
)


coef.names = data.frame(coefs = c(
  "\\% Agric. (1000m buffer)",
  '\\% Forest (1000m buffer)',
  '\\% Devel. (1000m buffer)',
  '\\% Devel. in HUC8',
  "\\% Agric. in HUC8",
  '\\% Forest in HUC8',
  'Elevation (10m)',
  'Dist. from coast (10km)',
  'Non-OWEB Restoration ($100k)',
  'OWEB to WC ($100k)',
  'OWEB to SWCD ($100k)',
  'OWEB to WC * OWEB to SWCD',
  'DIC:'))


coef.table = join_all(list(coef.names,coef.all.12,coef.all.36,coef.all.60))
rownames(coef.table) = coef.table$coefs
coef.table = coef.table[,-1]

colnames(coef.table) = c('1 year sum','3 year sum','5 year sum')
#c('$\\hat{\beta} (0.025,0.975)$','$\\hat{\beta} (0.025,0.975)$','$\\hat{\beta} (0.025,0.975)$')


stargazer(coef.table,label='table:basecoefs',out='JPART_Submission/Version2/modelcoefs.tex',type='latex',summary=F,
          title = 'Posterior parameter estimates, all grant types')

library(mail)
sendmail('tyler.andrew.scott@gmail.com','script finished','model_all_funding.R finished on union')


















