rm(list=ls())

setwd('win/user/quinalt')
load("temp_workspace_noprecip.RData")


library(INLA)
require(xtable)
library(maptools)

#test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 


mod.data = all.params.spdf@data
mod.data = mod.data[,-grep('Total.Solids',colnames(mod.data))]
mod.data = mod.data[,-grep('Total.Phos',colnames(mod.data))]
mod.data$seasonal = mod.data$Abs.Month
mod.data$total.period = mod.data$Abs.Month
mod.data$sq.owqi = ((as.numeric(as.character(mod.data$owqi)))^2)
mod.data$l.owqi = log(as.numeric(as.character(mod.data$owqi)))
mod.data = filter(mod.data,YEAR>=1995)
mod.data$HUC8 = as.character(mod.data$HUC8)
mod.data$under.wc = ifelse(is.na(mod.data$which.wc),0,1)
mod.data$under.swcd = ifelse(is.na(mod.data$which.swcd),0,1)



covars = mod.data[,c('elevation','seaDist','HUC8','total.period','YEAR',
                    'ag.huc8','dev.huc8','wet.huc8','forst.huc8','l.owqi',
                    'county.pop.growthrate','owqi','under.wc','under.swcd',
                 'seasonal','Ag','Dev','Wetl','Forst',
                 grep('OWEB',names(mod.data),value=T))]

covars$OWEB.wq.TotalCash_Prior12 = (covars$OWEB.wq.TotalCash_All - covars$OWEB.wq.TotalCash_12)
covars$OWEB_Grant_Capacity.WC_Prior12 = (covars$OWEB_Grant_Capacity.WC_All - covars$OWEB_Grant_Capacity.WC_12)
covars$OWEB_Grant_Capacity.WC_Prior36 = (covars$OWEB_Grant_Capacity.WC_All - covars$OWEB_Grant_Capacity.WC_36)
covars[is.na(covars)] = 0
covars = covars[covars$YEAR>=1995,]
k = 100000
covars[,grep('OWEB',names(covars))] = covars[,grep('OWEB',names(covars))]/k
#covars[,grep('OWEB',names(covars))] = log(covars[,grep('OWEB',names(covars))])


# non-oweb restoration funding to date
# oweb restoration funding prior to number
#


form0 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  
  OWEB_Grant_Restoration_12_WC+
  OWEB_Grant_Capacity_12_WC +
  OWEB_Grant_Outreach_12_WC +
  OWEB_Grant_Education_12_WC +
  OWEB_Grant_Outreach_12_WC:OWEB_Grant_Education_12_WC:OWEB_Grant_Restoration_12_WC:OWEB_Grant_Capacity_12_WC+
 f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)

mod0 <- inla(form0, family='gaussian', 
             data=data.frame(y=covars$l.owqi, covars,b0=1), 
             control.predictor=list(compute=TRUE),
             #     control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
             control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)

covars2 = covars
covars2[,grep('OWEB',names(covars2))] = log(covars[,grep('OWEB',names(covars2))])

form1 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + ag.huc8+
  forst.huc8 + elevation + seaDist + 
  OWEB_Grant_Restoration_12_WC+
  OWEB_Grant_Capacity_12_WC +
  OWEB_Grant_Outreach_12_WC +
  OWEB_Grant_Education_12_WC +
  OWEB_Grant_Outreach_12_WC:OWEB_Grant_Education_12_WC:OWEB_Grant_Restoration_12_WC:OWEB_Grant_Capacity_12_WC+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)

mod1 <- inla(form0, family='gaussian', 
             data=data.frame(y=covars2$l.owqi, covars2,b0=1), 
             control.predictor=list(compute=TRUE),
             #     control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
             control.compute=list(dic=TRUE, cpo=TRUE),verbose=T)




mod1$summary.fixed

round(exp(mod0$summary.fixed)[,1:2],2)
round(exp(mod1$summary.fixed)[,1:2],2)


ggplot(mod.data) + geom_point(aes(x=Decimal_long,y=Decimal_Lat,colour=as.factor(under.wc)))

dim(mod.data)

mod0 = lmer(l.owqi~ elevation + seaDist+Ag + Dev + Wetl + Forst+OWEB_Grant_Capacity.WC_Prior12+
              forst.huc8 + wet.huc8 + dev.huc8+
              (1|HUC8) + (1|YEAR),data=covars)

summary(mod0)
sum0 = summary(mod0)






covars$Total_OWEB_All = rowSums(covars[,intersect(grep('All',names(covars),value=T), grep('OWEB',names(covars),value=T))])
covars$Total_OWEB_12 = rowSums(covars[,intersect(grep('12',names(covars),value=T), grep('OWEB',names(covars),value=T))])
covars$Total_OWEB_36 = rowSums(covars[,intersect(grep('36',names(covars),value=T), grep('OWEB',names(covars),value=T))])
covars$Total_OWEB_60 = rowSums(covars[,intersect(grep('60',names(covars),value=T), grep('OWEB',names(covars),value=T))])

covars$Total_Non_OWEB_36 = covars$Other.FALSE.TotalCash_36 + covars$Public.FALSE.TotalCash_36
covars$Total_Non_OWEB_12 = covars$Other.FALSE.TotalCash_12 + covars$Public.FALSE.TotalCash_12
covars$Total_Non_OWEB_60 = covars$Other.FALSE.TotalCash_60 + covars$Public.FALSE.TotalCash_60
covars$Total_Non_OWEB_All = covars$Other.FALSE.TotalCash_All + covars$Public.FALSE.TotalCash_All

##
#plot(test,border='grey90',col='grey80',
 #    xlim = range(mesh.a$loc[, 1]),
  #   ylim = range(mesh.a$loc[, 2]))
#plot(mesh.a, asp=1,main=NULL,sub=NULL,add=T)
#points(temp$DECIMAL_LONG[!duplicated(temp$DECIMAL_LONG)],
#       temp$DECIMAL_LAT[!duplicated(temp$DECIMAL_LAT)],col='blue',pch=21)
#legend(x=-116.5,y=41.75,legend='Station',pch=21,col='blue',pt.cex=1.5)

covars$Total_Prior_To_12 = (covars$Total_Non_OWEB_All + covars$Total_OWEB_All)-
(covars$Total_Non_OWEB_12 + covars$Total_OWEB_12)

covars$Total_Prior_To_36 = (covars$Total_Non_OWEB_All + covars$Total_OWEB_All)-
  (covars$Total_Non_OWEB_36 + covars$Total_OWEB_36)

covars$Total_Prior_To_60 = (covars$Total_Non_OWEB_All + covars$Total_OWEB_All)-
  (covars$Total_Non_OWEB_60 + covars$Total_OWEB_60)


#Model 0: No spatial effect
form0 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  dev.huc8 + 
  forst.huc8 + elevation + seaDist + 
 # OWEB.wq.TotalCash_Prior12+
  #OWEB_Grant_Capacity.WC_12+
#  OWEB_Grant_Restoration.WC_12+
  
  OWEB_Grant_Outreach.WC_12+
  OWEB_Grant_Education.WC_12+
  OWEB_Grant_Restoration.WC_12+
  OWEB_Grant_Capacity.WC_12+
  OWEB_Grant_Outreach.WC_12:OWEB_Grant_Education.WC_12:OWEB_Grant_Restoration.WC_12:OWEB_Grant_Capacity.WC_12+
  f(HUC8,model='iid')+
 f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)


sum(is.na(covars$ag.huc8))

modsum = summary(mod0)


modsum


modsum$fixed[10:nrow(modsum$fixed),]
covars$TotalCash_36
summary(mod0)$dic$dic
6213.95
library(INLA)
install.packages('INLA')
#cREATE MESH, SPDE
mod.data$Decimal_long

#note: mesh, spde object used for models 1-7
(mesh.a <- inla.mesh.2d(cbind(mod.data$Decimal_long,mod.data$Decimal_Lat), max.edge=c(5,40),cut=.10))$n
plot(mesh.a)
spde.a <- inla.spde2.matern(mesh.a) 

####make plot
# plot(mesh.a,main=F,sub=F)
# plot(test,,col='grey80',add=T)
# plot(mesh.a,main=F,sub=F,add=T)
# plot(params.spdf,add=T,col='blue',pch=1)
# legend(x='bottomright',legend = 'Station',pch = 1,col='blue')
#savePlot(type = 'png',filename = 'oregonmeshplot.png')
#######


# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(mesh.a, loc=cbind(mod.data$Decimal_long,mod.data$Decimal_Lat))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=mod.data$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))


form1 <-   y ~ 0 + b0 + Ag + Forst + Dev  + ag.huc8 + dev.huc8 + forst.huc8 + elevation + seaDist + 
  Total_Prior_To_12+
  OWEB_Restoration.WC_12+
  OWEB_Tech.WC_12+
  OWEB_Ed.WC_12+
  OWEB_Council.WC_12+
  OWEB_Restoration.WC_12:OWEB_Tech.WC_12:OWEB_Ed.WC_12:OWEB_Council.WC_12+
  OWEB_Restoration.Public_12+
  OWEB_Tech.Public_12+
  OWEB_Ed.Public_12+
  OWEB_Restoration.Public_12:OWEB_Tech.Public_12:OWEB_Ed.Public_12+
  f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+
  f(s, model=spde.a)

mod1 <- inla(form1, family='gaussian', data=inla.stack.data(stk.1),
               control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
             #  control.inla=list(strategy='laplace'), 
               control.compute=list(dic=TRUE, cpo=TRUE))



#################

#Model 2: random effect is replicated each year 
table(repl.2 <- temp$YEAR-(min(temp$YEAR)-1))
dim(A.2 <- inla.spde.make.A(mesh.a, repl=repl.2, loc=cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT)))
ind.2 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.repl=length(unique(temp$YEAR)))
stk.2 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.2,1),
                     effects=list(ind.2, list(data.frame(b0=1,covars))))
# formulae
form2 <-  y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a,replicate=s.repl)

mod2 <- inla(form2, family='gaussian', data=inla.stack.data(stk.2),
              control.predictor=list(A=inla.stack.A(stk.2), compute=TRUE),
            #  control.inla=list(strategy='laplace'), 
              control.compute=list(dic=TRUE, cpo=TRUE))



####MODELS 3-4: RANDOM EFFECT CORRELATED BETWEEN YEARS
##MODELS 3-4 USE SAME STACK
table(gr.3 <- temp$YEAR-(min(temp$YEAR)-1))
dim(A.3 <- inla.spde.make.A(mesh.a, group=gr.3, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT)))
ind.3 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.group=length(unique(gr.3)))
stk.3 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.3,1),
                    effects=list(ind.3, list(data.frame(b0=1,covars))))

#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
form3 <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  +   f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

mod3 <- inla(form3, family='gaussian', data=inla.stack.data(stk.3),
                control.predictor=list(A=inla.stack.A(stk.3), compute=T),
               # control.inla=list(strategy='laplace'), 
             control.inla(list(h=1.0)),
                control.compute=list(dic=TRUE, cpo=TRUE))

#Model 4: random effect is correlated between years. exchangable correlation ($m3b$) 
form4 <- y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +   f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+  
  f(s, model=spde.a, group=s.group, control.group=list(model='exchangeable'))

mod4 <- inla(form4, family='gaussian', data=inla.stack.data(stk.3),
                control.predictor=list(A=inla.stack.A(stk.3), compute=T),
                # control.inla=list(strategy='laplace'), 
                control.compute=list(dic=TRUE, cpo=TRUE))


#Model 5: Replication each month
table(repl.5 <- (temp$YEAR-min(temp$YEAR))*12 + as.integer(temp$MONTH.NUM))
dim(A.5 <- inla.spde.make.A(mesh.a, repl=repl.5, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT)))
ind.5 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.repl=length(unique(repl.5))+1)
stk.5 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.5,1), tag='e',
                     effects=list(ind.5, list(data.frame(b0=1,covars))))
form5 <- y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +   f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+
  f(s, model=spde.a, replicate=s.repl) 

mod5 <- inla(form5, family='gaussian', data=inla.stack.data(stk.5),
                control.predictor=list(A=inla.stack.A(stk.5), compute=TRUE),
              #  control.inla=list(strategy='laplace'), 
                control.compute=list(dic=TRUE, cpo=TRUE))



####Models 6-7: REPLICATION BETWEEN YEARS
##MODELS 6-7 USE SAME STACK
table(repl.6 <- temp$YEAR-(min(temp$YEAR)-1))
table(gr.6 <- as.integer(as.character(temp$MONTH.NUM)))
dim(A.6 <- inla.spde.make.A(mesh.a, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT), 
                            repl=repl.6, group=gr.6))
ind.6 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.repl=length(unique(temp$YEAR)),
                              n.group=length(unique(temp$MONTH.NUM)))
stk.6 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.6,1),
                    effects=list(ind.6, list(data.frame(b0=1,covars))))

#Model 6: Replication between years. autoregressive correlation between months within year. 
form6 <- y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'), replicate=s.repl)

mod6 <- inla(form6, family='gaussian', data=inla.stack.data(stk.6),
                 control.predictor=list(A=inla.stack.A(stk.6), compute=T),
                # control.inla=list(strategy='laplace'), 
                 control.compute=list(dic=TRUE, cpo=TRUE))

#Model 7: Replication between years. exchangable correlation between months

form7 <-y ~ 0 + b0 + Ag + Forst + Dev  + 
  elevation + seaDist  +  f(HUC8,model='iid')+
  f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+
  f(s, model=spde.a, group=s.group, control.group=list(model='exchangeable'), replicate=s.repl) 

mod7 <- inla(form7, family='gaussian', data=inla.stack.data(stk.6),
          control.predictor=list(A=inla.stack.A(stk.6), compute=T),
        #control.inla=list(strategy='laplace'), 
          control.compute=list(dic=TRUE, cpo=TRUE))

#models 3, 4 and 7 give the followin warning
# *** WARNING *** Eigenvalue 3 of the Hessian is -41.3053 < 0
# *** WARNING *** Set this eigenvalue to 1.94764
# *** WARNING *** This have consequence for the accurancy of
# *** WARNING *** the approximations; please check!!!
# *** WARNING *** R-inla: Use option inla(..., control.inla = list(h = h.value), ...) 
# *** WARNING *** R-inla: to chose a different  `h.value'.

library(stargazer)


dic.scores = data.frame(
mod0$dic$dic,
mod1$dic$dic,
mod2$dic$dic,
NA,
mod4$dic$dic,
mod5$dic$dic,
mod6$dic$dic,
NA)
colnames(dic.scores) = c(paste('M',0:7,sep=''))

dic.scores[1,] = round(dic.scores[1,])
rownames(dic.scores) = 'DIC score'

stargazer(dic.scores,type='latex',summary=F,out='comparebasemodsdic.tex',label='table:dicscores',
          title='Compare DIC scores for different SPDE models',table.placement='!hbtp')


dic.scores

save.image('spde.fit.results.RData')

rm(list=ls())
setwd('H:/quinalt')










