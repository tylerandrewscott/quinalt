
if(!run.owqi.only)
{
  for (i in 1:nrow(mod.data))
  {
    mod.data$hist.avg.temp_si[i] = 
      ifelse(is.na(mean(mod.data$temp_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$temp_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$temp_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.bod_si[i] = 
      ifelse(is.na(mean(mod.data$bod_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$bod_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$bod_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.ph_si[i] = 
      ifelse(is.na(mean(mod.data$ph_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$ph_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$ph_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.p_si[i] = 
      ifelse(is.na(mean(mod.data$p_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$p_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$p_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.n_si[i] =   
      ifelse(is.na(mean(mod.data$n_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$n_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$n_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.ts_si[i] = 
      ifelse(is.na(mean(mod.data$ts_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$ts_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$ts_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.do_si[i] = 
      ifelse(is.na(mean(mod.data$do_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$do_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$do_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.ecoli_si[i] = 
      ifelse(is.na(mean(mod.data$ecoli_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$ecoli_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$ecoli_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.fec_si[i] = 
      ifelse(is.na(mean(mod.data$fec_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$fec_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$fec_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
    
    mod.data$hist.avg.bact_si[i] = 
      ifelse(is.na(mean(mod.data$bact_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]])),
             mean(mod.data$bact_si[mod.data$Abs.Month<mod.data$Abs.Month[i]]),
             mean(mod.data$bact_si[mod.data$HUC8==mod.data$HUC8[i]&mod.data$Abs.Month<mod.data$Abs.Month[i]]))
  }
}




tex.swcd.p1 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p1$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p1$dic$dic,mod.swcd.p1$waic$waic))

tex.swcd.p2 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p2$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p2$dic$dic,mod.swcd.p2$waic$waic))

tex.swcd.p3 <- texreg::createTexreg(
  coef.names = mod.swcd.p1$names.fixed,
  coef = mod.swcd.p3$summary.lincomb.derived$mean,
  ci.low = mod.swcd.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.swcd.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.swcd.p3$dic$dic,mod.swcd.p3$waic$waic))

htmlreg(l=list(tex.swcd.p1,tex.swcd.p2,tex.swcd.p3),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
        file = '../../../Deliverables/JPART/Version2/swcdmodtable.html')






draws = 100000
oweb.wc.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.swcd.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.wc.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.swcd.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.wc.p3.all = inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`) +
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OWEB.proj.in.last.3yr.SWCD`)
oweb.swcd.p3.all = inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$OWEB.proj.in.last.3yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OWEB.proj.in.last.3yr.SWCD`)

margs = data.frame(oweb.wc.p1.all,oweb.swcd.p1.all,
                   oweb.wc.p2.all,oweb.swcd.p2.all,
                   oweb.wc.p3.all,oweb.swcd.p3.all)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1.all',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2.all',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3.all',margs.lon$key)] <- 'p3'
margs.lon$which = NA
margs.lon$which[grep('wc',margs.lon$key)] <- 'WC'
margs.lon$which[grep('swcd',margs.lon$key)] <- 'SWCD'
margs.lon$uq <- paste(margs.lon$which,margs.lon$window)



library(ggplot2)

margs.lon$uq = as.factor(margs.lon$uq)

levels(margs.lon$uq) = c("SWCD 1yr ", "SWCD 2yr " ,"SWCD 3yr ", "WC 1yr",  "WC 2yr"  , "WC 3yr"  )


p.gone = ggplot(subset(margs.lon))+ 
  geom_density(aes(x=value,
                   color=uq,linetype=uq),lwd=1)+
  scale_linetype_manual(values=c(1,2,3,1,2,3)) +
  scale_color_manual(values=c(rep(alt.col,3),rep('black',3))) +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Funds to WC vs. SWCD') + ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=16),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=18)) 

#ggsave(filename = 'Deliverables/JPART/Final/figure2.eps',p2,width=5,dpi = 500,fonts=c("serif", "Palatino"))



mod.swcd.p1 <- inla(form.swcd.p1, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))
mod.swcd.p2 <- inla(form.swcd.p2, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))


mod.swcd.p3 <- inla(form.swcd.p3, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))


form.swcd.p1 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.1yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.1yr.WC + 
  OWEB.proj.in.last.1yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.1yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.1yr.WC*STAFF.FTE + 
  # OWEB.proj.in.last.1yr.SWCD + 
  OWEB.proj.in.last.1yr.WC*OWEB.proj.in.last.1yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  # f(total.period,model='rw2',values=seq(min(covars$total.period),max(covars$total.period)),replicate = Station.Repl) +
  #f(seasonal,model='seasonal',values=seq(min(covars$total.period),max(covars$total.period)),     season.length=length.of.season) + 
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p1)%*%A.1), e= rep(0,n.covariates.base.p1)))

form.swcd.p2 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.2yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.2yr.WC + 
  OWEB.proj.in.last.2yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.2yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.2yr.WC*STAFF.FTE + 
  # OWEB.proj.in.last.2yr.SWCD + 
  OWEB.proj.in.last.2yr.WC*OWEB.proj.in.last.2yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p2)%*%A.1), e= rep(0,n.covariates.base.p2)))

form.swcd.p3 <- y ~ 0 + b0 + Decimal_Lat + Decimal_long + ag.huc8+forst.huc8 + dev.huc8 + 
  elev100m +  monthly.precip.median + hist.avg.owqi + OWRI.proj.in.past.3yr +
  #YEARS.ACTIVE + OP.BUDGET.200k +  STAFF.FTE + 
  #OWEB.proj.in.last.3yr.WC + 
  OWEB.proj.in.last.3yr.WC*YEARS.ACTIVE + 
  OWEB.proj.in.last.3yr.WC*OP.BUDGET.200k +  
  OWEB.proj.in.last.3yr.WC*STAFF.FTE + 
  # OWEB.proj.in.last.3yr.SWCD + 
  OWEB.proj.in.last.3yr.WC*OWEB.proj.in.last.3yr.SWCD +
  f(HUC8,model='iid',param=c(0.001,0.001)) + 
  f(total.period,model='iid',param=c(0.001,0.001)) +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q.base.p3)%*%A.1), e= rep(0,n.covariates.base.p3)))


X.swcd.p1 <- cbind(rep(1,n.data),
                   covars$Decimal_Lat, covars$Decimal_long,
                   covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                   covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.1yr)
#covars$YEARS.ACTIVE)

n.covariates.swcd.p1 = ncol(X.swcd.p1)
Q.swcd.p1 = qr.Q(qr(X.swcd.p1))

# 
X.swcd.p2 <- cbind(rep(1,n.data), covars$Decimal_Lat, covars$Decimal_long,
                   covars$ag.huc8, covars$forst.huc8,
                   covars$dev.huc8,
                   covars$elev100m,covars$hist.avg.owqi,
                   covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.2yr)

n.covariates.swcd.p2 = ncol(X.swcd.p2)
Q.swcd.p2 = qr.Q(qr(X.swcd.p2))
# 
X.swcd.p3 <- cbind(rep(1,n.data),
                   covars$Decimal_Lat, covars$Decimal_long,
                   covars$ag.huc8,covars$forst.huc8,covars$dev.huc8,
                   covars$elev100m,covars$hist.avg.owqi,covars$monthly.precip.median,
                   covars$OWRI.proj.in.past.3yr)


dep.var.list = c(grep('_si',names(mod.data),value=T),'owqi')



for (i in dep.var.list)
{
  stk.1 <- inla.stack(data=list(y=covars[,i]), A=list(A.1,1),
                      effects=list(ind.1, 
                                   list(data.frame(b0=1,covars[,colnames(covars)%in% dep.var.list==FALSE]))))
  
  mod.base.p1 <- inla(form.base.p1, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  library(tidyr)
  temp = mod.base.p1$summary.fixed[,c('mean','0.025quant','0.975quant')]
  temp$Variable = rownames(temp)
  temp$Dep.Var = i
  temp$Model = 'base.p1'
  empty.list[[paste('p1',i)]] = temp
  
  mod.base.p2 <- inla(form.base.p2, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  
  temp = mod.base.p2$summary.fixed[,c('mean','0.025quant','0.975quant')]
  temp$Variable = rownames(temp)
  temp$Dep.Var = i
  temp$Model = 'base.p2'
  empty.list[[paste('p2',i)]] = temp
  
  mod.base.p3 <- inla(form.base.p3, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))
  temp = mod.base.p3$summary.fixed[,c('mean','0.025quant','0.975quant')]
  temp$Variable = rownames(temp)
  temp$Dep.Var = i
  temp$Model = 'base.p3'
  empty.list[[paste('p3',i)]] = temp
}



base.coef.df = join_all(empty.list,type='full')
names(base.coef.df) = c("mean"  ,  "upper" ,"lower", "Variable"  , "Dep.Var"  ,  "Model" )

base.coef.df$Variable <- factor(base.coef.df$Variable)

