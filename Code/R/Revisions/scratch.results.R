rm(list=ls())

load('Code/R/Revisions/test.results.RData')

library(texreg)

tex.base.p1 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p1$summary.lincomb.derived$mean,
  ci.low = mod.base.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p1$dic$dic,mod.base.p1$waic$waic))

tex.base.p2 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p2$summary.lincomb.derived$mean,
  ci.low = mod.base.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p2$dic$dic,mod.base.p2$waic$waic))    
    
tex.base.p3 <- texreg::createTexreg(
  coef.names = mod.base.p1$names.fixed,
  coef = mod.base.p3$summary.lincomb.derived$mean,
  ci.low = mod.base.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.base.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.base.p3$dic$dic,mod.base.p3$waic$waic))
  
  
htmlreg(l=list(tex.base.p1,tex.base.p2,tex.base.p3),leading.zero=TRUE,
       omit.coef = c('b0'),ci.test = 0,digits = 3,
       custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
       file = '../../../Deliverables/JPART/Version2/basemodtable.html')


tex.project.p1 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p1$summary.lincomb.derived$mean,
  ci.low = mod.project.p1$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p1$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p1$dic$dic,mod.project.p1$waic$waic))

tex.project.p2 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p2$summary.lincomb.derived$mean,
  ci.low = mod.project.p2$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p2$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p2$dic$dic,mod.project.p2$waic$waic))

tex.project.p3 <- texreg::createTexreg(
  coef.names = mod.project.p1$names.fixed,
  coef = mod.project.p3$summary.lincomb.derived$mean,
  ci.low = mod.project.p3$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.project.p3$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.project.p3$dic$dic,mod.project.p3$waic$waic))



htmlreg(l=list(tex.project.p1,tex.project.p2,tex.project.p3),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Past 1yr Funds','Past 2yr Funds','Past 3yr Funds'),
        file = '../../../Deliverables/JPART/Version2/projectmodtable.html')


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





library(INLA)

draws = 100000
oweb.p1 = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC)
oweb.p1.staff = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC`)
oweb.p1.budg = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC`)
oweb.p1.years = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC`)
oweb.p1.all = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC`)

oweb.p2 = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC)
oweb.p2.staff = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC`)
oweb.p2.budg = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC`)
oweb.p2.years = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC`)
oweb.p2.all = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC`)

oweb.p3 = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC)
oweb.p3.staff = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC`)
oweb.p3.budg = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC`)
oweb.p3.years = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC`)
oweb.p3.all = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC`)+
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC`)
 

margs = data.frame(oweb.p1,oweb.p1.all,
                   oweb.p2,oweb.p2.all,
                   oweb.p3,oweb.p3.all)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3',margs.lon$key)] <- 'p3'
margs.lon$which = 'Linear'
margs.lon$which[grep('all',margs.lon$key)] <- 'Interaction'

margs.lon$uq <- paste(margs.lon$which,margs.lon$window)
margs.lon$uq = as.factor(margs.lon$uq)
levels(margs.lon$uq)
levels(margs.lon$uq) = c('Linear 1yr ','Linear 2yr ','Linear 3yr ','Interaction 1yr ','Interaction 2yr','Interaction 3yr')

ggplot(subset(margs.lon))+ 
  geom_density(aes(x=value,
                   color=uq,linetype=uq),lwd=1)+
  scale_linetype_manual(values=c(1,2,3,1,2,3)) +
  scale_color_manual(values=c(rep(alt.col,3),rep('black',3))) +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Linear only vs. Interaction') + ylab('Density') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=20),
        axis.text=element_text(size=18),
        legend.title = element_blank(),
        legend.text = element_text(size=18)) 



library(ggplot2)







draws = 100000
oweb.wc.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC`)+
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC`) +
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.swcd.p1.all = inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$OWEB.proj.in.last.1yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OWEB.proj.in.last.1yr.SWCD`)

oweb.wc.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC`) +
inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC`) +
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC`) +
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.swcd.p2.all = inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$OWEB.proj.in.last.2yr.SWCD) + 
  inla.rmarginal(draws, mod.swcd.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OWEB.proj.in.last.2yr.SWCD`)

oweb.wc.p3.all = inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC`)+
  inla.rmarginal(draws, mod.swcd.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC`) +
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

bas.col = 'black'
alt.col = '#E69F00'

library(ggplot2)

margs.lon$uq = as.factor(margs.lon$uq)

levels(margs.lon$uq) = c("SWCD 1yr ", "SWCD 2yr " ,"SWCD 3yr ", "WC 1yr",  "WC 2yr"  , "WC 3yr"  )

ggplot(subset(margs.lon))+ 
  geom_density(aes(x=value,
                color=uq,linetype=uq),lwd=1)+
  scale_linetype_manual(values=c(1,2,3,1,2,3)) +
  scale_color_manual(values=c(rep(alt.col,3),rep('black',3))) +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Funds to WC vs. SWCD') + ylab('Density') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=20),
        axis.text=element_text(size=18),
        legend.title = element_blank(),
        legend.text = element_text(size=18)) 






+  
  scale_size_manual(values =rep(c(1,0.7),each=4))+
  scale_color_manual(values=c(stochdefcolor,stochoptcolor,
                              dtrmndefcolor, dtrmnoptcolor,
                              stochdefcolor,stochdefcolor,
                              stochoptcolor,stochoptcolor)) 


guide_legend

+
  geom_density(aes(x=value,col=uq,linetype=uq),data = margs.lon) 
  
  
  
   +
  scale_color_colorblind() + 
  guides(col=guide_legend(ncol=2,nrow=3,colour=c('red','blue','red','blue','red','blue')))+
  

colour=rep(colorblind_pal()(2),each=2)
?geom_density

  


rep(c(alt.col,bas.col),each=3)

?scale_color_colorblind
library(scales)
show_col(colorblind_pal()(8))

 
  scale_color_manual(labels=c('SWCD','WC','Extra'),values=c('red','green','blue'),breaks=c(1:3))
  
  
  ?scale_color_manual
  
  
  scale_linetype_manual(values=rep(1:3,each=2),labels=c('1yr','2yr','3yr','1yr','2yr','3yr')) +
  guides(col=guide_legend(ncol=2),linetype=guide_legend(nrow=3,ncol=3))

rep(1:3,each=2)

?scale_linetype

scale_color_colorblind(linetype=c(1,1,2,2,3,3))

?scale_color_discrete

+ #,limits=c(-0.8,2.5))+


?guides



margs = data.frame(oweb.wc.p2.all,oweb.swcd.p2.all)
margs.lon = gather(margs)
ggplot(data=margs.lon,aes(x=value,col=key,linetype=key)) + 
  geom_density(lwd=1) + theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Past 2yrs funding to WC vs. SWCD') + ylab('Density') +
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  scale_x_continuous(expand=c(0,0)) + #,limits=c(-0.8,2.5))+
  scale_color_colorblind(labels=c('WC','SWCD'))+
  scale_linetype(labels=c('WC','SWCD'))

margs = data.frame(oweb.wc.p3.all,oweb.swcd.p3.all)
margs.lon = gather(margs)
ggplot(data=margs.lon,aes(x=value,col=key,linetype=key)) + 
  geom_density(lwd=1) + theme_bw() +theme_tufte(ticks=FALSE) +
  xlab('Sampled Posterior Marginals: Past 3yrs funding to WC vs. SWCD') + ylab('Density') +
  theme(legend.position = c(.85,.5),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  scale_x_continuous(expand=c(0,0)) + #,limits=c(-0.8,2.5))+
  scale_color_colorblind(labels=c('WC','SWCD'))+
  scale_linetype(labels=c('WC','SWCD'))



n = 100 
racewhite = rep_len(c('0','1'),100)
educate = 1:n 
y = rnorm(n) 

r = inla(y ~ 1 + racewhite*educate, 
         data = data.frame(racewhite, educate), 
         control.predictor = list(compute=TRUE)) 

## net effect of educate + racewhite:educate 
lc = inla.make.lincombs( 
  educate= r$model.matrix[, "educate"], 
  "racewhite1:educate" = r$model.matrix[, "racewhite1:educate"]) 




rr = inla(y ~ 1 + racewhite*educate, 
          data = data.frame(racewhite, educate), 
          control.predictor = list(compute=TRUE), 
          lincomb = lc,verbose=T) 


plot(rr$summary.lincomb.derived$mean)


plot(density(rr$summary.lincomb.derived$mean), lty=1)
lines(density(educate_racewhite), lty=2)



texreg:::get.data(mod.base.p1)



summary(mod.base.p1)

get.data(mod.base.p1)

class(mod.base.p1)


base.p1.coef = mod.base.p1$summary.fixed

head(base.p1.coef)

texreg::extract.glm(as.glm(mod.base.p1))

library(texreg)

texreg

library(stargazer)

get.data(mod.base.p1)
createTexreg(coef.names, coef, se, pvalues = numeric(0), ci.low = numeric(0), 
             ci.up = numeric(0), gof.names = character(0), gof = numeric(0), 
             gof.decimal = logical(0), model.name = character(0))


mod.base.p1.texreg = createTexreg(coef.names = mod.base.p1$names.fixed,
             gof = c(mod.base.p1$dic$dic,mod.base.p1$waic$waic),
             coef = mod.base.p1$summary.fixed$mean,
             ci.low = mod.base.p1$summary.fixed$`0.025quant`,
             ci.up = mod.base.p1$summary.fixed$`0.975quant`,
             gof.names = c('DIC','WAIC'))

mod.base.p2.texreg = createTexreg(coef.names = mod.base.p2$names.fixed,
                                  gof = c(mod.base.p2$dic$dic,mod.base.p2$waic$waic),
                                  coef = mod.base.p2$summary.fixed$mean,
                                  ci.low = mod.base.p2$summary.fixed$`0.025quant`,
                                  ci.up = mod.base.p2$summary.fixed$`0.975quant`,
                                  gof.names = c('DIC','WAIC'))
mod.base.p3.texreg = createTexreg(coef.names = mod.base.p3$names.fixed,
                                  gof = c(mod.base.p3$dic$dic,mod.base.p3$waic$waic),
                                  coef = mod.base.p3$summary.fixed$mean,
                                  ci.low = mod.base.p3$summary.fixed$`0.025quant`,
                                  ci.up = mod.base.p3$summary.fixed$`0.975quant`,
                                  gof.names = c('DIC','WAIC'))
##Model for paper body
texreg(c(mod.base.p1.texreg,mod.base.p2.texreg,mod.base.p3.texreg),
       file='../../../Deliverables/JPART/Version2/basecoefs.tex',
       leading.zero = TRUE,dcolumn = TRUE,use.packages = FALSE,
       omit.coef="(huc8)|(Decimal)|(b0)|(elev)|(hist)|(monthly)|(OWRI)",type='latex')
 
##Base Models for appendix
texreg(c(mod.base.p1.texreg,mod.base.p2.texreg,mod.base.p3.texreg),
       file='../../../Deliverables/JPART/Version2/wholebasecoefs.tex',
       leading.zero = TRUE,dcolumn = TRUE,use.packages = FALSE,
       #omit.coef="(huc8)|(Decimal)|(b0)|(elev)|(hist)|(monthly)|(OWRI)",
       type='latex')

mod.project.p1.texreg = createTexreg(coef.names = mod.project.p1$names.fixed,
                                  gof = c(mod.project.p1$dic$dic,mod.project.p1$waic$waic),
                                  coef = mod.project.p1$summary.fixed$mean,
                                  ci.low = mod.project.p1$summary.fixed$`0.025quant`,
                                  ci.up = mod.project.p1$summary.fixed$`0.975quant`,
                                  gof.names = c('DIC','WAIC'))

mod.project.p2.texreg = createTexreg(coef.names = mod.project.p2$names.fixed,
                                     gof = c(mod.project.p2$dic$dic,mod.project.p2$waic$waic),
                                     coef = mod.project.p2$summary.fixed$mean,
                                     ci.low = mod.project.p2$summary.fixed$`0.025quant`,
                                     ci.up = mod.project.p2$summary.fixed$`0.975quant`,
                                     gof.names = c('DIC','WAIC'))

mod.project.p3.texreg = createTexreg(coef.names = mod.project.p3$names.fixed,
                                     gof = c(mod.project.p3$dic$dic,mod.project.p3$waic$waic),
                                     coef = mod.project.p3$summary.fixed$mean,
                                     ci.low = mod.project.p3$summary.fixed$`0.025quant`,
                                     ci.up = mod.project.p3$summary.fixed$`0.975quant`,
                                     gof.names = c('DIC','WAIC'))
##Model for paper body
screenreg(c(mod.project.p1.texreg,mod.project.p2.texreg,mod.project.p3.texreg),
      # file='../../../Deliverables/JPART/Version2/projectcoefs.tex',
       leading.zero = TRUE,dcolumn = TRUE,use.packages = FALSE,
       omit.coef="(huc8)|(Decimal)|(b0)|(elev)|(hist)|(monthly)|(OWRI)",type='latex')

library(lubridate)

library(texreg)


levels(base.coef.df$Variable) = c(
  "ag.huc8"                        ,         "b0"                             ,         "Decimal_Lat"                ,             "Decimal_long"          ,                 
  "dev.huc8"                       ,         "elev100m"                       ,         "forst.huc8"                 ,             "monthly.precip.median"     ,             
  "OP.BUDGET.200k"                  ,  
  "OP.BUDGET.200k:OWEB.proj.in.WC",     "OP.BUDGET.200k:OWEB.proj.in.WC", "OP.BUDGET.200k:OWEB.proj.in.WC",
  "OWEB.proj.in.WC"    ,     "OWEB.proj.in.WC"         ,     "OWEB.proj.in.WC"         ,  
  "OWRI.proj.in.W","OWRI.proj.in.W","OWRI.proj.in.W",
  "STAFF.FTE"             ,                 
  "YEARS.ACTIVE"                      ,   
  "YEARS.ACTIVE:OWEB.proj.in.last.WC",   "YEARS.ACTIVE:OWEB.proj.in.last.WC", "YEARS.ACTIVE:OWEB.proj.in.last.WC")


drop.vars = c('b0','Decimal_Lat',"Decimal_long","ag.huc8",'forst.huc8','dev.huc8',
              'monthly.precip.median','elev100m')
library(ggthemes)

place.rects = data.frame(xmin=seq(0.5,(sum(unique(base.coef.df$Variable) %in% drop.vars==FALSE)-0.5),1),
                         xmax=seq(1.5,(sum(unique(base.coef.df$Variable) %in% drop.vars==FALSE)+0.5),1),
                         fill.col = rep_len(c('grey20','grey60'),length.out=sum(unique(base.coef.df$Variable) %in% drop.vars==FALSE)))

sub.coef = base.coef.df[base.coef.df$Variable %in% drop.vars==FALSE,]

p1 <- ggplot() + 
  geom_linerange(data = sub.coef, 
                 aes(x=factor(Variable),ymin=lower,ymax=upper,col=Dep.Var),
                 position=position_dodge(width=0.3),show_guide = FALSE, lwd = 1.5) +
  geom_rect(data=place.rects, aes(xmin=xmin, xmax=xmax),
            ymin = -Inf,ymax = Inf, alpha=0.2,fill = place.rects$fill.col) +
  guides(col = guide_legend(NULL)) + 
  geom_hline(yintercept=0,lty=2) + 
  coord_flip() + theme_tufte(ticks=F)
p1


test = mod.data[mod.data$YEAR==2013,]

head(test)


dat1 <- subset(sub.coef,upper >= 0)
dat2 <- subset(sub.coef,lower < 0)

ggplot() + 
  geom_bar(data = dat1, aes(x=Variable, y=upper, fill=Dep.Var),stat = "identity",position='dodge') +  
  geom_bar(data = dat2, aes(x=Variable, y=lower, fill=Dep.Var),stat = "identity",position='dodge')










summary(mod.base.p1)
summary(mod.base.p2)
summary(mod.base.p3)

summary(mod.project.p1)

summary(mod.project.p2)

summary(mod.project.p3)
covars$OWRI.proj.in.p3.wy

test = read.csv('Input/oweb_download_grant.csv')
library(dplyr)
test = test %>% filter(HUC8 == 17080003,YEAR == 2000)
head(test)
covars[5000:5010,]

summary(mod.project.p1)
17080003          139 2001


summary(mod.project.p3)

covars$OWEB.p3.WC
form_base_24
covars$OWEB.p1.Public

summary(mod.base.12)
summary(mod.base.24)
summary(mod.base.36)

summary(mod.base.p1)
summary(mod.base.p2)

summary(covars$OWRI.proj.in.p1.wy)

hist(covars$WC.grants.in.p1.wy^2)

summary(mod.project.24)


mod.base.12$waic$waic
mod.base.24$waic$waic
mod.base.36$waic$waic




form_project_24
mod_base_24

summary(mod.project.12)
summary(mod_base_24)
summary(mod_project_24)
rownames(mod_project_24$summary.fixed)
summary(mod.project.24)

rownames(mod_project_12$summary.fixed)[18] <- 'Full.Interaction'
rownames(mod_project_24$summary.fixed)[18] <- 'Full.Interaction'
rownames(mod_project_36$summary.fixed)[18] <- 'Full.Interaction'



summary(mod_project_24)
