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
