rm(list=ls())

load('Code/R/Revisions/test.results.RData')

load('temp_workspace_data.RData')

plot(mesh.a)
points(all.params.spdf)

plot(mesh.a)
plot(all.params.spdf,add=TRUE)


plot(mesh.a)
points(mesh.a$loc[,2]~mesh.a$loc[,1],col='red')


library(texreg)
library(ggplot2)




library(coda)
library(gdata)


summary(mod.base.p1)


library(INLA)

draws = 100000
oweb.p1 = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC)
oweb.p1.staff = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)
oweb.p1.budg = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)
oweb.p1.years = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`)
oweb.p1.all = inla.rmarginal(draws, mod.base.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC) + 
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC:YEARS.ACTIVE`)

oweb.p2 = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC)
oweb.p2.staff = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)
oweb.p2.budg = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)
oweb.p2.years = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`)
oweb.p2.all = inla.rmarginal(draws, mod.base.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC) + 
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC:YEARS.ACTIVE`)

oweb.p3 = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC)
oweb.p3.staff = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)
oweb.p3.budg = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)
oweb.p3.years = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`)
oweb.p3.all = inla.rmarginal(draws, mod.base.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC) + 
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:STAFF.FTE`)+
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:OP.BUDGET.200k`)+
  inla.rmarginal(draws, mod.base.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC:YEARS.ACTIVE`)
 
library(tidyr)
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
library(ggthemes)
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
summary(mod.swcd.p1)

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

bas.col = 'black'
alt.col = '#E69F00'

library(ggplot2)

margs.lon$uq = as.factor(margs.lon$uq)

levels(margs.lon$uq) = c("SWCD 1yr ", "SWCD 2yr " ,"SWCD 3yr ", "WC 1yr",  "WC 2yr"  , "WC 3yr"  )

bas.col = 'black'
alt.col = '#E69F00'
alt.col2 = "#56B4E9"

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



draws = 100000
oweb.project.p1.all.Tech = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Tech:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.all.Restoration = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Restoration:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.all.Outreach = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Outreach:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p2.all.Tech = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Tech:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.all.Restoration = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Restoration:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.all.Outreach = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Outreach:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p3.all.Tech = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Tech) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Tech`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Tech:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.all.Restoration = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Restoration) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Restoration`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Restoration:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.all.Outreach = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Outreach) + 
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Outreach`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Outreach:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p1.fixed.Tech = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Tech) + 
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Tech`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Tech`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Tech:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.fixed.Restoration = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Restoration) + 
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Restoration`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Restoration`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Restoration:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p1.fixed.Outreach = inla.rmarginal(draws,mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Outreach) + 
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.1yr.WC.Outreach`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.1yr.WC.Outreach`)+
#  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.1yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$`OWEB.proj.in.last.1yr.WC.Outreach:OWEB.proj.in.last.1yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p1$marginals.fixed$OWEB.proj.in.last.1yr.WC.Interaction)

oweb.project.p2.fixed.Tech = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Tech) + 
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Tech`)+
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Tech`)+
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Tech`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Tech:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.fixed.Restoration = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Restoration) + 
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Restoration`)+
 # inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Restoration`)+
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Restoration`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Restoration:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p2.fixed.Outreach = inla.rmarginal(draws,mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Outreach) + 
 # inla.rmarginal(draws, mod.project.p2$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.2yr.WC.Outreach`)+
#  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.2yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p2$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.2yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$`OWEB.proj.in.last.2yr.WC.Outreach:OWEB.proj.in.last.2yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p2$marginals.fixed$OWEB.proj.in.last.2yr.WC.Interaction)

oweb.project.p3.fixed.Tech = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Tech) + 
 # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Tech`)+
#  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Tech`)+
 # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Tech`) +
 inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Tech:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.fixed.Restoration = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Restoration) + 
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Restoration`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Restoration`)+
 # inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Restoration`) +
 inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Restoration:OWEB.proj.in.last.3yr.WC.Capacity`)+
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)

oweb.project.p3.fixed.Outreach = inla.rmarginal(draws,mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Outreach) + 
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`STAFF.FTE:OWEB.proj.in.last.3yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OP.BUDGET.200k:OWEB.proj.in.last.3yr.WC.Outreach`)+
  #inla.rmarginal(draws, mod.project.p3$marginals.fixed$`YEARS.ACTIVE:OWEB.proj.in.last.3yr.WC.Outreach`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$`OWEB.proj.in.last.3yr.WC.Outreach:OWEB.proj.in.last.3yr.WC.Capacity`) +
  inla.rmarginal(draws, mod.project.p3$marginals.fixed$OWEB.proj.in.last.3yr.WC.Interaction)


margs = data.frame(
  oweb.project.p1.all.Tech,
  oweb.project.p1.all.Restoration,
  oweb.project.p1.all.Outreach,
  oweb.project.p2.all.Tech,
  oweb.project.p2.all.Restoration,
  oweb.project.p2.all.Outreach,
  oweb.project.p3.all.Tech,
  oweb.project.p3.all.Restoration,
  oweb.project.p3.all.Outreach,
  oweb.project.p1.fixed.Tech,
  oweb.project.p1.fixed.Restoration,
  oweb.project.p1.fixed.Outreach,
  oweb.project.p2.fixed.Tech,
  oweb.project.p2.fixed.Restoration,
  oweb.project.p2.fixed.Outreach,
  oweb.project.p3.fixed.Tech,
  oweb.project.p3.fixed.Restoration,
  oweb.project.p3.fixed.Outreach)
library(tidyr)
margs.lon = gather(margs)

margs.lon$window = NA
margs.lon$window[grep('p1.',margs.lon$key)] <- 'p1'
margs.lon$window[grep('p2.',margs.lon$key)] <- 'p2'
margs.lon$window[grep('p3.',margs.lon$key)] <- 'p3'

margs.lon$which = NA
margs.lon$which[grep('Outreach',margs.lon$key)] <- 'Outreach'
margs.lon$which[grep('Tech',margs.lon$key)] <- 'Tech'
margs.lon$which[grep('Restoration',margs.lon$key)] <- 'Restoration'

margs.lon$fix = NA
margs.lon$fix[grep('all',margs.lon$key)] <- 'All'
margs.lon$fix[grep('fixed',margs.lon$key)] <- 'Fixed'

margs.lon$uq <- paste(margs.lon$which,margs.lon$fix,margs.lon$window)
margs.lon$uq = as.factor(margs.lon$uq)

bas.col = 'black'
alt.col = '#E69F00'
alt.col2 = "#56B4E9"
library(dplyr)
library(ggplot2)

head(margs.lon)
short.labs1 <- c('1yr w/ ','2yr w/','3yr w/',
                '1yr w/out','2yr w/out','3yr w/out')
short.labs2 <- c('1yr w/ ','2yr w/','3yr w/',
                 '1yr w/out','2yr w/out','3yr w/out')
ggplot(filter(margs.lon,which=='Outreach'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#E69F00','black'),each=3),
                     name='Outreach Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Outreach Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
 guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.85,.6),
        axis.title=element_text(size=20),
        axis.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Outreach + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0),limits = c(-30,60))+
  scale_y_continuous(expand=c(0,0))
library(scales)

ggplot(filter(margs.lon,which=='Tech'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#56B4E9','black'),each=3),
                     name='Tech. Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Tech. Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.20,.6),
        axis.title=element_text(size=20),
        axis.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Tech. + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))

ggplot(filter(margs.lon,which=='Restoration'))+ 
  geom_density(aes(x=value, color=uq,linetype=uq),lwd=1)+
  scale_color_manual(values=rep(c('#009E73','black'),each=3),
                     name='Restoration Projects \n w/, w/out Capacity $', labels=short.labs1) +
  scale_linetype_manual(values=rep(c(1:3),2),
                        name='Restoration Projects \n w/, w/out Capacity $', labels=short.labs1)  +
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3))+
  guides(col=guide_legend(ncol=2,nrow=3),linetype=guide_legend(ncol=2,nrow=3)) +
  theme_bw() +theme_tufte(ticks=FALSE) +
  theme(legend.position = c(.30,.6),
        axis.title=element_text(size=20),
        axis.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))  +
  xlab('Sampled Posterior Marginals: Restoration + capacity building') + 
  ylab('') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))



#Very Poor: 0-59, Poor: 60-79, Fair: 80-84, Good: 85-89, Excellent: 90-100

covars$owqi2 <- covars$owqi + 4
covars$owqi2 <- ifelse(covars$owqi2>100,100,covars$owqi2)

covars$Qual = 'Excellent'
covars$Qual[covars$owqi < 90] <- 'Good'
covars$Qual[covars$owqi < 85] <- 'Fair'
covars$Qual[covars$owqi < 80] <- 'Poor'
covars$Qual[covars$owqi < 60] <- 'Very Poor'

covars$Qual = ordered(as.factor(covars$Qual),levels = c("Very Poor","Poor",'Fair','Good','Excellent'))

covars$Qual2 = 'Excellent'
covars$Qual2[covars$owqi2 < 90] <- 'Good'
covars$Qual2[covars$owqi2 < 85] <- 'Fair'
covars$Qual2[covars$owqi2 < 80] <- 'Poor'
covars$Qual2[covars$owqi2 < 60] <- 'Very Poor'
covars$Qual2 = ordered(as.factor(covars$Qual2),levels = c("Very Poor","Poor",'Fair','Good','Excellent'))

both.covars = c(covars$owqi,covars$owqi2)
which.owqi = rep(c('Observed','Observed + 4'),each=length(covars$owqi))



library(ggthemes)

ggplot() +
  geom_rect(aes(xmin=c(15,60,80,85,90),xmax=c(60,80,85,90,100),ymin=0,ymax=0.08),
            fill = c("#D55E00", "#E69F00","#999999", "#56B4E9", "#009E73"),alpha = 0.2)+
 stat_density(aes(x=owqi,fill='grey'),data=covars,trim=TRUE,alpha=0.5,colour='black') +
 stat_density(aes(x=owqi2,fill='blue'),data=covars,trim=TRUE,alpha=0.5,colour='black')+
  geom_vline(x=60,lty=2,col='grey50')+ geom_vline(x=80,lty=2,col='grey50')+geom_vline(x=85,lty=2,col='grey50') +geom_vline(x=90,lty=2,col='grey50')+
  theme_bw() +
  scale_x_continuous('OWQI',expand=c(0,0),breaks=c(seq(20,90,10))) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.08)) +
  theme_tufte(ticks=F)+
  theme(
    legend.position = c(0.25,.5),
    axis.ticks=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=16),
    legend.text=element_text(size=14),
    legend.title=element_text(size=16)
    )  +
  scale_fill_colorblind(labels=c('Observed+4','Observed'),name='Distribution of Scores')+
  geom_text(aes(x=c(30,70,82.5,87.5,95),label=c("Very Poor","Poor",'Fair','Good','Excellent'),
                y=c(0.07,0.07,0.075,0.07,0.075)))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))
         #color = guide_legend(override.aes = list(linetype = 0)))

show_col(colorblind_pal()(8))
