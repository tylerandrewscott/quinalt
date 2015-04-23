setwd('H:/quinalt')
rm(list=ls())

source('owri.restoration.projects.R')

#save.image('midpoint.0.RData')

source('oregon.wq.data.R')

#save.image('midpoint.1.RData')

source('landcover.by.huc8.R')

#save.image('midpoint.2.RData')

source('add.additional.data.R')

#save.image('midpoint.3.RData')

source('parse.OWEB.data.R')

#save.image('midpoint.4.RData')

#source('create.present.values.R')

#save.iamge('midpoint.5.RData')

source('landuse.buffers.R')

#save.image('midpoint.6.RData')

source('prepare_model.R')

#save.image('midpoint.7.Rdata')

#identify best functional form for spatial effects
source('fit.spde.model.R')

#test sum of all grants, 1/3/5yr rolling sums
source('test.total.grants.R')

#compare effects of restoration grants with non-restoration grants
source('rest.vs.non.rest.R')

#compare all four grant types
source('compare.diff.grants.R')

source('make.obs.by.time.plot.R')
source('make.time.trend.plot.R')


citation(package='raster')
citation(package='sp')
citation(package='INLA')

