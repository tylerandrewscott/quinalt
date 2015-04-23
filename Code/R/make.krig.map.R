
rm(list = ls(all = TRUE))
setwd('H:/quinalt')
load('midpoint.7.v2.RData')
require(xtable)
library(INLA)
library(rgdal);library(rgeos);library(maptools)
library(geoR)
library(plyr)
library(dplyr)

test = readOGR(dsn='government_units','state_nrcs_a_or')

inla.setOption(num.threads=16) 

temp = params.spdf@data



covars = temp[,c('ag.huc8','dev.huc8','forst.huc8','wet.huc8','elevation',
                 'seaDist','HUC8','total.period',
                 'seasonal','Ag','Dev','Wetl','Forst','rsum.oweb.all.3yr',
                 'rsum.oweb.all.1yr','rsum.oweb.all.5yr')]

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
(mesh.a <- inla.mesh.2d(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT),max.edge=c(5, 40),cut=.025))$n

spde.a <- inla.spde2.matern(mesh.a) 

####MODELS 3-4: RANDOM EFFECT CORRELATED BETWEEN YEARS
##MODELS 3-4 USE SAME STACK
# table(gr.3 <- temp$YEAR-(min(temp$YEAR)-1))
# dim(A.3 <- inla.spde.make.A(mesh.a, group=gr.3, loc=cbind(temp$DECIMAL_LONG, temp$DECIMAL_LAT)))
# ind.3 <- inla.spde.make.index(name='s', n.spde=mesh.a$n, n.group=length(unique(gr.3)))
# stk.3 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.3,1),
#                     effects=list(ind.3, list(data.frame(b0=1,covars))))


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


# #Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
form3 <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
  elevation + seaDist  +   f(HUC8,model='iid')+
  f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
  f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

mod3 <- inla(form3, family='gaussian', data=inla.stack.data(stk.3),
             control.predictor=list(A=inla.stack.A(stk.3), compute=T),
             # control.inla=list(strategy='laplace'), 
             control.inla(list(h=1.0)),
             control.compute=list(dic=TRUE, cpo=TRUE))


##########################
### RESULTS 
##########################
#Extract parameters of the random field
rf.mod2 <- inla.spde2.result(mod2, 's', spde.a) 
c(mean=inla.emarginal(function(x) x, rf.mod2$marginals.range[[1]]), 
  q=inla.hpdmarginal(0.95, rf.mod2$marginals.range[[1]]))[c(2,1,3)]

#Summary table of the posterior distributions of all parameters (fixed and random)
tables <- t(sapply(c(mod2$marginals.fix, 
                     mod2$marginals.hy[1:2], 
                     rf.mod2$marginals.range[1]), function(m) 
                       c(mean=inla.emarginal(function(x) x, m), 
                         lim=inla.hpdmarginal(0.95, m)))) 



#Model 3: random effect is correlated between years. autoregressive correlation ($m3a$) 
# form.all.1yr <-y ~ 0 + b0 +  Ag + Forst + Dev  + 
#   elevation + seaDist  + rsum.oweb.all.1yr +  f(HUC8,model='iid')+
#   f(HUC8,model='iid')+ f(total.period,model='rw2') + f(seasonal,model='seasonal',season.length=12)+ 
#   f(s, model=spde.a, group=s.group, control.group=list(model='ar1'))

##################################
### MAP of random field mean and sd (Figure 6)
##################################
library(lattice)

proj<-inla.mesh.projector(mesh.a,xlim=range(coordinates(params.spdf)[,1]), ylim=range(coordinates(params.spdf)[,2]))
str(proj)#matrix 100 x 100
dat = as.data.frame(proj$lattice$loc)
dim(dat)#10000rows

ndat<-data.frame(sapply(dat, rep.int, times=19))
# dim(ndat)#110000, replicated 4 times for each year
ndat$year<-rep(1:19,each=10000)
table(ind.2$s.repl)#476 spde nodes every year
length(mod2$summary.random$s$mean)#1904/4

xmeans <- list()
xsd <- list()
for (j in 1:19) {
  xmeans[[j]] <- inla.mesh.project(proj,mod2$summary.random$s$mean[ind.2$s.repl==j])
  xsd[[j]] <- inla.mesh.project(proj,mod2$summary.random$s$sd[ind.2$s.repl==j])
}

ndat <- data.frame(ndat, unlist(xmeans),unlist(xsd)) 
head(ndat)

ndat$year <- factor(ndat$year+1994)
colnames(ndat)[4:5]<-c("mean","sd")


# pdf("Figures/MeanGRF.pdf")
my.at <- seq(-3500,0,500)
levelplot(mean ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),
          par.strip.text=list(cex=2),strip = strip.custom(bg="white"))



+ 
  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

# pdf("Figures/SDGRF.pdf")
levelplot(sd ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white"))

+  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")

library(maptools)


orshp <- readShapeSpatial("government_units/state_nrcs_a_or.shp")

dim(bgrid <-spsample(orshp,type="hexagonal", n=10000, offset=c(0,0)))
#dim(bgrid <- read.table('Data/gridpred.txt', header=TRUE)) #Grid data
bgrid = data.frame(x=bgrid$x,y=bgrid$y)


library(raster)
m = data.frame(lon = bgrid$x,lat = bgrid$y)
us.alt = getData('alt',country='US')
elevation.df = cbind(m, alt = raster::extract(us.alt[[1]], m, method = "bilinear"))

data = temp

par(mfrow=c(1,1))
with(data, plot(DECIMAL_LONG, DECIMAL_LAT, asp=1))
points(bgrid$x, bgrid$y, col=gray(.7), pch=4, cex=0.01)

head(bgrid)

library(splancs) 
iig <- sapply(1995:2013, function(y) {
  ii.y <- which(data$YEAR==y)
  d <- nndistF(cbind(data$DECIMAL_LONG[ii.y], data$DECIMAL_LAT[ii.y]), cbind(bgrid$x, bgrid$y))
  which(d<.5)
})
str(iig)


itc <- lapply(1:19, function(y) {
  ii.y <- which(data$YEAR==((1995:2013)[y]))
  d <- loccoords(bgrid[iig[[y]],], 
                 cbind(data$DECIMAL_LONG, data$DECIMAL_LAT)[ii.y,])
  w <- exp(-d/0.5)
  #drop((w/rowSums(w))%*%(data$Forst[ii.y]))
})


sapply(itc, summary)

bgrid$elevation = elevation.df$alt
library(ggplot2)
library(plyr)
library(dplyr)

#load oregon boundary shapefile
oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")


#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
#join into one polygon
reg <- unionSpatialPolygons(oregon.huc8, rep(1,91),100)
oregon.huc8.points = fortify(reg, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")
#create oregon coastline dataframe
oregon.coast.df = subset(oregon.huc8.df,long<(-124)&lat>=min(oregon.df[,2]))
coords <- as.matrix(cbind(bgrid$x,bgrid$y))
mat.dists <- spDists(coords, oregon.coast.df[,1:2], longlat=TRUE)
bgrid$"seaDist" <- apply(mat.dists, 1, min)




library(shapefiles)
library(rgdal)
library(raster)

R4.crop = raster('H:/duckabush/tf_crop_2011')
R4.past = raster('H:/duckabush/tf_past_2011')
R4.dev = raster('H:/duckabush/tf_dev_2011')
R4.wet = raster('H:/duckabush/tf_wetl_2011')
R4.forst = raster('H:/duckabush/tf_forst_2011')

uq.points = bgrid[,1:2]
uq.points = sp::SpatialPoints(coords = uq.points,proj4string = CRS(proj4string(oregon)))
library(sp)
library(rgdal)
uq.points = spTransform(uq.points,CRSobj = CRS(proj4string(R4.dev)))


R4.crop.v = raster::extract(R4.crop,uq.points,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.past.v = raster::extract(R4.past,uq.points,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.dev.v = raster::extract(R4.dev,uq.points,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.wet.v = raster::extract(R4.wet,uq.points,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.forst.v = raster::extract(R4.forst,uq.points,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.ag.v = data.frame(ID = R4.crop.v$ID,tf_ag_2011 = R4.crop.v$tf_crop_2011+R4.past.v$tf_past_2011)

R4 = join_all(list(R4.ag.v,R4.dev.v,R4.wet.v,R4.forst.v))

save.image('temp.pred.RData')
bgrid = data.frame(cbind(bgrid,R4))
colnames(bgrid) = c("x" ,"y","elevation", "seaDist" , "ID" , 'Ag','Dev','Wetl','Forst')

which.huc8 = over(spTransform(uq.points,CRSobj = CRS(proj4string(oregon.huc8))),oregon.huc8)
bgrid$HUC8 = which.huc8$HUC8

bgrid$YEAR = 2013
bgrid$total.period = 228
bgrid$seasonal = 228




### Building prediction stacks
### for each year with
###  duration=15.94, Ngillnets=43.44
###  bathymetry over grid and TC.tspp interpolated

A.1 <- inla.spde.make.A(mesh.a, loc=cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT))
ind.1 <- inla.spde.make.index('s', mesh.a$n)

stk.1 <- inla.stack(data=list(y=temp$l.owqi), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))


### Building prediction stacks
### for each year with
###  duration=15.94, Ngillnets=43.44
###  bathymetry over grid and TC.tspp interpolated


Ap <- inla.spde.make.A(mesh.a, cbind(
    bgrid$x[iig[[1]]], bgrid$y[iig[[1]]]))
  
pstack = inla.stack(data=list(y=NA), 
             tag=paste('prd', 1, sep=''), A=list(Ap, 1), 
             effects=list(ind.1, 
                          data.frame(b0=1, 
                                     Forst=bgrid$Forst[iig[[1]]],
                                     Wetl=bgrid$Wetl[iig[[1]]],
                                     Dev=bgrid$Dev[iig[[1]]],
                                     elevation=bgrid$elevation[iig[[1]]],
                                     seaDist=bgrid$seaDist[iig[[1]]],
                                     YEAR = bgrid$YEAR[iig[[1]]], 
                                     HUC8 = bgrid$HUC8[iig[[1]]],
                                     total.period = bgrid$HUC8[iig[[1]]],
                                     seasonal = bgrid$HUC8[iig[[1]]]
                                   )))


stk = inla.stack(stk.2,pstack)
res = inla(form2, family='gaussian', data=inla.stack.data(stk),
           control.predictor=list(A=inla.stack.A(stk), compute=TRUE),
           #  control.inla=list(strategy='laplace'), 
           control.compute=list(dic=TRUE, cpo=TRUE))
print(res$cpu)
ii <- inla.stack.index(stk, names(pstack$effects$index))$data 

yprd = res$summary.linear.pred[ii,]

jprd <- data.frame(
  x=bgrid$x[iig[[1]]], y=bgrid$y[iig[[1]]], lpred=yprd$mean, YEAR = 2013)

write.table(jprd, 'jprd.txt', row.names=FALSE) #save predictions



#read the prediction outputs
jprd <- read.table('jprd.txt',header=T)


proj = proj4string(oregon.huc8)
res0 <- SpatialPoints(cbind(jprd$x, jprd$y), CRS(proj))
##res0 <- sp:::as.SpatialPolygons.SpatialPixels(SpatialPixels(pts))##, data.frame(pred=exp(jprd$lpred)))
##plot(SpatialPixels(pts))

#newproj <- "+proj=longlat +zone=19 +ellps=GRS80" # lat/long
res <- SpatialPointsDataFrame(spTransform(res0, CRS(proj4string(oregon.huc8))),
                         data.frame(pred=jprd$lpred))
library(ggthemes)

library(ggmap)



obox = bbox(oregon.huc8)
obox[1,1] = -125
obox[1,2]= -116
obox[2,1] = 41.5
obox[2,2] = 46.5

test = get_map(location = obox,maptype = 'toner',source='osm')

res.df = data.frame(x=coordinates(res)[,1],y=coordinates(res)[,2],pred = res$pred)

dec12pred = ggmap(test)+ geom_point(aes(x=x,y=y,colour=pred),data=res.df,size=3.5,shape=19)+ theme_tufte(ticks=F)+
  scale_colour_gradient2_tableau(name='December 2012 \nPrediction',limits=c(0,100))+
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.direction='vertical',legend.title=element_text(size=18),
        legend.text = element_text(size=14),
        #legend.background=element_rect(fill = 'white',colour='black'),
        legend.position = c(.67,.3))



  scale_fill_gradient_tableau(palette ='Red'

summary(res$pred)

mean(res$pred)
2.02



q <- seq(0,100,25)
leg <- paste(paste(c('<', q[2:4], '>'), colapse='', sep=''),
             c('', rep('-', 3), ''),
             paste(c(q[2:5], q[5]), colapse='', sep=''), sep='')

# jpeg("Figures/predictions.jpeg")
par(mfrow=c(2,2), mar=c(2,2,0.5, 0.5), mgp=c(2,1,0))#mgp:affect the margin line for the axis
for (i in 1:4) {
  if (i != 3){
    
    par(mfrow=c(1,1))
    plot(oregon,col="white",axes=T,border="black",xaxt="n",yaxt="n")# Canada land
    plot(oregon.huc8,col="white",axes=T,border="grey",add=T) # Greenland land
    ctest<-c("#C0BCBC","#7F7777","#585353","#373535","#0D0C0C")
    plot(res, col=ctest[findInterval(res$pred, q)], cex=.8, pch=19)
    
    min(res$pred)
res$pred[res$pred>100] = 100
    sum(res$pred>100)
  max(res$pred)
    
    contour(r1,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
    plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
    lines(mpa) # marine protected area zone
    ctest<-c("#C0BCBC","#7F7777","#585353","#373535","#0D0C0C")
    plot(res[[i]], add=T, col=ctest[findInterval(res[[i]]$pred, q)], cex=0.1, pch=19)
    #     legend(-50.5, 71, leg, fill=ctest)
    text(-65,66,labels=eval(i+2007),font=2,cex=1.2)
  }
  else {
    plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey")# to remove x axis # Canada land
    plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey",add=T) # Greenland land
    contour(r1,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
    plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
    lines(mpa) # marine protected area zone
    plot(res[[i]], add=T, col=ctest[findInterval(res[[i]]$pred, q)], cex=0.1, pch=19)
    text(-65,66,labels=eval(i+2007),font=2,cex=1.2)
    legend(-57, 71, leg, fill=ctest,bg="white")
  }
}
# dev.off()
ctest<-c("#C0BCBC","#7F7777","#585353","#373535","#0D0C0C")
plot(res, col=ctest[findInterval(res$pred, q)], cex=0.1, pch=19)
plot(res, col=ctest[findInterval(res$pred, q)], cex=0.1, pch=19)
head(res)
res$pred
res$marginals.fixed
