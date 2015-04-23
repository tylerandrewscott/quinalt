# Supplementary Materials: Applying Bayesian spatio-temporal models to fisheries
# bycatch in the Canadian Arctic R codes for the final model, inferences, and
# predictions

### Please refer to the INLA website: www.r-inla.org for additional information
rm(list = ls(all = TRUE))
##################################
### MAP of BAFFIN BAY (Figure 1a)
##################################
library(sp)
library(maptools)
library(spatstat)
library(rgdal)
library(reshape2)
library(spdep)
library(maps)
library(mapdata)
library(marmap)

setwd('H:/fish')

# load datasets
data <- read.table('Data/fackdata.txt', header=TRUE) #Greenland shark bycatch data
eez <- readOGR(dsn="Data/EEZ", layer='Atleez') # Canadian eez boundary


MPAlat<-c(68.25,68.25,67.25,67.25,68.25)
MPAlong<-c(-58.551306,-60.5,-60.5,-57.8425,-58.551306)
mpa<-cbind(MPAlong,MPAlat)

ca<-map("worldHires", "Canada", fill=TRUE, col="transparent", plot=FALSE)# in package "mapdata"
IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])
ca.sp <- map2SpatialPolygons(ca, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

Greenland<-map("worldHires", "Greenland", fill=TRUE, col="transparent", plot=FALSE)##in package "mapdata"
IDs <- sapply(strsplit(Greenland$names, ":"), function(x) x[1])
GR.sp <- map2SpatialPolygons(Greenland, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

iso<-read.bathy("Data/etopo1_bedrock.xyz",sep=" ") # read.bathy {marmap} data from NOAA: http://maps.ngdc.noaa.gov/viewers/wcs-client/

r1 <- as.raster(iso) # {marmap}
newproj <- "+proj=longlat +datum=WGS84"
# Note: package issue with marmap & raster package. Need to make sure that the raster package is not uploaded before using the as.raster() command from marmap

library(lattice)
library(raster)
r2 <- projectRaster(r1,crs=newproj)

data.sp<-data
coordinates(data.sp) <- ~longitude+latitude

# pdf(file="Figures/BaffinBayMap.pdf")
plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="lightgrey",axes=T,border="grey")# Canada land #,xaxt="n",yaxt="n"
plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="lightgrey",axes=T,border="grey",add=T) # Greenland land
contour(r2,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
lines(mpa) # marine protected area zone
points(data.sp,pch=20) # fishing locations
# dev.off()

##########################
### SPDE and INLA approach
##########################
library(INLA)

###Step 1) Prepare the mesh
mesh <- inla.mesh.2d(cbind(data$X, data$Y), max.edge=c(40, 80), cut=5)
plot(mesh,asp=1)
###Step 2) Define the SPDE
spde <- inla.spde2.matern(mesh)
###Step 3) Create observation matrix $A$ for each year
table(repl <- data$year-2007)#year index from 1:4
dim(A <- inla.spde.make.A(mesh, repl=repl, loc=cbind(data$X, data$Y)))
###Step 4) Create index and stack data
ind <- inla.spde.make.index(name='s', n.spde=spde$n.spde, n.repl=4)

head(data)
# y is Greenland shark bycatch (counts)
# covariates are specified by data[,c(4:7)]
stke <- inla.stack(data=list(gr=data$y), 
                   tag='est', A=list(A, 1), 
                   effects=list(ind, 
                       data.frame(b0=1, data[,c(4:7)])))

###Step 5) Specify the INLA formula 
form <-  gr ~ 0 + b0 + I(duration) + Ngillnets + I(TC.tspp) + 
    f(inla.group(bathymetry), model='rw1') + #inla.group reduce the number to unique values
    f(s, model=spde, replicate=s.repl) 

###Step 6) Call INLA
rl.nb2.gr <- inla(form, family='zeroinflatednbinomial2', 
                 data=inla.stack.data(stke), 
                control.compute=list(dic=TRUE), 
                 control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                 control.predictor=list(A=inla.stack.A(stke)),inla.call='remote') ## Run the model remotely

# save(rl.nb2.gr,file="outputs/rl.nb2.gr.Rdata")

##################################
### MAP of BAFFIN BAY + MESH (Figure 1b)
##################################
# Project to km Canadian/Greenland 
Arcticproj<-"+proj=utm +zone=19 +ellps=GRS80 +units=km +no_defs"
ca.km <- spTransform(ca.sp, CRS(Arcticproj))
GR.km <- spTransform(GR.sp, CRS(Arcticproj))
library(raster)
r.km <- projectRaster(r1,crs=Arcticproj)

# pdf(file="Figures/BFMapMesh.pdf")
plot(mesh,asp=1,main=NULL,sub=NULL)
plot(ca.km,add=T)
plot(GR.km,add=T)
box(lwd=2)
# dev.off()

##########################
### RESULTS 
##########################
#Extract parameters of the random field
rf.gr <- inla.spde2.result(rl.nb2.gr, 's', spde) 
c(mean=inla.emarginal(function(x) x, rf.gr$marginals.range[[1]]), 
  q=inla.hpdmarginal(0.95, rf.gr$marginals.range[[1]]))[c(2,1,3)]

#Summary table of the posterior distributions of all parameters (fixed and random)
tables <- t(sapply(c(rl.nb2.gr$marginals.fix, 
                     rl.nb2.gr$marginals.hy[1:2], 
                     rf.gr$marginals.range[1]), function(m) 
                       c(mean=inla.emarginal(function(x) x, m), 
                         lim=inla.hpdmarginal(0.95, m)))) 
round(tables, 4)

# Relative effect of the fixed effects
# Duration in hours and decimal minutes
exp(rl.nb2.gr$summary.fix[2,1])  
# Ngillnets - number of gillnet panels per haul
exp(rl.nb2.gr$summary.fix[3,1])  
# TC.tspp - Greenland halibut cacth weight (in metric tonnes)
exp(rl.nb2.gr$summary.fix[4,1])

##################################
### MAP of random field mean and sd (Figure 6)
##################################
library(lattice)
head(gr1)
proj<-inla.mesh.projector(mesh,xlim=range(data$X), ylim=range(data$Y))
str(proj)#matrix 100 x 100
dat = as.data.frame(proj$lattice$loc)
dim(dat)#10000rows
table(gr1$year)
ndat<-data.frame(sapply(dat, rep.int, times=4))
# dim(ndat)#110000, replicated 4 times for each year
ndat$year<-rep(1:4,each=10000)
table(ind$s.repl)#476 spde nodes every year
length(rl.nb2.gr$summary.random$s$mean)#1904/4

xmeans <- list()
xsd <- list()
     for (j in 1:4) {
         xmeans[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$mean[ind$s.repl==j])
        xsd[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$sd[ind$s.repl==j])
     }
   
ndat <- data.frame(ndat, unlist(xmeans),unlist(xsd)) 
head(ndat)
ndat$year <- factor(ndat$year+2007)
colnames(ndat)[4:5]<-c("mean","sd")

# pdf("Figures/MeanGRF.pdf")
my.at <- seq(-3500,0,500)
levelplot(mean ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) +  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

# pdf("Figures/SDGRF.pdf")
levelplot(sd ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) +  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

##########################
### PREDICTIONS
##########################
dim(bgrid <- read.table('Data/gridpred.txt', header=TRUE)) #Grid data
head(bgrid,2)

par(mfrow=c(1,1))
with(data, plot(X, Y, asp=1))
points(bgrid$x, bgrid$y, col=gray(.7), pch=4, cex=0.01)

#*Grid locations* We used our final model to predict expected bycatch in areas 
#neigbouring observed fishing hauls. However, since the fishery expended 
#spatially, the spatial fishing pattern changed over time with some years with 
#no fishing in some areas of our domain (e.g., northeast cluster at 71 degrees 
#was not fished in 2008 and 2009). For these area-time, we did not predict
#bycatch. However, note that it would be possible to do so (assuming some
#average fishing conditions (duration, Ngillnets, TC.tspp), but the spatial
#random effect would be very close to zero and with large variance. We selected
#nearest neighbour grid points less than 30km from our fishing hauls
#using nndistF function from [splancs].

library(splancs) 
iig <- sapply(2008:2011, function(y) {
    ii.y <- which(data$year==y)
    d <- nndistF(cbind(data$X[ii.y], data$Y[ii.y]), cbind(bgrid$x, bgrid$y))
    which(d<30)
})
str(iig)

# *Prediction of Greenland halibut (TC.tspp)* Since Greenland halibut catch (kg)
# are driven by unknown variable, we also interpolated TC.tspp over the grid for
# each year. The interpolation was done using a weigthed mean, where the weights
# are proportional to exp(-dist/a), where 'a' is equal to 0.5km, that is inversely
# proportional to the total grid distance (1x1km).

library(geoR)
itc <- lapply(1:4, function(y) {
    ii.y <- which(data$year==((2008:2011)[y]))
    d <- loccoords(cbind(bgrid$x, bgrid$y)[iig[[y]],], 
                   cbind(data$X, data$Y)[ii.y,]) 
    w <- exp(-d/0.5)
    drop((w/rowSums(w))%*%(data$TC.tspp[ii.y]))
})
sapply(itc, summary)
summary(data$TC)

### Building prediction stacks
### for each year with
###  duration=15.94, Ngillnets=43.44
###  bathymetry over grid and TC.tspp interpolated
y.stk.p <- lapply(1:4, function(y) {
    cat('year', (2008:2011)[y], '\n')
    Ap <- inla.spde.make.A(mesh, cbind(
        bgrid$x[iig[[y]]], bgrid$y[iig[[y]]]), repl=y, n.repl=4)
    inla.stack(data=list(gr=NA), 
               tag=paste('prd', y, sep=''), A=list(Ap, 1), 
               effects=list(ind, 
                   data.frame(b0=1, bathymetry=bgrid$bathymetry[iig[[y]]],
                              duration=15.98, Ngillnets=43.44,
                              TC.tspp=itc[[y]])))
})

### we can join all stack data into one and run it all at once, 
### but this is expensive. So we use each prediction stack per year.
yprd <- lapply(y.stk.p, function(s) {
    stk <- inla.stack(stke, s) 
    res <- inla(form, family='zeroinflatednbinomial2', 
                data=inla.stack.data(stk), quantiles=NULL, 
                control.predictor=list(A=inla.stack.A(stk), compute=TRUE, quantiles=NULL), 
                control.compute=list(return.marginals=FALSE), 
                inla.call='remote')
    print(res$cpu)
    ii <- inla.stack.index(stk, names(s$effects$index))$data 
    res$summary.linear.pred[ii,]
})

### join all into data.frame
jprd <- Reduce('rbind', lapply(1:4, function(y) data.frame(
    x=bgrid$x[iig[[y]]], y=bgrid$y[iig[[y]]], lpred=yprd[[y]]$mean, year=(2008:2011)[y])))

write.table(jprd, 'jprd.txt', row.names=FALSE) #save predictions

##################################
### MAPs of Prediction (Figure 7)
##################################
#read the prediction outputs
jprd <- read.table('jprd.txt',header=T)
jprd.y <- split(jprd, jprd$year)
str(jprd.y)

proj <- "+proj=utm +zone=19 +ellps=GRS80 +units=km +no_defs" #old projection
res0 <- lapply(jprd.y, function(x) SpatialPoints(cbind(x$x, x$y), CRS(proj)))
##res0 <- sp:::as.SpatialPolygons.SpatialPixels(SpatialPixels(pts))##, data.frame(pred=exp(jprd$lpred)))
##plot(SpatialPixels(pts))

newproj <- "+proj=longlat +zone=19 +ellps=GRS80" # lat/long
res <- lapply(1:length(res0), function(i)
              SpatialPointsDataFrame(spTransform(res0[[i]], CRS(newproj)),
                              data.frame(pred=exp(jprd.y[[i]]$lpred))))

q <- c(0, 1, 3, 5, 10, 30)
leg <- paste(paste(c('<', q[2:4], '>'), colapse='', sep=''),
             c('', rep('-', 3), ''),
             paste(c(q[2:5], q[5]), colapse='', sep=''), sep='')

# jpeg("Figures/predictions.jpeg")
par(mfrow=c(2,2), mar=c(2,2,0.5, 0.5), mgp=c(2,1,0))#mgp:affect the margin line for the axis
for (i in 1:4) {
    if (i != 3){
    plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="lightgrey",xaxt="n",yaxt="n")# Canada land
    plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey",add=T) # Greenland land
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

