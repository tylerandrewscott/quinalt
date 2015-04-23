rm(list=ls())

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")


setwd('//Users/TScott/Downloads/oregon_outline')
list.files()
outline.path <- 
getwd()



oregon.out = readOGR(dsn="//Users/TScott/Downloads/or_state_boundary", layer='or_state_boundary')
oregon.out@data$id = rownames(oregon.out@data)
oregon.out.points = fortify(oregon.out, region="id")
oregon.out.df = join(oregon.out.points, oregon.out@data, by="id")

oregon.huc8 = readOGR(dsn="//Users/TScott/Downloads/oregon_HUC8", 
                      layer='Hydologic_Boundaries__4th_Level__HUC8_')
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

oregon.riv = readOGR(dsn="//Users/TScott/Downloads/oregon_rivers", 
                      layer='Rivers__1_100k_')
oregon.riv@data$id = rownames(oregon.riv@data)
oregon.riv.points = fortify(oregon.riv, region="id")
oregon.riv.df = join(oregon.riv.points, oregon.riv@data, by="id")


head(oregon.out.df)
oregon.plot<-ggplot()+
  geom_path(aes(long,lat,group=group),colour='black',data=oregon.out.df)+
  coord_equal()+
  geom_path(aes(long,lat,group=group),colour='dark grey',data=oregon.huc8.df)

print(oregon.plot)









oregon.huc8 = readOGR(dsn="//Users/TScott/Downloads/oregon_HUC8", layer='Hydologic_Boundaries__4th_Level__HUC8_')
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")


?system.fi


dsn <- system.file("vectors/test_trk2.gpx", package = "rgdal")[1]
test_trk2 <- try(readOGR(dsn=dsn, layer="tracks"))
if (class(test_trk2) != "try-error") summary(test_trk2)
test_trk2pts <- try(readOGR(dsn=dsn, layer="track_points"))
if (class(test_trk2pts) != "try-error") summary(test_trk2pts)
dsn <- system.file("vectors", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="trin_inca_pl03")
birds <- readOGR(dsn=dsn, layer="trin_inca_pl03")
summary(birds)



huc8.plot<-ggplot(oregon.huc8.df)+
  aes(long,lat,group=group)+
  geom_path(colour='dark grey')+
  coord_equal()

print(huc8.plot)



print(huc8.plot)


oregon_wc = readOGR(dsn=".", layer='WBD_OR.gdb')

oregon_wc@data$id = rownames(oregon_wc@data)
oregon_wc.points = fortify(oregon_wc, region="id")
oregon_wc.df = join(oregon_wc.points, oregon_wc@data, by="id")

wc.plot<-ggplot(oregon_wc.df)+
  aes(long,lat,group=group,fill=instname)+
  geom_polygon()+
  geom_path(colour='white')+
  coord_equal()

print(wc.plot)
qplot(oregon_)

+
  scale_fill_brewer("Oregon Watershed Councils")

ggplot(utah.df) + 
  aes(long,lat,group=group,fill=LEVEL3_NAM) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Utah Ecoregion")


utah = readOGR(dsn=".", layer="eco_l3_ut")
utah@data$id = rownames(utah@data)
utah.points = fortify(utah, region="id")
utah.df = join(utah.points, utah@data, by="id")



fohuc8<-ggplot2::fortify(ohuc8)
fohuc8<-fortify.sp(ohuc8)

ggmap(ohuc8)

owc.map<-get_map(ohuc8)
class(ohuc8)


names(ohuc8)
head(ohuc8)
ohuc8<-ohuc8[nc90$STATE_NAME!='Alaska'&nc90$STATE_NAME!='Hawaii',]

fohuc8<-ggplot2::fortify(ohuc8)



fnc90<-fortify(nc90)
nc90dens<-merge(nc90,mydf,by.x='FIPS',by.y='code')

fnc <- fortify(nc90dens)

fncd <- cbind(fnc, nc90dens@data[fnc$id,])

gg<-ggplot(fncd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = density), colour = alpha("white", 1/2), size = 0.2) +
  scale_fill_continuous(name='Population Density (/sq. mile)',limits=c(0,1500),#labels=c('0','500','1000','>1500'),
                        low='light grey',high='black')+xlab('Longitude')+
  geom_point(aes(x=lon,y=lat,group=schoolloc$Name,size=NIH2013/10000000),
             colour='red',data=schoolloc[schoolloc$lat>22,])+
  scale_size('2013 NIH Awards',breaks=c(10,20,30,40),
             labels=c('$100M','$200M','$300M','$400M'))+
  ylab('Latitude')+
  ggtitle('Population Density and NIH Awards to Medical Schools (2013)')+
  theme(axis.title=element_blank(),axis.text=element_blank(),
        axis.ticks=element_blank(),panel.grid=element_blank())
print(gg)
