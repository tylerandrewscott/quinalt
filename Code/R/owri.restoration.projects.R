
##CALCULATE OWRI ON RESTORATION PROJECT SPENDING BY HUC8 and YEAR
###################
require(foreign)
require(plyr)
require(dplyr)
require(rgdal)
require(sp)
require(rgeos)
require(maptools)
require(ggplot2)
require(reshape2)

setwd("/homes/tscott1/win/user/quinalt_original/")

oregon.huc8 = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)

oregon.owri.points = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_point_projects")
oregon.owri.points@data$id = rownames(oregon.owri.points@data)
oregon.owri.points = spTransform(oregon.owri.points,CRS(proj4string(oregon.huc8)))
oregon.owri.points.df = as.data.frame(oregon.owri.points)

oregon.poly.proj = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_poly_projects")
oregon.poly.proj@data$id = rownames(oregon.poly.proj@data)
oregon.poly.proj.points = fortify(oregon.poly.proj, region="id")
oregon.poly.proj.df = join(oregon.poly.proj.points, oregon.poly.proj@data, by="id")

oregon.line.proj = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_line_projects")
oregon.line.proj@data$id = rownames(oregon.line.proj@data)
oregon.line.proj.points = fortify(oregon.line.proj, region="id")
oregon.line.proj.df = join(oregon.line.proj.points, oregon.line.proj@data, by="id")

#convert line and poly projects to point projects using centroids
poly.centroids<-coordinates(matrix(cbind(oregon.poly.proj@data$point_x,oregon.poly.proj@data$point_y),ncol=2))
oregon.poly.proj.points = SpatialPointsDataFrame(coords=poly.centroids,data=oregon.poly.proj@data,proj4string=CRS(proj4string(oregon.poly.proj)))

line.centroids<-coordinates(matrix(cbind(oregon.line.proj@data$point_x,oregon.line.proj@data$point_y),ncol=2))
oregon.line.proj.points = SpatialPointsDataFrame(coords=line.centroids,data=oregon.line.proj@data,proj4string=CRS(proj4string(oregon.line.proj)))

#make one larg spatialpointsdf with all point/poly/line projects as points
oregon.poly.proj.points = spTransform(oregon.poly.proj.points,CRS(proj4string(oregon.owri.points)))
oregon.line.proj.points = spTransform(oregon.line.proj.points,CRS(proj4string(oregon.owri.points)))
all.points.list = list(oregon.owri.points,oregon.line.proj.points,oregon.poly.proj.points)
all.points.list[[2]]@data = all.points.list[[2]]@data[,names(oregon.owri.points@data)]
all.points.list[[3]]@data = all.points.list[[3]]@data[,names(oregon.owri.points@data)]
all.points  = do.call("rbind", all.points.list)

all.points@data$id = rownames(all.points@data)
all.points.df = as.data.frame(all.points)


all.points.uq.df = all.points.df %>% select(-gis_source,-site_id,-id,-point_x,-point_y,-coords.x1,-coords.x2)
all.points.uq.df = all.points.uq.df[!duplicated(all.points.uq.df$project_nb),]
all.points.uq.df$PROJNUM = all.points.uq.df$project_nb

proj.info = join(proj.info,all.points.uq.df,type='left')




owri.projects<-read.dbf('owri_projects.dbf')
owri.projects$project_nb<-owri.projects$PROJNUM
owri.projects$OWRI.funded = ifelse(owri.projects$TotalBoth==0,0,1)
owri.projects = owri.projects[owri.projects$OWRI.funded==1,]


temp = stack(table(all.points.df$project_nb))
colnames(temp) <- c('pnum','project_nb')
temp$project_nb<-as.character(temp$project_nb)
owri.projects$project_nb<-as.character(owri.projects$project_nb)
owri.projects = join(owri.projects,temp)
owri.projects$pnum[is.na(owri.projects$pnum)] = 1
owri.projects$TotalDiv = owri.projects$TotalBoth / owri.projects$pnum


all.points@data <- join(all.points@data,owri.projects)

which8.owri.points = over(all.points,oregon.huc8)

all.points@data = join(all.points@data,which8.owri.points)

temp =stack(tapply(all.points@data$TotalDiv,as.character(all.points@data$HUC8),sum))
colnames(temp) <- c('SpendHUC8','HUC8')
oregon.huc8@data <- join(oregon.huc8@data,temp)

head(oregon.huc8@data)


oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

library(lubridate)

owri.projects$CompleteYe[owri.projects$CompleteYe==0] = owri.projects$StartYear[owri.projects$CompleteYe==0]
owri.projects$CompleteMo[owri.projects$CompleteMo==0] = owri.projects$StartMonth[owri.projects$CompleteMo==0]
owri.projects$CompleteYe[owri.projects$CompleteYe==0] = year(ymd(owri.projects$ReportDate[owri.projects$CompleteYe==0]))
owri.projects$CompleteMo[owri.projects$CompleteMo==0] = month(ymd(owri.projects$ReportDate[owri.projects$CompleteMo==0]))


rm(oregon.line.proj);rm(oregon.poly.proj);rm(oregon.owri.points)
rm(oregon.line.proj.points);rm(oregon.poly.proj.points)

temp = owri.projects %.% filter(drvdHUC4th!='11111111') %.%
  group_by(drvdHUC4th,CompleteYe,CompleteMo)%.%
  summarise(year.month.total = sum(TotalBoth))
temp = data.frame(temp)
temp = temp%>% mutate(proj.end.date = ymd(paste(CompleteYe,CompleteMo,'15')))
temp = temp %>% mutate(uq = paste(drvdHUC4th,proj.end.date))
temp = filter(temp,!is.na(temp$proj.end.date))


n.time = length(seq(mdy('1 15 1990'),length=(12*length(1990:2014)),by='month'))
n.huc8 = length(sort(unique(oregon.huc8$HUC8)))
owri.by.huc8 = data.frame(end.date = rep(seq(mdy('1 15 1990'),length=(12*length(1990:2014)),by='month'),
    each=n.huc8),huc8 = rep(sort(unique(oregon.huc8$HUC8)),n.time))
owri.by.huc8 = owri.by.huc8 %>% mutate(uq = paste(huc8,end.date))

owri.by.huc8$owri.yearmonth.spend = 0
owri.by.huc8$owri.yearmonth.spend[match(temp$uq,owri.by.huc8$uq)] = temp$year.month.total

huc8.projects.df = owri.by.huc8
huc8.projects.df$YEAR = year(huc8.projects.df$end.date)
huc8.projects.df$MONTH = month(huc8.projects.df$end.date)

rm(list=ls()[grep('huc8.projects',ls(),invert=TRUE)])



