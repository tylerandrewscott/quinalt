
setwd('/homes/tscott1/win/user/quinalt/')

library(sp)
library(rgdal)
library(plyr)
library(dplyr)


install.packages(c("devtools", "rJython", "rJava", "rjson"))
library(devtools)
library(rJava)
library(rJython)

install_github("trinker/gmailR")
install_github("kbroman/mygmailR")




########### ADD OWEB GRANTS ############
oweb.summary<-read.csv('Input/oweb_download_grant.csv')
oweb.summary$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(oweb.summary$Project.Summary,oweb.summary$X,sep=' '),
                                                                           oweb.summary$X.1,sep=''),oweb.summary$X.2,sep=''),oweb.summary$X.3, sep=''),oweb.summary$X.4, sep =''),
                                                   oweb.summary$X.5, sep = ''), oweb.summary$X.6, sep = ''), oweb.summary$X.7, sep = ''),
                                 oweb.summary$X.8, sep = ''), oweb.summary$X.9, sep = '')
oweb.summary = oweb.summary[,-c(grep('X',names(oweb.summary)))]



oweb.grants = readOGR(dsn="SpatialData/OWEB_grants", layer="OWEB_Grants_8-4-2014")
oweb.grants@data$id = rownames(oweb.grants@data)
or.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
which.huc8 = over(oweb.grants,spTransform(or.huc8,CRS(proj4string(oweb.grants))))
oweb.grants@data = cbind(oweb.grants@data,which.huc8)

oweb.grants@data = oweb.grants@data %>% dplyr::rename(Project.ID = gr_project,Project.Number = applicatio)
grant.gis = oweb.grants@data

oweb.cancelled = read.csv('Input/oweb_cancelled.csv')
oweb.notawarded = read.csv('Input/oweb_notawarded.csv')
oweb.withdrawn =  read.csv('Input/oweb_withdrawn.csv')
oweb.monitoring = read.csv('Input/oweb_monitoring.csv')
oweb.completed = read.csv('Input/oweb_completed.csv')
oweb.funded =  read.csv('Input/oweb_funded.csv')
oweb.pending = read.csv('Input/oweb_pending.csv')
oweb.open = read.csv('Input/oweb_open.csv')
oweb.ineligible = read.csv('Input/oweb_ineligible.csv')


oweb.all = join_all(list(oweb.completed,oweb.cancelled,oweb.notawarded,oweb.monitoring,oweb.funded,
              oweb.open,oweb.ineligible,oweb.withdrawn,oweb.pending),type='full')

oweb.all = join(oweb.all,grant.gis)


not.mapped = read.csv('Input/Grants_not_Mapped_August_2014.csv')

not.mapped = not.mapped %>% dplyr::rename(Project.Number = APPLICATION.NUMBER,
                                          Project.ID = GRANT.PROJECT.ID,
                      Project.Type = Grant.Type)

oweb.all[oweb.all==''] = NA

oweb.all = oweb.all[!is.na(oweb.all$Project.End.Date),]

oweb.all = oweb.all %>% filter(Project.Status != 'Cancelled') 

oweb.all$End.Date = as.Date(oweb.all$Project.End.Date, format = "%m/%d/%y")
library(lubridate)
oweb.all = oweb.all[year(oweb.all$End.Date)<=2013,]

oweb.all$MONTH = month(oweb.all$End.Date)
oweb.all$YEAR = (year(oweb.all$End.Date))
oweb.all$Abs.Month = Year.Month$Abs.Month[match(paste(oweb.all$YEAR,oweb.all$MONTH),paste(Year.Month$YEAR,Year.Month$Month.Num))]

oweb.all$uq.tid= paste(oweb.all$HUC8,oweb.all$Abs.Month,sep='_')

oweb.all = filter(oweb.all,!is.na(oweb.all$HUC8)
                  
oweb.restoration = oweb.all[oweb.all$Project.Type=='Restoration',]

public.values = c('City','Federal Agency','State Agency','County','Special District','Tribe')

oweb.restoration$which.group = ifelse(oweb.restoration$Grantee.Type %in% public.values ,'Public',
       ifelse(oweb.restoration$Grantee.Type =='Watershed Council','WC','Other'))


# nm = is.mapped[is.na(test$HUC8),]
# mapped  = is.mapped[!is.na(test$HUC8),]
# nm$HUC8 = mapped$HUC8[match(nm$Grantee,mapped$Grantee)]

# counties = readOGR(dsn="SpatialData/government_units", layer="county_nrcs_a_or")
# counties@data$id = rownames(counties@data)
# counties@data$center_long = coordinates(counties)[,1]
# counties@data$center_lat = coordinates(counties)[,2]
# or.county = SpatialPointsDataFrame(coords = coordinates(counties),data = counties@data,proj4string = CRS(proj4string(counties)))
# which.place = over(or.county,spTransform(oregon.huc8,CRS=CRS(proj4string(counties))))
# or.county@data = cbind(or.county@data,which.place)

levels(oweb.all$Project.Type) = c("Acquisition" ,'Monitoring/Assessment/Tech. Assistance',  "Council Support" , "Data Development"  ,   'Education/Outreach',           
                                  "Holding"     ,         "Law Enforcement"  ,    'Monitoring/Assessment/Tech. Assistance'    , 'Education/Outreach', 
                                  "OWEB Administration" ,
                                  "Research"    , "Restoration"   ,       "SWCD"     ,  'Monitoring/Assessment/Tech. Assistance','')

temp =  oweb.all %>% dplyr::group_by(uq.tid,Project.Type) %>% dplyr::summarise_each(funs(sum),Project.Amount)


huc8_data[,unique(paste('OWEB',oweb.all$Project.Type,sep='_'))] = NA

for (i in 1:nrow(temp))
{
huc8_data[match(temp$uq.tid[i],huc8_data$uq.tid),which(colnames(huc8_data)==paste('OWEB',temp$Project.Type[i],sep='_'))] = 
  temp$Project.Amount[i]
}






[1]