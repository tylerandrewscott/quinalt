setwd('/homes/tscott1/win/user/quinalt/')

YEAR = data.frame(YEAR = seq(First.Year,Last.Year,1))
MONTH = data.frame(MONTH = month.name,MONTH.ABB = month.abb)
Year.Month = merge(YEAR,MONTH, type='full')

Year.Month$Month.Num = match(Year.Month$MONTH,month.name)
Year.Month$Year.Num = Year.Month$YEAR - First.Year + 1
Year.Month$Abs.Month = Year.Month$Month.Num  + (Year.Month$Year.Num-1) *12



proj.info = read.csv('Input/owri_project_info.csv')
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}
proj.info$TotalCash <- convertCurrency(proj.info$TotalCash)
proj.info$TotalInKind <- convertCurrency(proj.info$TotalInKind)
proj.info$TotalBoth <- proj.info$TotalCash + proj.info$TotalInKind

proj.info$Start.Abs = Year.Month$Abs.Month[match(paste(proj.info$StartMonth,proj.info$StartYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]
proj.info$Complete.Abs = Year.Month$Abs.Month[match(paste(proj.info$CompleteMonth,proj.info$CompleteYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]


proj.info$Project.Number = proj.info$drvdOwebNum
proj.info$HUC8 = proj.info$drvdHUC4thField
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

### proj.info = join(proj.info,all.points.uq.df,type='left')
proj.info$YEAR = proj.info$CompleteYear
proj.info$Month.Num = proj.info$CompleteMonth

proj.info = join(proj.info,Year.Month)
proj.info$uq.tid = paste(proj.info$HUC8,proj.info$Abs.Month,sep='_')

oregon.huc8.df$uq.tid = paste(oregon.huc8.df$HUC8,oregon.huc8.df$Abs.Month,sep='_')


proj.partners= read.csv('Input/owri_project_participants.csv')
public.list = c('county','city','state agency or jobs program (or ownership), state universities',
                'Soil & Water Conservation District',
                'federal agency or jobs program (or ownership)',
                'Extension Service (e.g. OSU Extension)')
wc.list = 'watershed council'
proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
                                ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))
temp  = (table(proj.partners$PROJNUM,proj.partners$Category))
t1 = data.frame((ifelse(temp[,3] > 0,'WC',ifelse(temp[,2]>0,'Public','Other'))))
colnames(t1) = 'Lead.Type'
t1$PROJNUM = rownames(t1)

t2 = data.frame(rowSums(temp))
colnames(t2) = 'Num.Partners'
t2$PROJNUM = rownames(t2)
t3 = join(t1,t2)

proj.info = join(proj.info,t3)


#binary true/false: OWEB project?
proj.info$OWEB.Grant = ifelse(proj.info$drvdOwebNum=='',FALSE,TRUE)


#proj.info$activity_t = tolower(proj.info$activity_t)
proj.info$drvdProjDesc = tolower(proj.info$drvdProjDesc)

###Create true/false for project about water quality

proj.info$about_wq = FALSE
proj.info$about_wq[
  unique(c(grep('restoration',proj.info$drvdProjDesc),
           grep('riparian',proj.info$drvdProjDesc),
           grep('wetland',proj.info$drvdProjDesc),
           grep('planting',proj.info$drvdProjDesc),
           grep('stabilization',proj.info$drvdProjDesc),
           grep('fencing',proj.info$drvdProjDesc),
           grep('livestock',proj.info$drvdProjDesc),
           grep('grazing',proj.info$drvdProjDesc),
           grep('stabilize',proj.info$drvdProjDesc),
           grep('manure',proj.info$drvdProjDesc),
           grep('effluent',proj.info$drvdProjDesc),
           grep('seed',proj.info$drvdProjDesc),
           grep('sediment',proj.info$drvdProjDesc),
           grep('silt',proj.info$drvdProjDesc),
           grep('sidecast',proj.info$drvdProjDesc),
           grep('setback',proj.info$drvdProjDesc),
           grep('irrigation',proj.info$drvdProjDesc),
           grep('erosion',proj.info$drvdProjDesc),
           grep('tailings',proj.info$drvdProjDesc),
           grep('nutrient',proj.info$drvdProjDesc),
           grep('water quality',proj.info$drvdProjDesc),
           grep('air diffuser',proj.info$drvdProjDesc),
           grep('waterbar',proj.info$drvdProjDesc)
  ))
  ] = TRUE



oweb.dat<-read.csv('Input/oweb_download_grant.csv')

oweb.dat$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(oweb.dat$Project.Summary,oweb.dat$X,sep=' '),
                                                                      oweb.dat$X.1,sep=''),oweb.dat$X.2,sep=''),oweb.dat$X.3, sep=''),oweb.dat$X.4, sep =''),
                                              oweb.dat$X.5, sep = ''), oweb.dat$X.6, sep = ''), oweb.dat$X.7, sep = ''),
                            oweb.dat$X.8, sep = ''), oweb.dat$X.9, sep = '')







head(oweb.dat[oweb.dat$Project.Status=='Open',])
levels(oweb.dat$Project.Status)
oweb.dat$Project.Start.Date[oweb.dat$Project.Statues=='Open']


dim(proj.info)
dim(oweb.dat)
proj.info$Project.Number %in% oweb.dat$Project.Number[oweb.dat$Project.Type=='Restoration']

table(oweb.dat$Project.Number[oweb.dat$Project.Type=='Restoration'] %in% proj.info$Project.Number)

test = oweb.dat[oweb.dat$Project.Number[oweb.dat$Project.Type=='Restoration'] %in% proj.info$drvdOwebNum==TRUE,-c(which(colnames(oweb.dat)=='Project.Summary'):ncol(oweb.dat))]

test = oweb.dat[oweb.dat$Project.Type=='Restoration',]
table(oweb.dat$Project.Status,oweb.dat$Grantee.Type)
table(test$Project.Status)
table(test$Grantee.Type)

table(test$Dominant.Activity)




oweb.grants = readOGR(dsn="SpatialData/OWEB_grants", layer="OWEB_Grants_8-4-2014")
oweb.grants@data$id = rownames(oweb.grants@data)
or.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
which.huc8 = over(oweb.grants,spTransform(or.huc8,CRS(proj4string(oweb.grants))))
oweb.grants@data = cbind(oweb.grants@data,which.huc8)

oweb.dat$HUC8 = oweb.grants@data$HUC8[match(oweb.dat$Project.Number,oweb.grants@data$applicatio)]


dim(oweb.grants@data)
dim(oweb.dat)

table(oweb.dat$Grantee.Type[is.na(oweb.dat$HUC8)])


oweb.dat$HUC8[is.na(oweb.dat$HUC8)] = 
  
sum(proj.info$drvdHUC4thField==11111111)
  proj.info$drvdHUC4thField[match(oweb.dat$Project.Number[is.na(oweb.dat$HUC8)],proj.info$drvdOwebNum)]

oweb.grants@data$applicatio[is.na(oweb.grants@data$HUC8)]

head(oweb.grants@data)

table(match(oweb.grants@data$applicatio,oweb.dat$Project.Number))


test = match(oweb.dat$Project.Number,oweb.grants@data$applicatio)


oweb.dat$locx = oweb.grants$point_x[match(oweb.dat$Project.Number,oweb.grants$applicatio)]
oweb.dat$locy = oweb.grants$point_y[match(oweb.dat$Project.Number,oweb.grants$applicatio)]





oweb.grants@data$id = rownames(oweb.grants@data)


table(oweb.dat$Project.Type[is.na(oweb.dat$locy)])
dim(oweb.dat)
sum(is.na(oweb.dat$locy))

oweb.dat$HUC8 = proj.info$drvdHUC4thField[match(oweb.dat$Project.Number,proj.info$drvdOwebNum)]


sum(is.na(match(oweb.dat$Project.Number[is.na(oweb.dat$HUC8)],oweb.grants@data$applicatio)))




table(is.na(oweb.dat$HUC8),oweb.dat$Project.Type)

test3 = cbind(test,test2)


test3[is.na(test3[,1])&is.na(test3[,2]),]
sum(is.na(test3[,1])&is.na(test3[,2]))



sum(!is.na(match(oweb.dat$Project.Number,proj.info$drvdOwebNum)))



  
  
oweb.dat$Project.Type[is.na(test)]
year(mdy(oweb.dat$Award.Date[oweb.dat$Project.Status=='Funded']))
table(is.na(mdy(oweb.dat$Project.Start.Date)),oweb.dat$Project.Status)
sum(is.na(mdy(oweb.dat$Project.End.Date)))
sum(is.na(mdy(oweb.dat$Award.Date)))
names(oweb.dat)
proj.info = read.csv('Input/owri_project_info.csv')
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}
proj.info$TotalCash <- convertCurrency(proj.info$TotalCash)
proj.info$TotalInKind <- convertCurrency(proj.info$TotalInKind)
proj.info$TotalBoth <- proj.info$TotalCash + proj.info$TotalInKind

proj.info$Start.Abs = Year.Month$Abs.Month[match(paste(proj.info$StartMonth,proj.info$StartYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]
proj.info$Complete.Abs = Year.Month$Abs.Month[match(paste(proj.info$CompleteMonth,proj.info$CompleteYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]


proj.info$Project.Number = proj.info$drvdOwebNum
proj.info$HUC8 = proj.info$drvdHUC4thField
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

### proj.info = join(proj.info,all.points.uq.df,type='left')
proj.info$YEAR = proj.info$CompleteYear
proj.info$Month.Num = proj.info$CompleteMonth

proj.info = join(proj.info,Year.Month)
proj.info$uq.tid = paste(proj.info$HUC8,proj.info$Abs.Month,sep='_')

oregon.huc8.df$uq.tid = paste(oregon.huc8.df$HUC8,oregon.huc8.df$Abs.Month,sep='_')


proj.partners= read.csv('Input/owri_project_participants.csv')
public.list = c('county','city','state agency or jobs program (or ownership), state universities',
                'Soil & Water Conservation District',
                'federal agency or jobs program (or ownership)',
                'Extension Service (e.g. OSU Extension)')
wc.list = 'watershed council'
proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
                                ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))
temp  = (table(proj.partners$PROJNUM,proj.partners$Category))
t1 = data.frame((ifelse(temp[,3] > 0,'WC',ifelse(temp[,2]>0,'Public','Other'))))
colnames(t1) = 'Lead.Type'
t1$PROJNUM = rownames(t1)

t2 = data.frame(rowSums(temp))
colnames(t2) = 'Num.Partners'
t2$PROJNUM = rownames(t2)
t3 = join(t1,t2)

proj.info = join(proj.info,t3)


#binary true/false: OWEB project?
proj.info$OWEB.Grant = ifelse(proj.info$drvdOwebNum=='',FALSE,TRUE)


#proj.info$activity_t = tolower(proj.info$activity_t)
proj.info$drvdProjDesc = tolower(proj.info$drvdProjDesc)

###Create true/false for project about water quality

proj.info$about_wq = FALSE
proj.info$about_wq[
  unique(c(grep('restoration',proj.info$drvdProjDesc),
           grep('riparian',proj.info$drvdProjDesc),
           grep('wetland',proj.info$drvdProjDesc),
           grep('planting',proj.info$drvdProjDesc),
           grep('stabilization',proj.info$drvdProjDesc),
           grep('fencing',proj.info$drvdProjDesc),
           grep('livestock',proj.info$drvdProjDesc),
           grep('grazing',proj.info$drvdProjDesc),
           grep('stabilize',proj.info$drvdProjDesc),
           grep('manure',proj.info$drvdProjDesc),
           grep('effluent',proj.info$drvdProjDesc),
           grep('seed',proj.info$drvdProjDesc),
           grep('sediment',proj.info$drvdProjDesc),
           grep('silt',proj.info$drvdProjDesc),
           grep('sidecast',proj.info$drvdProjDesc),
           grep('setback',proj.info$drvdProjDesc),
           grep('irrigation',proj.info$drvdProjDesc),
           grep('erosion',proj.info$drvdProjDesc),
           grep('tailings',proj.info$drvdProjDesc),
           grep('nutrient',proj.info$drvdProjDesc),
           grep('water quality',proj.info$drvdProjDesc),
           grep('air diffuser',proj.info$drvdProjDesc),
           grep('waterbar',proj.info$drvdProjDesc)
  ))
  ] = TRUE


temp.huc8 = oregon.huc8.df

temp = proj.info %>% dplyr::group_by(uq.tid) %>% dplyr::summarise_each(funs(sum),TotalCash,TotalInKind,TotalBoth)

temp.huc8 = join(temp.huc8,temp)

temp = proj.info %>% dplyr::group_by(uq.tid,Lead.Type,OWEB.Grant) %>% 
  dplyr::summarise_each(funs(sum),TotalCash,TotalInKind,TotalBoth)

tt = melt(temp,id.vars=c('uq.tid','Lead.Type','OWEB.Grant'))
tt$var.id = paste(tt$Lead.Type,tt$OWEB.Grant,tt$variable,sep='.')

temp.huc8[,unique(tt$var.id)] = NA

#place subset values into proper column
for (i in 1:ncol(temp.huc8))
{
  if (colnames(temp.huc8)[i] %in% unique(tt$var.id))
  {
    t1 = tt[tt$var.id == colnames(temp.huc8)[i],]
    temp.huc8[colnames(temp.huc8)[i]] = t1$value[match(temp.huc8$uq.tid,t1$uq.tid)]
    
  }
}


temp.huc8[,grep('Total',colnames(temp.huc8))][is.na(temp.huc8[,grep('Total',colnames(temp.huc8))])] = 0


huc8_data = temp.huc8



