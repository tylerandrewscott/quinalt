setwd('//Users/TScott/Google Drive/quinalt')
list.files()


dat<-read.csv('oweb_download_grant.csv')

dat$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(dat$Project.Summary,dat$X,sep=' '),
                  dat$X.1,sep=''),dat$X.2,sep=''),dat$X.3, sep=''),dat$X.4, sep =''),
                          dat$X.5, sep = ''), dat$X.6, sep = ''), dat$X.7, sep = ''),
                              dat$X.8, sep = ''), dat$X.9, sep = '')

levels(dat$Region) <- c('NW','SW','WIL','CENT','EAST','MID','SW')

dat$Award.Date<-as.Date(dat$Award.Date,format = '%m/%d/%y')
dat$Project.Start.Date<-as.Date(dat$Project.Start.Date,format = '%m/%d/%y')
dat$Project.End.Date<-as.Date(dat$Project.End.Date,format = '%m/%d/%y')


#Acquisition
#Council Support
#Technical Assistance
#Outreach
#Assessment


dat$Project.Summary[dat$Project.Type=='Assessment']



cs$Proj.Paste

dat$Project.Summary[dat$Project.Type=='Technical Assistance']






names(dat)
table(dat$Project.Type)

head(dat[dat$Project.Type=='Acquisition',])

difftime(dat$Award.Date,dat$Project.Start.Date)
unique(dat$Grantee)
dat[dat$Grantee = '']
levels(dat$Project.Type)
names(dat)
dat$Proj.Paste

as.numeric(dat$Project.Start.Date)

dat$Award.Date[1]-dat$Award.Date[2]
dat$Award.Date
dat$Project.Start.Date-dat$Award.Date

dat$Award.Date
dat$Award.Date
dat$Award.Date-
dat$Project.Start.Date
dat$Project.End.Date



names(dat)
dat$Starting.Date

levels(dat$Region)

?date
dat$Region<-ifelse(dat$Region==2,'SW',dat$Region)
dat$Region<-ifelse(dat$Region==1,'NW',dat$Region)
dat$Region<-ifelse(dat$Region==3,'WIL',dat$Region)
dat$Region<-ifelse(dat$Region==4,'CENT',dat$Region)
dat$Region<-ifelse(dat$Region==5,'EAST',dat$Region)
dat$Region<-ifelse(dat$Region==6,'MID',dat$Region)




as.factor(dat$Region)
dat$Region
names(dat)
unique(dat$Region)
unique(dat$Basin)
names(dat)
dat$Basin
unique(dat$Proj.Paste)

dat$Proj.Paste
unique(dat$X.1)
unique(dat$X.2)
unique(dat$X.3)
unique(dat$X.4)


unique(dat$Project.Type)


table(dat$Project.Type, dat$Monitoring.Required)
table(dat$Project.Type)

dat$Award.Date
dat$Project.Start.Date
dat$Project.End.Date
dat$Grantee

dat[dat$Grantee.Type=='Small Grant Team',]

dat$Grantee.Type=='Watershed Council'

unique(dat$Grantee.Type)


source('model_prep.R')



