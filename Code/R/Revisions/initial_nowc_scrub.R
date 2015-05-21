
surv = read.csv('https://github.com/tylerascott/quinalt/raw/master/Input/Winter-2012-Council-Survey_Scott.csv')
surv[surv==''] = NA

wc.data = data.frame(NAME = surv$Custom.Data)
wc.data$RECOGNIZED = surv$Is.your.council.officially.designated.or.recognized.by.a.local.government.=='Yes'
wc.data$NONPROFIT = surv$Is.your.council.a.designated.501.c..3..non.profit.organization.=='Yes'
wc.data$CITY = surv$X.80
wc.data$ZIPCODE = surv$X.82

te.foun = melt(surv[,c(1,87:109)],id.vars='Custom.Data')
te.foun = te.foun[!is.na(te.foun$value),]
colnames(te.foun) = c('NAME','Question','YEAR.FOUNDED')
wc.data = join(wc.data,te.foun[,-2])

te.bud = melt(surv[,c(1,118:124)],id.vars='Custom.Data')
te.bud = te.bud[!is.na(te.bud$value),]
colnames(te.bud) = c('NAME','Question','OPERATING.BUDGET')
wc.data = join(wc.data,te.bud[,-2])

te.bud = melt(surv[,c(1,127:133)],id.vars='Custom.Data')
te.bud = te.bud[!is.na(te.bud$value),]
colnames(te.bud) = c('NAME','Question','TOTAL.BUDGET')
wc.data = join(wc.data,te.bud[,-2])


te.coord = melt(surv[,c(1,134:136)],id.vars='Custom.Data')
te.coord = te.coord[!is.na(te.coord$value),]
colnames(te.coord) = c('NAME','Question','COORD.TYPE')
wc.data = join(wc.data,te.coord[,-2])

te.coord.fte = melt(surv[,c(1,134:136)],id.vars='Custom.Data')
te.coord.fte = te.coord.fte[!is.na(te.coord.fte$value),]
colnames(te.coord.fte) = c('NAME','Question','COORD.FTE')
wc.data = join(wc.data,te.coord.fte[,-2])

te.staff.fte = melt(surv[,c(1,156:166)],id.vars='Custom.Data')
te.staff.fte = te.staff.fte[!is.na(te.staff.fte$value),]
colnames(te.staff.fte) = c('NAME','Question','STAFF.FTE')
wc.data = join(wc.data,te.staff.fte[,-2])

wc.data = cbind(wc.data,!is.na(surv[,c(211:216)]))
colnames(wc.data)[13:ncol(wc.data)] = c(paste(c('OWEB','Federal','Foundation','Donors','Membership','Other'),'Support',sep='.'))

wc.data$VOLUNTEERS = surv$Approximately.how.many.non.board.volunteers.have.you.engaged.over.the.past.year.

wc.data$BOARD.SIZE = surv$How.many.members.serve.on.your.board.

#write.csv(oregon.wc@data,'Input/watershed_councils.csv')
write.csv(wc.data,'Input/watershed_councils_tempfile.csv')
