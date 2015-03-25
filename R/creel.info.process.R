
"creel.info.process"<-function(infofiles){
  connect<-file(paste(infofiles),'r')  #connect to the data file
  foo<-readLines(connect, n=-1) #read the data as char. strings toa data.set
  close(connect)
  foo<-strsplit(foo,"\t")
  
  
  lake.code<<-foo[[2]][1]
  lake.name<<-foo[[3]][1]
  lake.size<<-foo[[4]][1]
  start.date<-paste(foo[[5]][1])
  end.date<-paste(foo[[6]][1])
  
  start.date<<-as.Date(start.date,"%m/%d/%y")
  end.date<<-as.Date(end.date,"%m/%d/%y")
  
  start_endDates<<-c(start.date,end.date)
  holidays<<- as.Date(c(as.Date(paste(foo[[16]][1]),"%m/%d/%y"),as.Date(paste(foo[[17]][1]),"%m/%d/%y"),as.Date(paste(foo[[18]][1]),"%m/%d/%y")))
  
  time.cut.off<<-format(strptime('14:00',"%H:%M"),"%H:%M")
  lod.dat<<-data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       LOD_hrs=c(9,10,12,13,14,15,15,14,13,11,10,9))  #approx length of day at Kearney NE, should not change
  
  num.time.periods<<-as.numeric(foo[[7]][1])
  num.lake.sections<<-ifelse(foo[[12]][1]=="N",1,NA)
  period_1.prob<<-as.numeric(foo[[8]][1])
  period_2.prob<<-as.numeric(foo[[9]][1])
  count_day<<-as.numeric(foo[[10]][1])
  count_length<<-as.numeric(foo[[11]][1]) #Minutes
  weekdays_month<<-as.numeric(foo[[13]][1])
  weekends_month<<-as.numeric(foo[[14]][1])
  hours_period<<-6.5
  
}

"creel.calculate"<-function(count.data,survey.data){
  count.data$Date<-as.Date(count.data$Date)
  count.data<-count.data[order(count.data$Date),-1]
  count.data$weekday<-weekdays(count.data$Date,abbreviate=TRUE)
  count.data$Month<-months(count.data$Date,abbreviate=TRUE)
  count.data$day_type<-ifelse(count.data$weekday%in%c('Sat','Sun'),'weekend','weekday')
  count.data$day_type[count.data$Date%in%holidays]<-'weekend'
  count.data$time.period<-ifelse(format(strptime(paste(count.data$time.1),"%Y-%m-%d %H:%M"),"%H:%M")<time.cut.off,1,2)
  count.data<-data.frame(site=paste(lake.code),count.data)
  
  angler_effort<-daily.angler.effort(count=count.data,LOD.info=lod.dat)
  
  survey.data$Date<-as.Date(survey.data$Date)
  survey.data$Month<-months(survey.data$Date,abbreviate=TRUE)
  survey.data$weekday<-weekdays(survey.data$Date,abbreviate=TRUE)
  survey.data$day_type<-ifelse(survey.data$weekday%in%c('Sat','Sun'),'weekend','weekday')
  survey.data$day_type[survey.data$Date%in%holidays]<-'weekend'
  survey.data$time.period<-ifelse(format(strptime(paste(survey.data$interv.time),"%H:%M"),"%H:%M")<time.cut.off,1,2)
  
  survey.data<-survey.data[order(survey.data$Date),]
  survey.data$Month<-factor(survey.data$Month,levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  ###  Calculates the angler effort and total harvest/catch/release per party
  party.catches<-survey.party.catch(surveydata=survey.data)
  creel.effort<-monthly.effort(surveyharvest=party.catches,surveydata=survey.data,angler_effort=angler_effort,
                               start_endDates=start_endDates)
  out<-list(count.data=count.data,survey.data=survey.data,creel.effort=creel.effort)
  return(out)
}
