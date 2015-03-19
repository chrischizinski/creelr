# This function displays will create a dataframe with a sequence of days
# from starting date to ending date

create_days_in_creel<-function(start_end.dates){
  days_in_creel<-data.frame(Date=seq(start_end.dates[1],start_end.dates[2],by="day"))
  days_in_creel$month<-as.numeric(format(days_in_creel$Date,"%m"))
  days_in_creel$day<-weekdays(days_in_creel$Date,abbreviate=T)
  days_in_creel$day_type<-ifelse(days_in_creel$day%in%c("Sat","Sun"),"weekend","weekday")
  return(days_in_creel)
}


########## Sets period start times ##################################
#Run before count.within.day.schedule()
# This function is not intended to be used on its own but within the 
# day.within.month.schedule()
#####################################################################

set.start.times<-function(month,period,params){
  start.times<-params$sampling.times[,grep("start.period",names(params$sampling.times))]
  start.times$end<-params$sampling.times$end
  start.times$month<-params$sampling.times$month
  start.times.month<-start.times[match(month,start.times$month),]
  timez<-data.frame(start=start.times.month[cbind(1:nrow(start.times.month),period)],
                    end=start.times.month[cbind(1:nrow(start.times.month),period+1)])
  return(timez)
}

########## Counts within a day #######################################
# This function randomly selects counts within a period given information
# in the parameter functions. 

# This function is not intended to be used on its own but within the 
# day.within.month.schedule()
######################################################################

count.within.day.schedule<-function(Date,start.time,end.time,params){
  counts=params$counts
  count.length=params$count.length
  seed=params$seed
  #count.length should be in minutes
  # Date should be in format Y-m-d i.e., "2014-04-01"
  # Time should be in 24-hr format H:M  i.e., "16:00"
  require(lubridate)

  start.time.2 <- strptime(ymd_hm(paste(Date,start.time)),format="%Y-%m-%d %H:%M:%S")
  end.time.2 <- strptime(ymd_hm(paste(Date,end.time)),format="%Y-%m-%d %H:%M:%S")
  
  min.periods<-((as.numeric(end.time.2-start.time.2)/counts)*60)
  time.breaks<-seq(start.time.2,end.time.2,by=paste(min.periods,"min"))
  count.times<-as.data.frame(matrix(NA,counts,1))
  names(count.times)<-"time"
  count.times$time<-as.POSIXct(count.times$time)

    for(i in 1:counts){
      avail.times<-seq(time.breaks[i],time.breaks[i+1],by=paste(count.length,"min"))
      count.times[i,1]<-avail.times[sample(1:length(avail.times),1)]
    }
  return(count.times)
}


########## Counts within a day #######################################
# This function randomly selects days within month, a randomized sampling period
#and then runs the count.within.day.schedule() to get randomly selected count times
# based on the in the parameters 
#
# holiday types has two levels.  weekend counts holidays as a weekend day,
# holiday special treats the days as a seperate strata and selects based
# on the parameter file
#
# add.hour adds an hour to the end time for each sampling period
######################################################################

day.within.month.schedule<-function(params,holiday.type="special",add.hour=TRUE){
  require(lubridate)
  set.seed(params$seed)
  start.date<-as.Date(params$start.date)
  end.date<-as.Date(params$end.date)
  datez<-data.frame(Date=seq(start.date,end.date,by="1 day"))
  datez$weekday<-wday(paste(datez$Date), label = TRUE, abbr = TRUE)
  datez$daytype<-ifelse(datez$weekday%in%c("Sat","Sun"),"weekends","weekdays")
  datez$month<-month(datez$Date)
  params$holidays<-as.Date(as.character(params$holidays))
  
  if(holiday.type=="weekend"){
    datez$daytype[as.character(datez$Date)%in%as.character(params$holidays)]<-"weekends"
  }
  
  if(holiday.type=="special"){
    datez<- datez[!as.character(datez$Date)%in%as.character(params$holidays),]
  }
  
  if(length(params$exclude.days)>0){
    datez<- datez[!as.character(datez$Date)%in%as.character(params$exclude.days),]
  }
  
  
  datez$n.days<-NA
  datez$n.days[datez$daytype=="weekends"]<-params$weekends
  datez$n.days[datez$daytype=="weekdays"]<-params$weekdays
  datez$row<-1:nrow(datez)
  
  dayspermonth<-data.frame(month=1:12, dayzpermonth=c(31,28,31,30,31,30,31,31,30,31,30,31))
  
  
  
  day_type.counts<-data.frame(table(datez$month,datez$daytype))
  names(day_type.counts)<-c("month","day_type","freq")
  
  if(any(day_type.counts$freq[day_type.counts$day_type=="weekdays"]<params$weekdays)){
    warning("The number of weekdays you want to sample are less than are available.  These months will be dropped")
    
    miss.id<-which(day_type.counts$freq[day_type.counts$day_type=="weekdays"]<params$weekdays)
    elim.month<-as.character(day_type.counts$month[day_type.counts$day_type=="weekdays"][miss.id])
    
    datez<-datez[!datez$month%in%elim.month,]
  }
  
  if(any(day_type.counts$freq[day_type.counts$day_type=="weekends"]<params$weekends)){
    warning("The number of weekends you want to sample are less than are available.  These months will be dropped")
    
    miss.id<-which(day_type.counts$freq[day_type.counts$day_type=="weekends"]<params$weekends)
    elim.month<-as.character(day_type.counts$month[day_type.counts$day_type=="weekends"][miss.id])
    
    datez<-datez[!datez$month%in%elim.month,]
  }
  
  #  Select the days per month
  ids<-sort(unlist(by(datez,datez[,c("month","daytype")],function(x) x$row[sample(1:nrow(x),x$n.days[1])])))
  sampled.days<-datez[ids,c("Date","weekday","daytype","month")]
  
  ## periods sampled
  rownames(sampled.days)<-NULL
  uniq.grp<-unique(paste(sampled.days$month,sampled.days$daytype))
  sampled.days$period<-NA
  
  for(i in 1:length(uniq.grp)){  # must include this in a loop to ensure that there  is a check of atleast 2 in each period
    OK=FALSE
    while(OK == FALSE){
      foo<-as.numeric(rownames(sampled.days[paste(sampled.days$month,sampled.days$daytype)==uniq.grp[i],]))
      periods.day<-sample(1:params$periods,size=length(foo),prob=params$period.probs,replace=TRUE)
      OK<-ifelse(all(as.data.frame(table(periods.day))$Freq>=2),TRUE,FALSE)
    }
    sampled.days$period[foo]<-periods.day
  }
  
  timez<-do.call(rbind.data.frame, by(sampled.days,sampled.days$Date, function(x) set.start.times(x$month,x$period,params)))
  timez$Date<-as.Date(rownames(timez))
  sampled.days.times<-merge(sampled.days,timez,by="Date",all=TRUE)
  count.times<-do.call(rbind.data.frame, by(sampled.days.times,sampled.days$Date, function(x) 
    format(count.within.day.schedule(x$Date,x$start,x$end,params),"%H:%M:%S")))
  names(count.times)<-paste("count",1:ncol(count.times),sep=".")
  count.times$Date<-as.Date(rownames(count.times))
  full.count<-merge(sampled.days.times,count.times,by="Date",all=TRUE)
  
  if(holiday.type=="special"){
    datez<-data.frame(Date=as.Date(params$holidays),holiday.grp=params$holiday.grp)
    datez$row<-1:nrow(datez)
    datez$month<-month(datez$Date)
    datez$daytype<-"holiday"
    datez$weekday<-wday(paste(datez$Date), label = TRUE, abbr = TRUE)
    sampled.days<-datez[sort(unlist(by(datez,datez[,c("holiday.grp")],function(x) x$row[sample(1:nrow(x),params$holiday.days)]))),]
    uniq.grp<-unique(params$holiday.grp)
    sampled.days$period<-NA
    
    for(i in 1:length(uniq.grp)){  # must include this in a loop to ensure that there  is a check of atleast 2 in each period
      OK=FALSE
      while(OK == FALSE){
        foo<-which(sampled.days$holiday.grp==uniq.grp[i])
        periods.day<-sample(1:params$periods,size=length(foo),prob=params$period.probs,replace=TRUE)
        OK<-ifelse(all(c(1:params$periods)%in%as.data.frame(table(periods.day))$periods.day),TRUE,FALSE)
      }
      sampled.days$period[foo]<-periods.day
    }
    timez<-do.call(rbind.data.frame, by(sampled.days,sampled.days$Date, function(x) set.start.times(x$month,x$period,params)))
    timez$Date<-as.Date(rownames(timez))
    
    sampled.days.times<-merge(sampled.days,timez,by="Date",all=TRUE)
    
    count.times<-do.call(rbind.data.frame, by(sampled.days.times,sampled.days$Date, function(x) 
      format(count.within.day.schedule(x$Date,x$start,x$end,params),"%H:%M:%S")))
    names(count.times)<-paste("count",1:ncol(count.times),sep=".")
    count.times$Date<-as.Date(rownames(count.times))
    
    holiday.days.counts<-merge(sampled.days.times,count.times,by="Date",all=TRUE)
    
    holiday.days.counts<-holiday.days.counts[,-which(names(holiday.days.counts)%in%c("holiday.grp", "row"))]
    
    full.count<-rbind(full.count,holiday.days.counts)
  }  
  
  if(add.hour==TRUE){
    new.time<-format(strptime(ymd_hm(paste(full.count$Date,full.count$end)),format="%Y-%m-%d %H:%M:%S")+3600,format="%H:%M")
    full.count$end<-as.character(full.count$end)
    full.count$end<-new.time
  }
  return(full.count)
} 


########## Counts within a day #######################################
# This function takes the schedule developed from day.within.month.schedule()
# and reformats it so that it can be saved as a csv and uploaded into google calendar

######################################################################

reformat.schedule<-function(schedule,lake){
## this function reformats a creel schedule to be uploaded to a google calendar
  google.calendar<-data.frame('Start Date' = schedule$Date,"End Date" = schedule$Date,
                              "Start Time" = schedule$start,"End Time" = schedule$end,
                              "Subject" =paste("Counts:  ",schedule$count.1,schedule$count.2),Location=lake)
  return(google.calendar)
}

day.within.month.schedule.2<-function(params,holiday.type="special",add.hour=TRUE){
  require(lubridate)
  require(dplyr)
  set.seed(params$seed)
  datez<-create_days_in_creel(c(as.Date(params$start.date),as.Date(params$end.date)))
  datez$special.grp<-0
  
  day_type.counts<-data.frame(table(datez$month,datez$day_type))
  names(day_type.counts)<-c("month","day_type","freq")
    specialdates<-data.frame(Date=params$holidays,special.grp=params$holiday.grp)
  
  num.per.month<-expand.grid(day_type=params$day.types,period=1:params$periods,section=1:params$lake.sections)
  num.per.month$days.per.month<-params$days.period
  num.per.month<-num.per.month[order(num.per.month$day_type,num.per.month$period),]
  
  if(holiday.type=="weekend"){
    datez$day.type[as.character(datez$Date)%in%as.character(params$holidays)]<-"weekends"
  }
  
  if(holiday.type=="special"){
    specialdates<-specialdates[as.character(specialdates$Date) %in% as.character(datez$Date),]
    datez$day_type[as.character(datez$Date)%in%as.character(params$holidays)]<-"highuse"
    datez$special.grp[match(as.character(specialdates$Date),as.character(datez$Date))]<-specialdates$special.grp
  }
  
  ###  
  days_exist<-datez %>%
              group_by(month,day_type) %>%
              summarise(N=n())

  days_needed<-data.frame(day_type=c("weekday","weekend","highuse"),
                          needed=params$periods*c(params$weekdays,params$weekends,params$specialdays),
                          days.per.period=c(params$weekdays,params$weekends,params$specialdays))
    
  days_exist<-left_join(days_exist,days_needed,by="day_type")
  
  days_exist$problem<-ifelse(days_exist$needed<=days_exist$N,0,1)
  days_exist$to.sample<-NA
  days_exist$to.sample[days_exist$problem==0]<-days_exist$days.per.period[days_exist$problem==0]
  days_exist$to.sample[days_exist$problem==1]<-floor(days_exist$N[days_exist$problem==1]/params$periods)

  if(any(days_exist$problem==1)) warning("The number of days requested are less than exist. Going with what exists")
  days_exist$day_type<-as.character(days_exist$day_type)
  num.per.month$day_type<-as.character(num.per.month$day_type)
  num.per.month<-full_join(num.per.month,days_exist[,c("month","day_type","to.sample")],by="day_type")


  datez.nohighuse<-datez[datez$day_type!="highuse",]
  datez.nohighuse<- left_join(datez.nohighuse,num.per.month,by=c("month","day_type"))
  datez.nohighuse$row<-1:nrow(datez.nohighuse)
  
  uniq.periods<-unique(datez.nohighuse$period)
  sampled.days.id<-NULL
  available.Dates<-unique(datez.nohighuse$Date)
  
  
 
  
  for(i in 1:length(uniq.periods)){
    foo.periods<-datez.nohighuse[datez.nohighuse$period==uniq.periods[i],]
    foo.periods<-foo.periods[foo.periods$Date%in%available.Dates,]
    

    daycounts<-data.frame(table(foo.periods$month,foo.periods$day_type))
    
    sum(num.per.month$days.per.month[num.per.month$day_type=="weekend"])
    
    
    ids<-sort(unlist(by(foo.periods,foo.periods[,c("month","day_type")],function(x) x$row[sample(1:nrow(x),size=x$to.sample[1])])))
    
    sampled.days.id<-c(sampled.days.id,ids)
    remove.Dates<-available.Dates[available.Dates%in%foo.periods$Date[foo.periods$row %in%ids]]
    available.Dates<-available.Dates[!available.Dates%in%remove.Dates]
  }
  
  sampled.days<-datez.nohighuse[datez.nohighuse$row%in%sampled.days.id,c("Date","month","day","day_type","section","period")]
  
  if(any(days_exist$day_type=="highuse")){
    
    datez.highuse<-datez[datez$day_type=="highuse",]
    datez.highuse<-left_join(datez.highuse,num.per.month,by=c("month","day_type"))
    datez.highuse$row<-1:nrow(datez.highuse)
    
    uniq.periods<-unique(datez.highuse$period)
    sampled.days.id.hu<-NULL
    available.Dates<-unique(datez.highuse$Date)
    datez.highuse$special.grp<-factor(datez.highuse$special.grp)
    
    for(i in 1:length(uniq.periods)){
      foo.periods<-datez.highuse[datez.highuse$period==uniq.periods[i],]
      foo.periods<-foo.periods[foo.periods$Date%in%available.Dates,]
      hu.ids<-sort(unlist(by(foo.periods,foo.periods[,c("special.grp")],function(x) x$row[sample(1:nrow(x),size=x$to.sample[1])])))
      
      sampled.days.id.hu<-c(sampled.days.id.hu,hu.ids)
      remove.Dates<-available.Dates[available.Dates%in%foo.periods$Date[foo.periods$row %in%hu.ids]]
      available.Dates<-available.Dates[!available.Dates%in%remove.Dates]
    }
    
    sampled.days.hu<-datez.highuse[datez.highuse$row%in%sampled.days.id.hu,c("Date","month","day","day_type","section","period")]
    
    sampled.days<-rbind(sampled.days,sampled.days.hu)
    
  }
  
  sampled.days<-sampled.days[order(sampled.days$Date),]
  rownames(sampled.days)<-NULL
  
  timez<-do.call(rbind.data.frame, by(sampled.days,sampled.days$Date, function(x) set.start.times(x$month,x$period,params)))
  timez$Date<-as.Date(rownames(timez))
  sampled.days.times<-merge(sampled.days,timez,by="Date",all=TRUE)
  count.times<-do.call(rbind.data.frame, by(sampled.days.times,sampled.days$Date, function(x) 
    format(count.within.day.schedule(x$Date,x$start,x$end,params),"%H:%M:%S")))
  names(count.times)<-paste("count",1:ncol(count.times),sep=".")
  count.times$Date<-as.Date(rownames(count.times))
  full.count<-merge(sampled.days.times,count.times,by="Date",all=TRUE)
  
  return(full.count)
  
}