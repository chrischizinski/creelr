
'survey.day.summary'<-function(year,countdata,holidayz){
  surveyd.days<-countdata$Date
  surveyd.months<-as.numeric(format(surveyd.days,"%m"))
  surveyd.day_type<-countdata$day_type
  surveyd.time_period<-countdata$time.period
    
  LOD<-as.data.frame(matrix(c(1,830,1730,540,31,2,800,1800,600,28,3,700,1900,720,31,4,700,2000,780,30,
  5,630,2030,840,31,6,600,2100,900,30,7,600,2100,900,31,8,630,2030,840,31,
  9,700,2000,780,30,10,800,1900,660,31,11,730,1730,600,30,12,800,1700,540,31),nrow=12,byrow=TRUE))
  
  names(LOD)<-c('Month','Sunrise','Sunset','LOD_m','N_days')    #<-info from B. Newcomb's SAS code that is based out of Kearney, NE.  
  LOD$LOD_h<-LOD$LOD_m/60   # Convert LOD in minutes to hours

  #automatically calculate the number of weekedays and weekend/holiday days per calendar year

  x = seq(as.Date(paste(year,"-01-01",sep="")), by = "1 day", length.out=365)
  calendar<-as.data.frame(cbind(paste(x),format(x,format='%b'),format(x,format='%A')))
  names(calendar)<-c("Date","Month","Day")
  calendar$Date<-as.Date(calendar$Date)
  calendar$Month<-factor(calendar$Month,levels=c("Jan","Feb","Mar","Apr",
                        "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  weekdayz<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
  calendar$Day_type<-ifelse(calendar$Day %in% weekdayz & !calendar$Date %in% holidayz,'Weekday','Weekend')
  num_weekdays<-as.data.frame(with(calendar,table(Month,Day_type)))
  days_in_period<-with(LOD,sum(N_days[min(surveyd.months):max(surveyd.months)]))
  ttl_surveyed<-length(unique(surveyd.days))  
  uniq.ind<-!duplicated(surveyd.days)
  num_weekdays.rev<-num_weekdays[num_weekdays$Month%in%unique(format(surveyd.days,format='%b')),]
  weekdays_in_period<-sum(num_weekdays.rev$Freq[num_weekdays.rev$Day_type=='Weekday'])
  weekends_in_period<-sum(num_weekdays.rev$Freq[num_weekdays.rev$Day_type=='Weekend'])
  week_type_surveyed<-as.matrix(table(surveyd.day_type[uniq.ind])) 
  weekdays_surveyed<-week_type_surveyed[rownames(week_type_surveyed)=='weekday']
  weekends_surveyed<-week_type_surveyed[rownames(week_type_surveyed)=='weekend']
  tab<-ftable(xtabs(~surveyd.day_type+surveyd.time_period+surveyd.months))
 
  out<-list(days_in_period=days_in_period,ttl_surveyed=ttl_surveyed,weekdays_in_period=weekdays_in_period,
  weekends_in_period=weekends_in_period,weekdays_surveyed=weekdays_surveyed,weekends_surveyed=weekends_surveyed,
  daytype_period_sampled=tab)

  return(out)
}

