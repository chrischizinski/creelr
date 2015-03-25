
'creel_error_check'<-function(surveydata,countdata){
  error<-0
  ## Surveys checked first
  if(any(is.na(surveydata$Date))){ # Check dates
   date_error.survey<-surveydata$int.name[which(is.na(surveydata$Date)==TRUE)]
   error<-error+1
  }else{date_error.survey<-"none"}
  
  if(any(is.na(surveydata$start.time))){ # Check start times
   starttime_error.survey<-surveydata$int.name[which(is.na(surveydata$start.time)==TRUE)]
   error<-error+1
  }else{starttime_error.survey<-"none"} 
  
  if(any(is.na(surveydata$interv.time))){ # Check interv times
   intertime_error.survey<-surveydata$int.name[which(is.na(surveydata$interv.time)==TRUE)]
   error<-error+1
  }else{intertime_error.survey<-"none"}
  

  if(any(is.na(surveydata$time.diff_hr)| surveydata$time.diff_hr> 24)){ # Check time difference
   timediff_error.survey<-surveydata$int.name[which(is.na(surveydata$time.diff_hr)==TRUE)]
   error<-error+1
  }else{timediff_error.survey<-"none"}
  
  
  if(any(is.na(surveydata$trip.type))){ # Check trip type
   triptype_error.survey<-surveydata$int.name[which(is.na(surveydata$trip.type)==TRUE)]
   error<-error+1
  }else{triptype_error.survey<-"none"}
  
  if(any(is.na(surveydata$sp.sought))){ # Check spp.sought
   spp.sought_error.survey<-surveydata$int.name[which(is.na(surveydata$sp.sought)==TRUE)]
   error<-error+1
  }else{spp.sought_error.survey<-"none"}
  
   if(any(is.na(surveydata$state))){ # Check state
   state_error.survey<-surveydata$int.name[which(is.na(surveydata$state)==TRUE)]
   error<-error+1
  }else{state_error.survey<-"none"}
  
  if(any(is.na(surveydata$county))){ # Check county
   county_error.survey<-surveydata$int.name[which(is.na(surveydata$county)==TRUE)]
   error<-error+1
  }else{county_error.survey<-"none"}
  
  if(any(is.na(surveydata$day_type))){ # Check daytype
   daytype_error.survey<-surveydata$int.name[which(is.na(surveydata$day_type)==TRUE)]
   error<-error+1
  }else{daytype_error.survey<-"none"}
  
  if(any(is.na(surveydata$time.period))){ # Check daytype
   timeperiod_error.survey<-surveydata$int.name[which(is.na(surveydata$time.period)==TRUE)]
   error<-error+1
  }else{timeperiod_error.survey<-"none"}

  ## Then count data

  if(any(is.na(countdata$month))|any(countdata$month>12)){ # Check dates
   month_error.count<-countdata$time.1[which(is.na(countdata$month)|any(countdata$month>12)==TRUE)]
   error<-error+1
  }else{month_error.count<-"none"}
  
  if(any(is.na(countdata$day))|any(countdata$day>31)){ # Check dates
   day_error.count<-countdata$time.1[which(is.na(countdata$month)|any(countdata$month>31)==TRUE)]
   error<-error+1
  }else{day_error.count<-"none"}
  
  if(any(is.na(countdata$year))){ # Check dates
   year_error.count<-countdata$time.1[which(is.na(countdata$year)==TRUE)]
   error<-error+1
  }else{year_error.count<-"none"}
  
  if(any(is.na(countdata$Date))){ # Check dates
   Date_error.count<-countdata$time.1[which(is.na(countdata$Date)==TRUE)]
   error<-error+1
  }else{Date_error.count<-"none"}
  
  if(any(is.na(countdata$Month))){ # Check dates
   Month_error.count<-countdata$time.1[which(is.na(countdata$Month)==TRUE)]
   error<-error+1
  }else{Month_error.count<-"none"}
  
  if(any(is.na(countdata$day_type))){ # Check dates
   daytype_error.count<-countdata$time.1[which(is.na(countdata$day_type)==TRUE)]
   error<-error+1
  }else{daytype_error.count<-"none"}
  
  if(any(is.na(countdata$time.period))){ # Check dates
   timeperiod_error.count<-countdata$time.1[which(is.na(countdata$time.period)==TRUE)]
   error<-error+1
  }else{timeperiod_error.count<-"none"}
  
  if(any(!unique(surveydata$Date)%in%unique(countdata$Date))){
  missing.interv<-paste(unique(surveydata$Date[which(!surveydata$Date%in%unique(countdata$Date))]), " is in Survey data but not Count data", sep="")
  error<-error+1
  }else{missing.interv<-"none"}
  
  if(any(!unique(countdata$Date)%in%unique(surveydata$Date))){
  missing.count<-paste(unique(countdata$Date[which(!countdata$Date%in%unique(surveydata$Date))]), " is in Count data but not Survey data", sep="")
  #error<-error+1
  }else{missing.count<-"none"}
  
  error_list<-list(NumErrors=error,
				date_error.survey=date_error.survey,
				starttime_error.survey=starttime_error.survey,
				intertime_error.survey=starttime_error.survey,
				timediff_error.surveyA=timediff_error.survey,
				triptype_error.survey=triptype_error.survey,
				spp.sought_error.survey=spp.sought_error.survey,
				state_error.survey=state_error.survey,
				state_error.survey=state_error.survey,
				daytype_error.survey=daytype_error.survey,
				timeperiod_error.survey=timeperiod_error.survey,
				month_error.count=month_error.count,
				day_error.count=day_error.count,
				year_error.count=year_error.count,
				Date_error.count=Date_error.count,
				Month_error.count=Month_error.count,
				daytype_error.count=daytype_error.count,
				timeperiod_error.count=timeperiod_error.count,
				missing.interv=missing.interv,
				missing.count=missing.count)
				
  return(error_list)
}
