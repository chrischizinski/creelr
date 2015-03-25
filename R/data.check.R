data.check<-function(count.data,survey.data,dup.dates=FALSE,error.count=1,error.survey=1){
  c.err<-0
  
  c.dates<-as.character(sort(count.data$Date))
  
  s.dates<-as.character(sort(survey.data$Date))
  s.dates<-s.dates[!duplicated(s.dates)]
  
  if(error.count==1){  #Turn on errors for counts
  ############### Counts ###############################
  
    
    if(dup.dates==FALSE){
      
      if(any(duplicated(c.dates))){
        print(paste("Duplicate count.dates on ", paste(c.dates[which(duplicated(c.dates))],collapse=", ")))
        c.err<-1
      }
    }
    
    if(any(!c.dates%in%s.dates)){
      print(paste("Counts completed on ", paste( c.dates[!c.dates%in%s.dates],collapse=", "),"do not have matching surveys" ))
    }
  }
  
  ############### Surveys ##############################
  s.err<-0
  if(error.survey==1){  
  
  interv.time<-as.POSIXct(paste(survey.data$Date,survey.data$interv.time),format="%Y-%m-%d %H:%M")
  if(any(is.na(interv.time))){
    print(paste("Errors in interv.time on ", paste(survey.data$Date[which(is.na(interv.time))],collapse=", ") ))
    s.err<-1
    print(survey.data[which(is.na(interv.time)),])
  }
  start.time<-as.POSIXct(paste(survey.data$Date,survey.data$start.time),format="%Y-%m-%d %H:%M")
  if(any(is.na(start.time))){
    print(paste("Errors in start.time on ", paste(survey.data$Date[which(is.na(start.time))],collapse=", ") ))
    s.err<-1
    
    print(survey.data[which(is.na(start.time)),])
  }
  

  if(any(!s.dates%in%c.dates)){
    print(paste("Surveys completed on ", paste( s.dates[!s.dates%in%c.dates],collapse=", "),"are not in counts" ))
    s.err<-1
  }
  }
  
  if(s.err!=0){ stop("Fundamental errors detected in the survey.data")
  }else{print("No errors detected in survey.data")}
  
  if(c.err!=0){ stop("Fundamental errors detected in the count.data")
  }else{print("No errors detected in count.data")}

}
