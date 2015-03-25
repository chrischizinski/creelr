'allocate.in.timetable'<-function(x,start_endDates,cols=""){
if(cols==""){
cols=c(1,2)
}

 x<-as.data.frame(x)
 
 Month<-months(seq(start_endDates[1],start_endDates[2],by=31),abbreviate=TRUE)
 Month<-Month[sort(rep(seq_len(length(Month)), 2))]
 day_type<-rep(c("weekday","weekend"),length(Month)/2)
 
 output<-data.frame(Month,day_type,count=matrix(0,length(Month),(ncol(x)-max(cols))))
 ind<-match(paste(x[,cols[1]],x[,cols[2]]),paste(output[,cols[1]],output[,cols[2]]))
 output[ind,3:ncol(output)]<-x[,(cols[2]+1):ncol(x)]
 
 names(output)[3:ncol(x)]<- names(x)[(cols[2]+1):ncol(x)]
 return(output)
}
