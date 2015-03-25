'reorganize.fremont'<-function(count.data,lake.codes,do.debug=FALSE){
  reorg.data<-NULL
    
  for(i in 1:length(lake.codes)){
   lake.set<-subset(count.data,site==lake.codes[i])
   dup.ind<-duplicated(lake.set$Date)
   
   lake.set.orig<-lake.set[!dup.ind,]
   lake.set.dup<- lake.set[dup.ind,]
   match.ind<-match(lake.set.dup$Date,lake.set.orig$Date)
   
   for(j in 1:length(match.ind)){
    if(any(duplicated(match.ind)))stop(paste("There are duplicate values in Lake",lake.codes[i],sep=' '))
    lake.set.orig[match.ind[j],c(15)]<-paste(lake.set.dup[match.ind[j],c(9)])
    lake.set.orig[match.ind[j],c(16:20)]<-lake.set.dup[match.ind[j],c(10:14)]
     
   }
   if(do.debug==TRUE){
    print(paste(lake.codes[i]))
    print(lake.set.orig[,c(9,15)] )
    }
   if(do.debug==FALSE){
   reorg.data<-rbind(reorg.data,lake.set.orig)
   }
  }
  return(reorg.data)
}