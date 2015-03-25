
'daily.angler.effort'<-function(count,LOD.info,num.time.periods="",num.lake.sections="",
  timeprob="", sectionprob=""){
  require(stringr)
  require(reshape2)
  if(num.time.periods==""){
    num.time.periods<-2
  }
  
  if(num.lake.sections==""){
    num.lake.sections<-1
  }

  if(timeprob==""){
    time.period<-c(1,2)
    prob.time=c(0.5,0.5)
  }
  timeprob<-data.frame(time.period,prob.time)
  
  if(sectionprob==""){
    sections<-c(1)
    prob.sections=c(1)
    count$lake.section<-1
  }
  sectionprob<-data.frame(sections,prob.sections)
  count.info<-as.data.frame(matrix(0,nrow(count),3))
  names(count.info)<-c("LOD","timeprob","sectionprob")
  for(i in 1:nrow(count)){
   count.info$LOD[i]<-lod.dat$LOD[which(count$Month[i]==lod.dat$Month)]
   count.info$timeprob[i]<-timeprob$prob.time[which(count$time.period[i]==timeprob$time.period)]
   count.info$sectionprob[i]<-sectionprob$prob.sections[which(count$lake.section[i]==sectionprob$sections)]
  }
 mean.func<-function(x) mean(x,na.rm=TRUE) #mean with remove NA's
  
  names_count<-names(count)
  names_count<-gsub("boats_ang","boat_ang",names_count)
  names(count)<-names_count

 bank_anglers<-apply(count[str_detect(names(count), fixed("bank_ang"))],1, function(x) mean(x,na.rm=TRUE))
 bank_anglers[is.nan(bank_anglers)]<-0        # correct for missing data
 boats<-apply(count[str_detect(names(count), fixed("boats"))],1, mean.func)
 boats[is.nan(boats)]<-0
 boat_anglers<-apply(count[str_detect(names(count), fixed("boat_ang"))],1, function(x) mean(x,na.rm=TRUE))
 boat_anglers[is.nan(boat_anglers)]<-0
 spr_anglers<-apply(count[str_detect(names(count), fixed("spr.angl"))],1, function(x) mean(x,na.rm=TRUE))
 spr_anglers[is.nan(spr_anglers)]<-0        # correct for missing data  
 bow_anglers<-apply(count[str_detect(names(count), fixed("bow.angl"))],1, function(x) mean(x,na.rm=TRUE))
 bow_anglers[is.nan(bow_anglers)]<-0        # correct for missing data
 spec_anglers<-apply(count[str_detect(names(count), fixed("spec.angl"))],1, function(x) mean(x,na.rm=TRUE))
 spec_anglers[is.nan(spec_anglers)]<-0
  
 ang_boat <-rep(0,length(boat_anglers))
 
 ang_boat[boats>0]<-boat_anglers[boats>0]/boats[boats>0]

 non_ang<-apply(count[str_detect(names(count), fixed("non_ang"))],1, mean.func)
 tot_anglers<-apply(cbind(bank_anglers,boat_anglers,spr_anglers,bow_anglers,spec_anglers),1,sum)
 

 adj_tot_angl.a<-tot_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_tot_angler_e<- adj_tot_angl.a/(count.info$timeprob*count.info$sectionprob)
 
 adj_bank_angl.a<-bank_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_bank_angler_e<-adj_bank_angl.a/(count.info$timeprob*count.info$sectionprob)
 
 adj_boat_anglers.a<-boat_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_boat_angler_e<- adj_boat_anglers.a/(count.info$timeprob*count.info$sectionprob)
  
 adj_spr_anglers.a<-spr_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_spr_angler_e<- adj_spr_anglers.a/(count.info$timeprob*count.info$sectionprob)
  
 adj_bow_anglers.a<-bow_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_bow_angler_e<- adj_bow_anglers.a/(count.info$timeprob*count.info$sectionprob)
  
 adj_spec_anglers.a<-spec_anglers*(count.info$LOD/num.time.periods)            # total  angler adjusted effort (Malvestuto 1978)
 adj_spec_angler_e<- adj_spec_anglers.a/(count.info$timeprob*count.info$sectionprob)
 
 output<-data.frame(count[,c("site","Date","time.1","Month","weekday","day_type","time.period")],
            TotalAngEffort=adj_tot_angler_e,BankAngEffort=adj_bank_angler_e,BoatAngEffort=adj_boat_angler_e,
            SprAngEffort=adj_spr_angler_e,BowAngEffort=adj_bow_angler_e,SpecAngEffort=adj_spec_angler_e,Angl_boat=ang_boat)
 if(any(duplicated(paste(output$site,output$Date,sep='_')))){
   warning("Duplicated dates in count data, mean taken", call. = FALSE)
   
   
   
    mean.output<-dcast(melt(output,id.var=c("site","Date"),measure.var=c("TotalAngEffort","BankAngEffort","BoatAngEffort","SprAngEffort","BowAngEffort","SpecAngEffort","Angl_boat")),site+Date~variable,mean)
    id.var<-output[!duplicated(paste(output$site,output$Date)),c("site","Date","time.1","Month","weekday","day_type","time.period")]
    output<-merge(id.var,mean.output,by=c("site","Date"),all=TRUE)   
 }           
 return(output)
}
