'survey.party.catch'<-function(surveydata,doWrite=""){
if(doWrite==""){
  doWrite<-FALSE
}
  require(reshape)
  h.col=c("hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12","hL13","hL14","hL15")
  r.col=c("r.lt.125","r.125","r.150","r.175","r.200","r.230","r.255","r.280","r.305","r.330",
          "r.355","r.380","r.405","r.430","r.455","r.480","r.510","r.530","r.560","r.585","r.610",
          "r.635","r.660","r.685","r.710","r.735_815","r.840_915","r.gt.915")

  ## Error remove
  rm.error.harvest<-ifelse(surveydata[,"spp.harvest"]>0 & all(surveydata[,paste(h.col)]==0)| all(is.na(surveydata[,paste(h.col)])),0,1)
  rm.error.release<-ifelse(surveydata[,"spp.release"]>0 & all(surveydata[,paste(r.col)]==0)| all(is.na(surveydata[,paste(r.col)])),0,1)
  rm.error<-apply(cbind(rm.error.harvest,rm.error.release),1,function(x) !any(x==0))
  survey.rev<-surveydata[rm.error,]
  survey.rev[,c(paste(h.col),paste(r.col))]<-apply(survey.rev[,c(paste(h.col),paste(r.col))],c(1,2),as.numeric)
  
  survey.rev.a<-survey.rev[!duplicated(survey.rev$int.name),c("int.name","spp.harvest","harvest.num","harvest.mass","spp.release","release.num")]
  harvest.spp<-as.data.frame(matrix(0,nrow(survey.rev.a),15))
  harvest.num<-as.data.frame(matrix(0,nrow(survey.rev.a),15))
  harvest.mass<-as.data.frame(matrix(0,nrow(survey.rev.a),15)) 
  release.spp<-as.data.frame(matrix(0,nrow(survey.rev.a),15)) 
  release.num<-as.data.frame(matrix(0,nrow(survey.rev.a),15))
  foo.spp_b.b<-survey.rev[duplicated(survey.rev$int.name),c("int.name","spp.harvest","harvest.num","harvest.mass","spp.release","release.num")]

    s<-1
    while(nrow(foo.spp_b.b)>0){
      foo.spp_b.c<-foo.spp_b.b[!duplicated(foo.spp_b.b$int.name),c("int.name","spp.harvest","harvest.num","harvest.mass","spp.release","release.num")]
      foo.spp_b.b<-foo.spp_b.b[duplicated(foo.spp_b.b$int.name),c("int.name","spp.harvest","harvest.num","harvest.mass","spp.release","release.num")]
    
      ind<- match(foo.spp_b.c$int.name,survey.rev.a$int.name)
      harvest.spp[ind,s]<-as.numeric(foo.spp_b.c$spp.harvest)
      harvest.num[ind,s]<-as.numeric(foo.spp_b.c$harvest.num)
      harvest.mass[ind,s]<-as.numeric(foo.spp_b.c$harvest.mass)
      release.spp[ind,s]<- as.numeric(foo.spp_b.c$spp.release)
      release.num[ind,s]<- as.numeric(foo.spp_b.c$release.num)
      
      s<-s+1
    }
    
    cut.off<-max(which(apply(rbind(harvest.spp,harvest.num,harvest.mass,release.spp,release.num),2,FUN=sum)>0))
    if(is.infinite(cut.off)) cut.off<-1 # If cut off is infiinte (i.e., no catches anywhere then 1)
    
    namez<-c("int.name","harvest.spp.1", paste("harvest.spp",2:(cut.off+1),sep="."),"harvest.num.1",paste("harvest.num",2:(cut.off+1),sep="."),
             "harvest.mass.1",paste("harvest.mass",2:(cut.off+1),sep="."),"release.spp.1", paste("release.spp",2:(cut.off+1),sep="."),"release.num.1",
             paste("release.num",2:(cut.off+1),sep="."))
    
    add.spp<-data.frame(survey.rev.a$int.name,survey.rev.a$spp.harvest,harvest.spp[,c(1:cut.off)],survey.rev.a$harvest.num,
               harvest.num[,c(1:cut.off)],survey.rev.a$harvest.mass,harvest.mass[,c(1:cut.off)],survey.rev.a$spp.release,
               release.spp[,c(1:cut.off)],survey.rev.a$release.num,release.num[,c(1:cut.off)])

    names(add.spp)<-namez
    
 
    
  survey.info<-survey.rev[,c("site","int.name","Date","num.anglers","angler.type","start.time",  
                             "interv.time","time.diff_hr","trip.type","sp.sought","success","state","county",
                             "Month","weekday","day_type","time.period")]

  id_vars<-c("int.name","sp.sought","spp.harvest","spp.release")
  measure_vars<-c("harvest.num","harvest.mass","release.num")#,"hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12","hL13","hL14","hL15")

  # catches from the survey
  survey.rev.m<-melt(survey.rev,id.vars=id_vars,measure.vars=measure_vars)
  survey.rev.m$value<-as.numeric(as.character(survey.rev.m$value))
  fish.catch.total<-cast(survey.rev.m, int.name~variable,sum)
  fish.catch.total$ttlcat_number<-fish.catch.total$harvest.num+fish.catch.total$release.num # total catch by interview number

  
  # sucessful catches
  target.success<-ifelse(survey.rev$sp.sought==survey.rev$spp.harvest|
         survey.rev$sp.sought==survey.rev$spp.release|
         (survey.rev$sp.sought== 999 & (survey.rev$spp.harvest>0 | survey.rev$spp.release>0)),1,0)
  #target.success.rev<-recast(data.frame(int.name=survey.rev$int.name,target.success),int.name~variable,sum,id.var="int.name" )
  #fish.catch.total.rev<-merge(fish.catch.total,target.success.rev,by="int.name") # harvest by party
  ####################################################################################################################
  survey.ttl_harvest<-merge(survey.info,fish.catch.total,by="int.name") # harvest by party
  survey.ttl_harvest$Har_ang<-survey.ttl_harvest$harvest.num/survey.ttl_harvest$num.anglers  # harvest per angler
  survey.ttl_harvest$Rel_ang<-survey.ttl_harvest$release.num/survey.ttl_harvest$num.anglers  # release per angler
  survey.ttl_harvest$Cat_ang<-survey.ttl_harvest$ttlcat_number/survey.ttl_harvest$num.anglers  # total catch per angler
  survey.ttl_harvest$Pressure<-survey.ttl_harvest$num.anglers*survey.ttl_harvest$time.diff_hr # angling pressure per party
  survey.ttl_harvest$Har_cpue<-survey.ttl_harvest$harvest.num/survey.ttl_harvest$Pressure  # harvest per angler
  survey.ttl_harvest$Rel_cpue<-survey.ttl_harvest$release.num/survey.ttl_harvest$Pressure  # release per angler
  survey.ttl_harvest$Cat_cpue<-survey.ttl_harvest$ttlcat_number/survey.ttl_harvest$Pressure # total catch per angler
  survey.ttl_harvest$HoursC<-NA
  survey.ttl_harvest$HoursC[survey.ttl_harvest$trip.type==1]<-survey.ttl_harvest$Pressure[survey.ttl_harvest$trip.type==1]
  survey.ttl_harvest$NumAngC<-NA
  survey.ttl_harvest$NumAngC[survey.ttl_harvest$trip.type==1]<-survey.ttl_harvest$num.anglers[survey.ttl_harvest$trip.type==1]
  survey.ttl_harvest$Hrs_AngC<-NA
  survey.ttl_harvest$Hrs_AngC[survey.ttl_harvest$trip.type==1]<-survey.ttl_harvest$time.diff_hr[survey.ttl_harvest$trip.type==1]
  survey.ttl_harvest$HoursI<-NA
  survey.ttl_harvest$HoursI[survey.ttl_harvest$trip.type==2]<-survey.ttl_harvest$Pressure[survey.ttl_harvest$trip.type==2]
  survey.ttl_harvest$NumAngI<-NA
  survey.ttl_harvest$NumAngI[survey.ttl_harvest$trip.type==2]<-survey.ttl_harvest$num.anglers[survey.ttl_harvest$trip.type==2]
  survey.ttl_harvest$Hrs_AngI<-NA
  survey.ttl_harvest$Hrs_AngI[survey.ttl_harvest$trip.type==2]<-survey.ttl_harvest$time.diff_hr[survey.ttl_harvest$trip.type==2]
  
  survey.ttl_harvest.reva<-merge(survey.ttl_harvest,add.spp,by="int.name",all=TRUE)
  survey.ttl_harvest.rev<-survey.ttl_harvest.reva[!(duplicated(survey.ttl_harvest.reva$int.name)),]
  ###############################################################################
  spp.harv.ind<-which(substr(names(survey.ttl_harvest.rev),1,12)=="harvest.spp.")
  spp.rel.ind<-which(substr(names(survey.ttl_harvest.rev),1,12)=="release.spp.")
  harv.num.ind<-which(substr(names(survey.ttl_harvest.rev),1,12)=="harvest.num.")
  harv.mass.ind<-which(substr(names(survey.ttl_harvest.rev),1,13)=="harvest.mass.")
  rel.num.ind<-which(substr(names(survey.ttl_harvest.rev),1,12)=="release.num.")
  
  species.caught<-survey.ttl_harvest.rev[,c(spp.harv.ind,spp.rel.ind)]
  spp.catch.nums<- survey.ttl_harvest.rev[,c(harv.num.ind,rel.num.ind)]
  spp.catch.nums<-apply(spp.catch.nums,c(1,2), as.numeric)
  spp.catch.mass<- survey.ttl_harvest.rev[,c(harv.mass.ind)]
  spp.catch.mass<-apply(spp.catch.mass,c(1,2), as.numeric)

  cws.nums<-as.data.frame(matrix(0,length(survey.ttl_harvest.rev$sp.sought),4))
  names(cws.nums)<-c("cws","cws.harvest","cws.mass","cws.release")

  species.caught<-apply(species.caught,c(1,2),as.character)
  species.caught<-apply(species.caught,c(1,2),function(x) trim(x))
  spp.sought<-as.character(survey.ttl_harvest.rev$sp.sought)
  spp.sought<-trim(spp.sought)


  for(i in 1:length(spp.sought)){
    if(is.na(spp.sought[i])){
        cws.nums$cws[i]<-NA
        cws.nums$cws.harvest[i]<-NA
        cws.nums$cws.mass[i]<-NA
        cws.nums$cws.release[i]<-NA
      }
    if(!is.na(spp.sought[i])){
    if(
      (spp.sought[i]%in%species.caught[i,]) |
      (spp.sought[i]=="999" & any(species.caught[i,]!="0"))
       ){
      cws.nums$cws[i]=1
      
      cws.ind<-as.numeric(which((spp.sought[i]==species.caught[i,]) |
        (spp.sought[i]=="999" & any(species.caught[i,]!="0"))))
      
      if(any(substr(colnames(species.caught)[cws.ind],1,3)=="har")){
        cws.harv.ind<-which(substr(colnames(species.caught)[cws.ind],1,3)=="har")
        cws.nums$cws.harvest[i]<-sum(spp.catch.nums[i,cws.ind[cws.harv.ind]])
        cws.nums$cws.mass[i]<-sum(spp.catch.mass[i,cws.ind[cws.harv.ind]])
      }  
      if(any(substr(colnames(species.caught)[cws.ind],1,3)=="rel")){
        cws.rel.ind<-which(substr(colnames(species.caught)[cws.ind],1,3)=="rel")
        cws.nums$cws.release[i]<-sum(spp.catch.nums[i,cws.ind[cws.rel.ind]])
      }
     }
    } 
  }
  cws.nums$cws.catch<-cws.nums$cws.harvest+ cws.nums$cws.release
  survey.ttl_harvest.revb<-cbind(survey.ttl_harvest.rev,cws.nums)
  #survey.ttl_harvest.revb$Month<-months(survey.ttl_harvest.revb$Date)
  
  ###############################################################################
  
  if(doWrite==TRUE){
    write.csv(survey.ttl_harvest.revb,paste("surveyoutput_",format(Sys.Date(), "%d%b%Y"),".csv",sep=""))
  }

  return(survey.ttl_harvest.revb)
}