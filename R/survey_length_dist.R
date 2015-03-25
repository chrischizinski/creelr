"survey_length_dist"<-function(surveydata,lib.loc){
  require(reshape)
  require(plyr)
  surveydata$Month<-factor(surveydata$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  surveydata$Month<-surveydata$Month[,drop=TRUE]
  uniq.harvest<-unique(surveydata$spp.harvest)
  if(any(uniq.harvest!=0)){
    uniq.harvest<-uniq.harvest[uniq.harvest!=0]
    colz.h<-c("int.name","site","Date","num.anglers","angler.type","start.time",
              "interv.time","time.diff_hr","trip.type","sp.sought","success","state","county","spp.harvest",
              "harvest.num","harvest.mass","hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12",
              "hL13","hL14","hL15","Month","weekday","day_type","time.period")
    harvest.data<-subset(surveydata,surveydata$spp.harvest%in%uniq.harvest)[,paste(colz.h)]
    harvest.data$Year<-as.character(format(harvest.data$Date,"%Y"))
    id.vars=c("int.name","Month","Year","spp.harvest","angler.type","trip.type")
    measure.vars=c(paste("hL",1:15,sep=""))
    harvest.data.m<-melt(harvest.data,id.vars=id.vars,measure.vars=measure.vars)
    harvest.data.m<-harvest.data.m[harvest.data.m$value!=0,-which(colnames(harvest.data.m)=="variable")]
    harvest.lengths<-ddply(harvest.data.m,c("spp.harvest"))
    names(harvest.lengths)[which(colnames(harvest.lengths)=="spp.harvest")]<-"species"
    names(harvest.lengths)[which(colnames(harvest.lengths)=="value")]<-"length"
    harvest.lengths$type="harvest"
    harvest.lengths<- match_creel_code(harvest.lengths,"species","fish.codes",lib.loc)
  }else{harvest.lengths<-NULL}
  
  
  uniq.release.a<-unique(surveydata$spp.release)
  if(any(uniq.release.a!=0)){
    uniq.release<-uniq.release.a[uniq.release.a!=0]
    colz.r<-c("int.name","site","Date","num.anglers","angler.type","start.time",
              "interv.time","time.diff_hr","trip.type","sp.sought","success","state","county","spp.release",
              "release.num","r.lt.125","r.125","r.150","r.175","r.200","r.230","r.255","r.280",
              "r.305","r.330","r.355","r.380","r.405","r.430","r.455","r.480",
              "r.510","r.530","r.560","r.585","r.610","r.635","r.660","r.685",
              "r.710","r.735_815","r.840_915","r.gt.915","Month","weekday","day_type","time.period")
    release.data<-subset(surveydata,surveydata$spp.release%in%uniq.release)[,colz.r]
    release.data$Year<-as.character(format(release.data$Date,"%Y"))
    release.sizes<-release.data[,c("r.lt.125","r.125","r.150","r.175","r.200","r.230","r.255","r.280",
                                   "r.305","r.330","r.355","r.380","r.405","r.430","r.455","r.480",
                                   "r.510","r.530","r.560","r.585","r.610","r.635","r.660","r.685",
                                   "r.710","r.735_815","r.840_915","r.gt.915")]
    rel.ind<-which(release.sizes>0,arr.ind=T)
    rel.nums<- release.sizes[rel.ind]
    release.lengths.a<-rep(colnames(release.sizes)[rel.ind[,2]],rel.nums)
    release.lengths.b<-substr(release.lengths.a,3,nchar(release.lengths.a))
    release.lengths.b<-ifelse(release.lengths.b=="lt.125","80",release.lengths.b)
    release.lengths.b<-ifelse(release.lengths.b=="735_815","775",release.lengths.b)
    release.lengths.b<-ifelse(release.lengths.b=="840_915","878",release.lengths.b)
    release.id<-release.data[rel.ind[,1],c("int.name","Month","Year","spp.release","angler.type","trip.type")]
    release.id<-release.id[rep(1:nrow(release.id),rel.nums),]
    release.id<-match_creel_code(release.id,"spp.release","fish.codes",lib.loc)
    release.lengths<-ddply(data.frame(release.id,length=release.lengths.b),c("spp.release"))
    names(release.lengths)[which(colnames(release.lengths)=="spp.release")]<-"species"
    release.lengths$type="release"
  }else{release.lengths<-NULL}
  
  out<-rbind(harvest.lengths,release.lengths)
  out$length<-as.numeric(as.character(out$length))
  
  out[is.na(out$length),]
  return(out)
}