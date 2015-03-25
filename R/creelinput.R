
 "creelinput"<-function(creelfiles,specialdata="",type="",doTrack=TRUE,doSave=TRUE){
  nfiles<-length(creelfiles)
  if(specialdata=="") specialdata=NULL
  if(type=="") type=NULL
  outfiles<-creel.process(datafiles=creelfiles,dotrack=doTrack)
  
  countdata<-outfiles$count
  surveydata<-outfiles$survey

  if(!is.null(specialdata)){  # Check to see if special data is null, if so continue adding in this data
    specfiles<-creel.process(datafiles=specialdata,dotrack=doTrack)  
    spec.countdata<-specfiles$count
    spec.surveydata<-specfiles$survey  
    spec.surveydata$int.name<-paste(spec.surveydata$int.name,"_spec",sep="")
    spec.surveydata$angler.type<-type
    if(nrow(spec.surveydata)==1 & spec.surveydata$int.name[1]=="0_spec"){spec.surveydata<-NULL}
    surveydata$angler.type<-as.character(surveydata$angler.type)
    surveydata.rev<-rbind(surveydata,spec.surveydata)
    surveydata<-surveydata.rev[order(surveydata.rev$Date),]
    
    if(type==3){ # ice angler
      countdata$spec.angl.1[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.1 
      countdata$spec.angl.2[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.2
      countdata$spec.angl.3[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.3
      countdata$spec.angl.4[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.4
    }
    
    if(type==4){ # spear angler
      countdata$spr.angl.1[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.1 
      countdata$spr.angl.2[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.2
      countdata$spr.angl.3[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.3
      countdata$spr.angl.4[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.4
    }
    if(type==5){ # bow angler
      countdata$bow.angl.1[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.1 
      countdata$bow.angl.2[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.2
      countdata$bow.angl.3[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.3
      countdata$bow.angl.4[match(countdata$Date,spec.countdata$Date)]<-spec.countdata$bank_ang.4
    }
  }
    if(doSave==TRUE){
    filnam.survey<-paste("survey",surveydata[1,1],strptime(Sys.time(),"%Y-%m-%d"),sep="_")
    write.csv(surveydata,paste(filnam.survey,"csv",sep="."))
    filnam.creel<-paste("count",countdata[1,1],countdata[1,4],strptime(Sys.time(),"%Y-%m-%d"),sep="_")
    write.csv(countdata,paste(filnam.creel,"csv",sep="."))
  }
  out<-list(surveydata=surveydata,countdata=countdata)
  return(out)
  
}
