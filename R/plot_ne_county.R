"plot_ne_county"<-function(surveydata,angler.type="all",type='num',brwr_pallet=''){
  if(brwr_pallet==""){
    brwr_pallet<- "RdBu"
  }

  require(ggplot2)
  require(reshape)
  require(maps)
  breaks<-c(-1,0,10,100,500,1000,1500,5000)
  
  surveydata<-surveydata[!duplicated(surveydata$int.name),]       #remove duplicated lines
  
  county.codes<-data.frame(county_code=c(1:93),subregion=c("douglas","lancaster","gage","custer","dodge","saunders",
  "madison","hall","buffalo","platte","otoe","knox","cedar","adams","lincoln","seward","york","dawson",
  "richardson","cass","scotts bluff","saline","boone","cuming","butler","antelope","wayne","hamilton",
  "washington","clay","burt","thayer", "jefferson","fillmore","dixon","holt","phelps","furnas","cheyenne",
  "pierce","polk","nuckolls","colfax", "nemaha","webster","merrick","valley","red willow","howard","franklin",
  "harlan","kearney","stanton", "pawnee","thurston","sherman","johnson","nance","sarpy","frontier","sheridan",
  "greeley","boyd","morrill", "box butte","cherry","hitchcock","keith","dawes","dakota","kimball","chase",
  "gosper","perkins","brown", "dundy","garden","deuel","hayes","sioux","rock","keya paha","garfield","wheeler",
  "banner","blaine","logan", "loup","thomas","mcpherson","arthur","grant","hooker"))
  
  surveydata.m<- melt(surveydata, id.vars=c("state","county","angler.type"), measure.vars=c("num.anglers"))
  surveydata.m$county<-as.numeric(as.character(surveydata.m$county))
  if(angler.type=="all"){
    county.sum<-cast(surveydata.m, state+county~variable,sum)
    county.sum.rev<-county.sum[county.sum$state==27| county.sum$state=="NE",c(2:3)]
    names(county.sum.rev)[1]<-"county_code"
  
    all.ne.county<-merge(county.codes,county.sum.rev,by="county_code",all=TRUE)
    all.ne.county$num.anglers[is.na(all.ne.county$num.anglers)]<-0
    all.ne.county$prop.anglers<-(all.ne.county$num.anglers/sum(all.ne.county$num.anglers))*100
    all.ne.county.rev<-all.ne.county[,c(2,3,4)]
    all.ne.county.rev$num.anglers.cat<-cut(all.ne.county.rev$num.anglers,breaks,labels=FALSE)
    out.of.state<-NULL
    out.of.state$count<-sum(county.sum$num.anglers[county.sum$state!=27])
    out.of.state$prop<-round(((out.of.state$count/sum(county.sum$num.anglers))*100),digits=2)

  }
  
  if(angler.type=="bank"){
    county.sum<-cast(surveydata.m, state+county+angler.type~variable,sum)
    county.sum.rev<-county.sum[county.sum$state==27 & county.sum$angler.type==1,c(2,4)]
    names(county.sum.rev)[1]<-"county_code"
  
    all.ne.county<-merge(county.codes,county.sum.rev,by="county_code",all=TRUE)
    all.ne.county$num.anglers[is.na(all.ne.county$num.anglers)]<-0
    all.ne.county$prop.anglers<-(all.ne.county$num.anglers/sum(all.ne.county$num.anglers))*100
    all.ne.county.rev<-all.ne.county[,c(2,3,4)]
    all.ne.county.rev$num.anglers.cat<-cut(all.ne.county.rev$num.anglers,breaks,labels=FALSE)
    out.of.state<-NULL
    out.of.state$count<-sum(county.sum$num.anglers[county.sum$state!=27& county.sum$angler.type==1])
    out.of.state$prop<-round(((out.of.state$count/sum(county.sum$num.anglers))*100),digits=2)
  
  }
  
  if(angler.type=="boat"){
    county.sum<-cast(surveydata.m, state+county+angler.type~variable,sum)
    county.sum.rev<-county.sum[county.sum$state==27 & county.sum$angler.type==2,c(2,4)]
    names(county.sum.rev)[1]<-"county_code"
  
    all.ne.county<-merge(county.codes,county.sum.rev,by="county_code",all=TRUE)
    all.ne.county$num.anglers[is.na(all.ne.county$num.anglers)]<-0
    all.ne.county$prop.anglers<-(all.ne.county$num.anglers/sum(all.ne.county$num.anglers))*100
    all.ne.county.rev<-all.ne.county[,c(2,3,4)]
    all.ne.county.rev$num.anglers.cat<-cut(all.ne.county.rev$num.anglers,breaks,labels=FALSE)
    out.of.state<-NULL
    out.of.state$count<-sum(county.sum$num.anglers[county.sum$state!=27& county.sum$angler.type==2])
    out.of.state$prop<-round(((out.of.state$count/sum(county.sum$num.anglers))*100),digits=2)
  
  }
  
   if(angler.type=="spear"){
    county.sum<-cast(surveydata.m, state+county+angler.type~variable,sum)
    county.sum.rev<-county.sum[county.sum$state==27 & county.sum$angler.type==4,c(2,4)]
    names(county.sum.rev)[1]<-"county_code"
  
    all.ne.county<-merge(county.codes,county.sum.rev,by="county_code",all=TRUE)
    all.ne.county$num.anglers[is.na(all.ne.county$num.anglers)]<-0
    all.ne.county$prop.anglers<-(all.ne.county$num.anglers/sum(all.ne.county$num.anglers))*100
    all.ne.county.rev<-all.ne.county[,c(2,3,4)]
    all.ne.county.rev$num.anglers.cat<-cut(all.ne.county.rev$num.anglers,breaks,labels=FALSE)
    out.of.state<-NULL
    out.of.state$count<-sum(county.sum$num.anglers[county.sum$state!=27& county.sum$angler.type==2])
    out.of.state$prop<-round(((out.of.state$count/sum(county.sum$num.anglers))*100),digits=2)
  
  }
  
    if(angler.type=="bow"){
    county.sum<-cast(surveydata.m, state+county+angler.type~variable,sum)
    county.sum.rev<-county.sum[county.sum$state==27 & county.sum$angler.type==5,c(2,4)]
    names(county.sum.rev)[1]<-"county_code"
  
    all.ne.county<-merge(county.codes,county.sum.rev,by="county_code",all=TRUE)
    all.ne.county$num.anglers[is.na(all.ne.county$num.anglers)]<-0
    all.ne.county$prop.anglers<-(all.ne.county$num.anglers/sum(all.ne.county$num.anglers))*100
    all.ne.county.rev<-all.ne.county[,c(2,3,4)]
    all.ne.county.rev$num.anglers.cat<-cut(all.ne.county.rev$num.anglers,breaks,labels=FALSE)
    out.of.state<-NULL
    out.of.state$count<-sum(county.sum$num.anglers[county.sum$state!=27& county.sum$angler.type==2])
    out.of.state$prop<-round(((out.of.state$count/sum(county.sum$num.anglers))*100),digits=2)
  
  }
  
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==1]<-"0"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==2]<-"1-10"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==3]<-"11-100"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==4]<-"101-500"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==5]<-"501-1000"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==6]<-"1001-1500"
    all.ne.county.rev$num.anglers.cat[all.ne.county.rev$num.anglers.cat==7]<-"1501-5000"
    all.ne.county.rev$num.anglers.cat<-factor(all.ne.county.rev$num.anglers.cat,levels=c("0","1-10","11-100","101-500","501-1000","1001-1500","1501-5000"))
    #all.ne.county.rev$num.anglers.cat<-all.ne.county.rev$num.anglers.cat[,drop=TRUE]
  ne.county<-map_data("county", "nebraska")
  ne.county.anglers<-merge(ne.county,all.ne.county.rev,by="subregion",all=TRUE)
  
  #reservoir_county<-subset(ne.county.anglers,subregion==reservoir.county)
  #reservoir_county.rev<-reservoir_county[1,]
  #reservoir_county.rev$long=mean(reservoir_county$long)
  #reservoir_county.rev$lat=mean(reservoir_county$lat)

  if(type=='num'){
     cols=c("0"="gray","1-10"="blue","11-100"="darkorchid","101-500"="red","501-1000"="orange","1001-1500"="yellow","1501-5000"="pink")
    p<-ggplot() +
    geom_polygon(data=ne.county.anglers, aes(long, lat, group = group, fill = num.anglers.cat))+ 
    scale_fill_manual(name = "Number of anglers",values = cols) +                
    theme_bw() + 
    borders("county","nebraska", colour="white")+
    xlab("") + 
    ylab("")+
    opts(title="", 
         axis.ticks = theme_blank(),
         axis.text.x = theme_blank(),
         axis.text.y = theme_blank())
  }
   if(type=='prop'){
    p<-ggplot() +
    geom_polygon(data=ne.county.anglers, aes(long, lat, group = group, fill = prop.anglers))+ 
    scale_fill_gradient(low="white",high="darkgray") +                
    theme_bw() + borders("county","nebraska", colour="black",size=1)+opts(title="", 
    axis.ticks = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank())+ xlab("") + ylab("")+
    coord_equal()
  }
  
  return(p)
}