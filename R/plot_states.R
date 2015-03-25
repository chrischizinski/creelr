'plot_states'<-function(surveydata,angler.type="all",type='num'){
  require(ggplot2)
  #require(reshape)
  require(plyr)
  require(maps)
  breaks<-c(-1,0,10,100,500,1000,1500,5000)
  surveydata<-surveydata[!duplicated(surveydata$int.name),]       #remove duplicated lines
  #names(surveydata)[12]<-"region"
  
  surveydata_sum<-ddply(surveydata, .(state,angler.type),summarize,num.anglers=sum(num.anglers))
  names(surveydata_sum)[1]<-"region"
  surveydata_sum<- match_creel_code(surveydata_sum,colz="region",code_type=c("state.codes"),lib.loc)
  surveydata_sum$region<-tolower(surveydata_sum$region)
  


  if(angler.type=="all"){
    state_anglers<-ddply(surveydata_sum,.(region),summarize,num.anglers=sum(num.anglers))
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
  }
  
  if(angler.type=="bank"){
    state_anglers<-subset(surveydata_sum,angler.type==1)
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
  }
  
  if(angler.type=="boat"){
    state_anglers<-subset(surveydata_sum,angler.type==2)
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
  }
  if(angler.type=="ice"){
    state_anglers<-subset(surveydata_sum,angler.type==3)
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
  }
  if(angler.type=="spear"){
    state_anglers<-subset(surveydata_sum,angler.type==4)
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
  }
  if(angler.type=="bow"){
    state_anglers<-subset(surveydata_sum,angler.type==5)
    state_anglers$numcat<-cut(state_anglers$num.anglers,breaks,labels=FALSE)
    state_anglers$prop<-(state_anglers$num.anglers/sum(state_anglers$num.anglers))*100
  }
  
    state_anglers$numcat[state_anglers$numcat==1]<-"0"
    state_anglers$numcat[state_anglers$numcat==2]<-"1-10"
    state_anglers$numcat[state_anglers$numcat==3]<-"11-100"
    state_anglers$numcat[state_anglers$numcat==4]<-"101-500"
    state_anglers$numcat[state_anglers$numcat==5]<-"501-1000"
    state_anglers$numcat[state_anglers$numcat==6]<-"1001-1500"
    state_anglers$numcat[state_anglers$numcat==7]<-"1501-5000"

  
  all_states <- map_data("state")
  all_states$num.angs<-"0"
  ind<-which(all_states$region%in%state_anglers$region)
  all_states$num.angs[ind]<-state_anglers$numcat[match(all_states$region[ind],state_anglers$region)]
  all_states$num.angs<-factor(all_states$num.angs,levels=c("0","1-10","11-100","101-500","501-1000","1001-1500","1501-5000"))
  all_states$prop.angs<-0
  all_states$prop.angs[ind]<-state_anglers$prop[match(all_states$region[ind],state_anglers$region)]
  all_states$prop.angs<-round(all_states$prop.angs,digits=1)
  
 
  if(type=='num'){
    cols=c("0"="gray","1-10"="blue","11-100"="darkorchid","101-500"="red","501-1000"="orange","1001-1500"="yellow","1501-5000"="pink")
    map<-ggplot()+ geom_polygon(data=all_states, aes(x=long, y=lat,group=group, fill=num.angs),colour="white") +
         scale_fill_manual(name = "Number of anglers",values=cols)+
         labs(title="",x="",y="")+
         theme_bw() +
        theme(axis.ticks = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())
    }
    
   if(type=='prop'){
    map<-ggplot()+ geom_polygon(data=all_states, aes(x=long, y=lat,group=group, fill=factor(prop.angs)),colour="white") +
    scale_fill_brewer(name = "Proportion of anglers",pal = paste(brwr_pallet))+
    labs(title="",x="",y="")+
    theme_bw() +
    theme(axis.ticks = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())
    }
  
return(map)
}