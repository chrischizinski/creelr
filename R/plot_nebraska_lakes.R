'plot_nebraska_lakes'<-function(combine.data,cols="",scale="",do.border="",do.label=""){
  require(ggplot2)
  load("H:/DataDepot/R/Creel/GIS/NElakes.Rdata")
  lake.names<-paste(combine.data[,1])
  if(do.border=="")do.border=FALSE
  if(do.label=="")do.label=FALSE
  if(scale=="")scale=1
  if(!is.numeric(combine.data[,2])){
    names(combine.data)[2]<-"factors"
    do.numeric=FALSE
  }else{do.numeric=TRUE}
  
  if('all.fremont'%in%lake.names){
    lake.names.rev<-lake.names[lake.names!='all.fremont']
    fremont.lakes<-c("Fremont Lake #1","Fremont Lake #2","Fremont Lake #3","Fremont Lake #3A","Fremont Lake #4",'Fremont Lake #5',
    'Fremont Lake #6','Fremont Lake #8','Fremont Lake #9','Fremont Lake #10','Fremont Lake #11',
    'Fremont Lake #12','Fremont Lake #13','Fremont Lake #14','Fremont Lake #15 (Victory Lake)','Fremont Lake #16',
    'Fremont Lake #17','Fremont Lake #18','Fremont Lake #19','Fremont Lake #20')
    #lake.names<-c(lake.names.rev,fremont.lakes)
    allF.ind<-which(combine.data[,1]=='all.fremont')
    fremont.data<-data.frame(fremont.lakes,rep(combine.data[allF.ind,2],length(fremont.lakes)))
    names(fremont.data)<-names(combine.data)
    combined.data.rev<-rbind(combine.data,fremont.data)
    combine.data<-combined.data.rev[-allF.ind,]
    lake.names<-paste(combine.data[,1])
  }
  if(any(!tolower(lake.names)%in%unique(tolower(ne.lakes$Name)))){stop("Your lake is not found in NE lakes list. Check lake name")}
   
  foo.lake<-subset(ne.lakes,tolower(ne.lakes$Name)%in%paste(tolower(lake.names)))
  foo.lake.rev<-data.frame(foo.lake,combine.data[match(tolower(foo.lake$Name),tolower(combine.data$lake.names)),2])
  names(foo.lake.rev)[ncol(foo.lake.rev)]<-names(combine.data)[2]
  
  rm(ne.lakes,foo.lake)
  if(do.border==TRUE){
    if(do.numeric==FALSE){
      map<-ggplot()+
      geom_polygon(data=ne.map,aes(long,lat,group=group),fill = "lightgray")+
      geom_polygon(data=foo.lake.rev,aes(long,lat,group=group,
                   fill=factors,
                   colour=factors),
                   size=scale, linetype=1)+
      scale_fill_manual("Count",values=cols)+                              
      scale_colour_manual("Count",values=cols)+
      theme_bw()+
           scale_y_continuous("")+
           scale_x_continuous("")+
           theme(axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                legend.position="none")  
    }else{
    map<-ggplot()+
      geom_polygon(data=ne.map,aes(long,lat,group=group),fill = "lightgray")+
      geom_polygon(data=foo.lake.rev,aes(long,lat,group=group,fill=num, colour=num),size=scale, linetype=1)+
      scale_fill_continuous("Count", low = "blue", high = "red") +
      scale_colour_continuous("Count",low = "blue", high = "red") +
            theme_bw()+
           scale_y_continuous("")+
           scale_x_continuous("")+
           theme(axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank()) 
    }
    }else{
    if(do.numeric==FALSE){
      map<-ggplot()+
            geom_polygon(data=foo.lake.rev,aes(long,lat,group=group,
                   fill=factors,
                   colour=factors),
                   size=scale, linetype=1)+
      scale_fill_manual("Count",values=cols)+
      scale_colour_manual("Count",values=cols)+
      theme_bw()+
           scale_y_continuous("")+
           scale_x_continuous("")+
           theme(axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                legend.position="none") 
    }else{
    map<-ggplot()+
      geom_polygon(data=foo.lake.rev,aes(long,lat,group=group,fill=num, colour=num),size=scale, linetype=1)+
      scale_fill_continuous("Count", low = "blue", high = "red") +
      scale_colour_continuous("Count",low = "blue", high = "red") +
      theme_bw()+
           scale_y_continuous("")+
           scale_x_continuous("")+
           theme(axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank()) 
    }
    }  
  
    if(do.label==TRUE){ 
      ind<-which(tolower(lake.mids$Name)%in%tolower(lake.names)) 
      map<-map+geom_text(data=lake.mids[ind,],aes(long,lat,label=Name), vjust=1,size=3, position="dodge")
    
    }

      return(map)

}