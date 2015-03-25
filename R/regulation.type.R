'regulation.type'<-function(species){
  reg.type<-NULL
  for(i in 1:length(species)){
    if(species[i] %in% c(520)){
      reg.type<-rbind(reg.type,"salmonid")
    }
    if (species[i] %in% c(620,640,645)){
      reg.type<-rbind(reg.type,"morone")
    }
    if (species[i] %in% c(840,845,850)){
      reg.type<-rbind(reg.type,"sander")
    }
    if (species[i] %in% c(430,435)){
      reg.type<-rbind(reg.type,"muskie")
    }
    if (species[i] %in% c(28)){
      reg.type<-rbind(reg.type,"shovelnose")
    }
    if (species[i] %in% c(24,26)){
      reg.type<-rbind(reg.type,"pallid&lake")
    }
    if (species[i] %in% c(420)){
      reg.type<-rbind(reg.type,"esox")
    }
    if (species[i] %in% c(350)){
      reg.type<-rbind(reg.type,"blue_cat")
    }
    if (species[i] %in% c(370)){
      reg.type<-rbind(reg.type,"flathead")
    }
    if (species[i] %in% c(700,720,722,724,725,726,728,730,735,740,745,780,785,790,
                          795,710,714,780)){
      reg.type<-rbind(reg.type,"panfish")
    }
    if (species[i] %in% c(940)){
      reg.type<-rbind(reg.type,"clam")
    }
    if (species[i] %in% c(910)){
      reg.type<-rbind(reg.type,"bullfrog")
    }
    if (species[i] %in% c(950)){
      reg.type<-rbind(reg.type,"snapper")
    }
    if (species[i] %in% c(770,750, 760)){
      reg.type<-rbind(reg.type,"micropterus")
    }
    if (species[i] %in% c(360)){
      reg.type<-rbind(reg.type,"channel_cat")
    }
    if (!species[i] %in% c(500,505,510,520,521,522,523,524,525,526,527,528,529,
                           530,531,532,550,555,560,561,562,563, 565,620,640,645,
                           840,845,850,430,435,28,24,26,420,350,370,700,720,722,
                           724,725,726,728,730,735,740,745,780,785,790,795,710,714,
                           780, 940, 910,950,770,750,760,360)){
      reg.type<-rbind(reg.type,"no_reg")
    }
  }
  return(reg.type)
}
