
'compliance.assess'<-function(survey.data,site=NULL,doPrint=1,doWrite=1){
  
#   if(names(survey.data)[38]!='hl1') stop("Incorrect column alignment in survey.data")
  
  compliance.names<-c("int.name","site","catch","trout.bag.limit","trout.oneover","morone.bag.limit","morone.oneover",
                      "sander.bag.limit","sander.under.length","sander.over.length","sander.more15_18","sander.more22",
                      "sander.more15_20","sander.more28","sander.slot", "muskie.bag.limit","crappie.under.length", 
                      "muskie.under.length","shovlenose.bag.limit","pallid_lake.bag.limit","esox.bag.limit","esox.slot","esox.oneover",
                      "esox.under.limit","blue_cat.bag.limit","flathead.bag.limit","flathead.oneover","panfish.bag.limit","clam.bag.limit",
                      "bullfrog.bag.limit","snapper.bag.limit","micropterus.bag.limit","micropterus.oneover",
                      "micropterus.under.length","channel_cat.bag.limit")
  
  r.10inch<-250 # regulation standards (mm)
  r.12inch<-300
  r.15inch<-380
  r.16inch<-405
  r.18inch<-455
  r.20inch<-510
  r.21inch<-530
  r.22inch<-560
  r.28inch<-710
  r.30inch<-760
  r.34inch<-860
  r.40inch<-1015
  
  violations<-NULL
  
  harvest.surveys<-survey.data[survey.data$spp.harvest!="0",]
  
  uniq.surveys<-unique(harvest.surveys$int.name)
  for(i in 1:length(uniq.surveys)){
    compliance.data<-data.frame(matrix(0,1,length(compliance.names))) # storage matrix for the binary violations
    names(compliance.data)<-compliance.names    # name the column headers of the storate matrix
    harvest.surveys$site<-site
    
    if(doPrint==1){
      print(paste('Processing survey',uniq.surveys[i],sep=' '))
    }
    
    foo.survey<-subset(harvest.surveys,int.name==paste(uniq.surveys[i])) #look at each survey individually
    foo.survey[,c("harvest.num","harvest.mass","hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10",
                  "hL11","hL12","hL13","hL14","hL15")]<-
                    apply(foo.survey[,c("harvest.num","harvest.mass","hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10",
                                                "hL11","hL12","hL13","hL14","hL15")],c(1,2),function(x) as.numeric(as.character(x)))
    compliance.data$int.name<-paste(uniq.surveys[i]) # paste in the survey number in the storage matrix
    compliance.data$site<-foo.survey$site[1]
    num.fish<-which(foo.survey$spp.harvest>0)
    if(length(num.fish)>0){                             # if no fish skip to next survey
      compliance.data$catch<-1                          # binary 1 for fish were caught
      foo.survey.rev<-foo.survey[num.fish,]         # remove the release data and only lines with fish harvest info
      reg.type<-regulation.type(foo.survey.rev$spp.harvest)  #check the species to see if there are regulations on that species
      length.id<-which(colnames(foo.survey.rev)%in%c("hL1","hL2","hL3","hL4","hL5","hL6","hL7","hL8","hL9","hL10",
                                                                             "hL11","hL12","hL13","hL14","hL15"))
      
      
      
      if(any(reg.type!="no_reg")){
        uniq.regs<-unique(reg.type)
        for(j in length(uniq.regs)){                            # cycle through these applicable regulations
          ind<-which(reg.type==uniq.regs[j])                    # for the ith regulation which harvest fish are applicable
          
          #############################################################################
          ################ regulation types ###########################################
          
          ################## salmnonid regs ###########################################
          if(uniq.regs[j]=="salmonid"){                         
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(5*foo.survey.rev$num.anglers[1])){
              compliance.data$trout.bag.limit<-1
            }
            if(length(which(foo.survey.rev[ind,length.id]>r.16inch))>(1*foo.survey.rev$num.anglers[1])){
              compliance.data$trout.oneover<-1
            }
          }
          
          ################# morone regulations ########################################
          if(uniq.regs[j]=="morone"){                           # striped bass regulations
            if(foo.survey.rev$site[1] %in% c(3335,5110,5130,5135)){
              
              if(foo.survey.rev$site[1] %in% c(3335)){
                if(sum(foo.survey.rev[ind,"harvest.num"])>(3*foo.survey.rev$num.anglers[1])){      #bag limit
                  compliance.data$morone.bag.limit<-1
                }
              }
              if(foo.survey.rev$site[1] %in% c(5110)){                                                                                      
                if(sum(foo.survey.rev[ind,"harvest.num"])>0){
                  compliance.data$morone.bag.limit<-1
                }
              }
              
              if(foo.survey.rev$site[1] %in% c(5130,5135)){
                if(sum(foo.survey.rev[ind,"harvest.num"])>(3*foo.survey.rev$num.anglers[1])){      #  Special Regs
                  compliance.data$morone.bag.limit<-1
                }
                if(length(which(foo.survey.rev[ind,length.id]>=r.18inch))>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$morone.oneover<-1                  # one over limit
                }
              
              }
            }else{
              if(sum(foo.survey.rev[ind,"harvest.num"])>(15*foo.survey.rev$num.anglers[1])){      # Statewide Regs
                compliance.data$morone.bag.limit<-1
              }             
            }
          }        
          ################ muskie regs #################################################
          if(uniq.regs[j]=="muskie"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(1*foo.survey.rev$num.anglers[1])){
              compliance.data$muskie.bag.limit<-1
            }
            if(any(which(foo.survey.rev[ind,length.id]<r.40inch & foo.survey.rev[ind,length.id]>0))){
              compliance.data$muskie.under.length<-1
            }
          }
          ################ esox regs  #################################################
          if(uniq.regs[j]=="esox"){
            if(foo.survey.rev$site[1] %in% c(1600,4900,4910,6925)){
              if(foo.survey.rev$site[1] %in% c(4900,4910,6925)){
                if(any(which(foo.survey.rev[ind,length.id]<r.30inch & foo.survey.rev[ind,length.id]>0))){
                  compliance.data$esox.under.limit<-1
                }
                if(sum(foo.survey.rev[ind,"harvest.num"])>(3*foo.survey.rev$num.anglers[1])){
                  compliance.data$esox.bag.limit<-1
                }
                
              }
              if(foo.survey.rev$site[1] %in% c(1600)){
                if(any(which(foo.survey.rev[ind,length.id]<r.34inch & foo.survey.rev[ind,length.id]>r.28inch))){
                  compliance.data$esox.slot<-1
                }
                if(sum(foo.survey.rev[ind,"harvest.num"])>(10*foo.survey.rev$num.anglers[1])){
                  compliance.data$esox.bag.limit<-1
                }
                
                if(length(which(foo.survey.rev[ind,length.id]>r.34inch))>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$esox.oneover<-1
                }

              }
              
              }else{
                if(sum(foo.survey.rev[ind,"harvest.num"])>(3*foo.survey.rev$num.anglers[1])){
                  compliance.data$esox.bag.limit<-1
                }
            }
          } 
          ############### shovelnose regs #############################################
          if(uniq.regs[j]=="shovelnose"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(10*foo.survey.rev$num.anglers[1])){
              compliance.data$shovlenose.bag.limit<-1
            }
          }
          ############### pallid and lake regs ########################################
          if(uniq.regs[j]=="pallid&lake"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>0){
              compliance.data$pallid_lake.bag.limit<-1
            }
          }
          
          ############### Channel catfish ###########################################
          if(uniq.regs[j]=="channel_cat"){
            if(foo.survey.rev$site[1] %in% c(5485)){                                                                                      # Special regs
              if(sum(foo.survey.rev[ind,"harvest.num"])>0){
                compliance.data$channel_cat.bag.limit<-1
              }
            }
            else{ # No special regs
              if(sum(foo.survey.rev[ind,"harvest.num"])>(5*foo.survey.rev$num.anglers[1])){
                compliance.data$channel_cat.bag.limit<-1
              }
            }       
          }
          ############### blue catfish     ############################################
          if(uniq.regs[j]=="blue_cat"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(1*foo.survey.rev$num.anglers[1])){
              compliance.data$blue_cat.bag.limit<-1
            }
          }       
          
          ############# flathead catfish #############################################
          if(uniq.regs[j]=="flathead"){
            if(foo.survey.rev$site[1] %in% c(5110,5485)){                                             # Special regs
              if(sum(foo.survey.rev[ind,"harvest.num"])>0){
                compliance.data$flathead.bag.limit<-1
              }
            }
            else{        # No special regs
              if(sum(foo.survey.rev[ind,"harvest.num"])>(5*foo.survey.rev$num.anglers[1])){
                compliance.data$flathead.bag.limit<-1
              }
              if(length(which(foo.survey.rev[ind,length.id]>r.30inch & foo.survey.rev[ind,length.id]>0))>1){
                compliance.data$flathead.oneover<-1
              }
            }
          }
          ############### Panfish regs ###############################################
          if(uniq.regs[j]=="panfish"){
            if(foo.survey.rev$site[1] %in% c(5110,6925)){
              if(foo.survey.rev$site[1] %in% c(5110) & any(foo.survey.rev$spp.harvest %in% c(780,785,790,795))){    #Special regs for BO crappie <10inch
                crappie<-which(foo.survey.rev$spp.harvest %in% c(780,785,790,795))
                if(any(which(foo.survey.rev[crappie,length.id]<r.10inch &
                  foo.survey.rev[crappie,length.id]>0))){
                  compliance.data$crappie.under.length<-1
                }
              }
              if(foo.survey.rev$site[1] == 6925 & any(foo.survey.rev$spp.harvest %in% c(780,785,790,795))){
                crappie<-which(foo.survey.rev$spp.harvest %in% c(780,785,790,795))
                if(any(which(foo.survey.rev[crappie,length.id]<r.10inch &
                  foo.survey.rev[crappie,length.id]>0))){
                  compliance.data$crappie.under.length<-1
                }
              }
              }else{
                if(sum(foo.survey.rev[ind,"harvest.num"])>(15*foo.survey.rev$num.anglers[1])){                  # No special regs
                  compliance.data$panfish.bag.limit<-1
                }
              }
          }
          ############ Black bass regs ############################################### 
          if(uniq.regs[j]=="micropterus"){
            if(foo.survey.rev$site[1] %in% c(3537,3710,5120,5130,5135,5265,5440,5475,5480,5485,5520,5740,5745)){
              if(foo.survey.rev$site[1] %in% c(3537,5120,5130,5135,5265,5440,5475,5480,5485,5520,5740,5745)){    # High Use Lakes
                if(any(which(foo.survey.rev[ind,length.id]<r.21inch & foo.survey.rev[ind,length.id]>0))){
                  compliance.data$micropterus.under.length<-1
                }
                if(sum(foo.survey.rev[ind,"harvest.num"])>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$micropterus.bag.limit<-1
                }
              }
              if(foo.survey.rev$site[1] %in% c(3710)){    # High Use Lakes
                if(any(which(foo.survey.rev[ind,length.id]<r.12inch & foo.survey.rev[ind,length.id]>0))){
                  compliance.data$micropterus.under.length<-1
                }
                if(sum(foo.survey.rev[ind,"harvest.num"])>(5*foo.survey.rev$num.anglers[1])){
                  compliance.data$micropterus.bag.limit<-1
                }
              }
              
              }else{
                if(sum(foo.survey.rev[ind,"harvest.num"])>(4*foo.survey.rev$num.anglers[1])){
                  compliance.data$micropterus.bag.limit<-1
                }
                if(length(which(foo.survey.rev[ind,length.id]>r.21inch))>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$micropterus.oneover<-1
                }
                if(any(which(foo.survey.rev[ind,length.id]<r.15inch &
                  foo.survey.rev[ind,length.id]>0))){
                  compliance.data$micropterus.under.length<-1
                }
                }

          }
          ########### Clam regs ################################################### 
          if(uniq.regs[j]=="clam"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(10*foo.survey.rev$num.anglers[1])){
              compliance.data$clam.bag.limit<-1
            }
          }
          ############ Bullfrog regs ##############################################
          if(uniq.regs[j]=="bullfrog"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(8*foo.survey.rev$num.anglers[1])){
              compliance.data$bullfrog.bag.limit<-1
            }
          }
          ############ Snapping turtle regs #######################################
          if(uniq.regs[j]=="snapper"){
            #if special regs lake will go here
            if(sum(foo.survey.rev[ind,"harvest.num"])>(10*foo.survey.rev$num.anglers[1])){
              compliance.data$snapper.bag.limit<-1
            }
          }
          ############# Sander regs ###############################################
          if(uniq.regs[j]=="sander"){                               # walleye regulations
            if(foo.survey.rev$site[1]%in% c(2780,2740,5110,6915,6925)){  #Lakes with special regs
              
              ## Branched Oak Spec regs              
              if(foo.survey.rev$site[1] %in% c(5110)){       # Special regulations for Branched Oak
                if(any(which(foo.survey.rev[ind,length.id]<r.22inch &           # under length viol
                  foo.survey.rev[ind,length.id]>0))){
                  compliance.data$sander.under.length<-1
                }
                if(sum(foo.survey.rev[ind,"harvest.num"])>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$sander.bag.limit<-1
                }
                
                ##Calmus, Harlan, and Merrit Spec regs
                if(foo.survey.rev$site[1] %in% c(2780,2740,6915)){       # Special regulations for Calmus, Harlan, and Merrit
                  foo.length.a<-which(foo.survey.rev[ind,length.id]>=r.15inch &    # fish that are between 15-18 inches
                    foo.survey.rev[ind,length.id]<=r.18inch)
                  foo.length.b<-which(foo.survey.rev[ind,length.id]>r.18inch)     # fish greater than 18
                  foo.length.c<-which(foo.survey.rev[ind,length.id]>r.22inch)     # fish greater than 22
                  
                  if(any(which(foo.survey.rev[ind,length.id]<r.15inch &           # under length viol
                    foo.survey.rev[ind,length.id]>0))){
                    compliance.data$sander.under.length<-1
                  }
                  if(length(foo.length.a)>(1*foo.survey.rev$num.anglers[1])){  # more than one fish between 15-18
                    compliance.data$sander.more15_18<-1
                  }
                  if(length(foo.length.c)>(1*foo.survey.rev$num.anglers[1])){ # more than one fish > 22
                    compliance.data$sander.more22<-1
                  }
                  if(sum(foo.survey.rev[ind,"harvest.num"])>(4*foo.survey.rev$num.anglers[1])){  # bag limit of 4
                    compliance.data$sander.bag.limit<-1
                  }
                }
                #Sherman Spec regs
                if(foo.survey.rev$site[1] == 6925){                 # Special regulations for Sherman
                  foo.length.a<-which(foo.survey.rev[ind,length.id]>=r.15inch &    # fish that are between 15-20 inches
                    foo.survey.rev[ind,length.id]<=r.20inch)
                  foo.length.b<-which(foo.survey.rev[ind,length.id]>r.28inch)     # fish greater than 28
                  
                  if(any(which(foo.survey.rev[ind,length.id]<r.15inch &           # less than 15 (381)
                    foo.survey.rev[ind,length.id]>0))){
                    compliance.data$sander.under.length<-1
                  }
                  if(length(foo.length.a)>(2*foo.survey.rev$num.anglers[1])){  # more than two fish between 15-20
                    compliance.data$sander.more15_20<-1
                  }
                  if(any(which(foo.survey.rev[ind,length.id]>r.20inch &           # slot limit (381)
                    foo.survey.rev[ind,length.id]<r.28inch))){
                    compliance.data$sander.slot<-1
                  }
                  if(length(foo.length.b)>(1*foo.survey.rev$num.anglers[1])){  # more than one fish >28
                    compliance.data$sander.more28<-1
                  }
                  if(sum(foo.survey.rev[ind,"harvest.num"])>(3*foo.survey.rev$num.anglers[1])){  # bag limit of 3
                    compliance.data$sander.bag.limit<-1
                  }
                }
                
                ####
                } 
              }else{
                if(sum(foo.survey.rev[ind,"harvest.num"])>(4*foo.survey.rev$num.anglers[1])){
                  compliance.data$sander.bag.limit<-1
                }
                if(any(which(foo.survey.rev[ind,length.id]<r.15inch &
                  foo.survey.rev[ind,length.id]>0))){
                  compliance.data$sander.under.length<-1
                }
                if(length(which(foo.survey.rev[ind,length.id]>r.22inch))>(1*foo.survey.rev$num.anglers[1])){
                  compliance.data$sander.over.length<-1
                }
                }

          }
          
          ######################### End of regs ########################################
        }
      }
    }
    violations<-rbind(violations,compliance.data) 
  }
  violations<-as.data.frame(violations)
  violations[,"int.name"]<-as.factor(violations[,"int.name"])
  violations[,"site"]<-as.factor(violations[,"site"])
  if(doWrite==1){
    write.csv(violations,paste("violations_",format(Sys.Date(), "%d%b%Y"),".csv",sep=""))
  }
  
  return(violations)
}