"creel.process"<-function(datafiles,dotrack=TRUE){
  for(j in 1:length(datafiles)){
      connect<-file(paste(datafiles[j]),'r')  #connect to the data file
      foo<-readLines(connect, n=-1) #read the data as char. strings toa data.set
      close(connect)  #close the connection to the file

      num.fish.storage<-6 # number of fish for the storage

      data.file.survey<-NULL         #Null dataset to store new data
      data.file.count<-NULL         #Null dataset to store new data
      line.stack<-NULL            #Null vector to store line.types
      

      temp.harvest<-as.data.frame(matrix(0,num.fish.storage,18))# reset the temporary storage for harvest
      temp.release<-as.data.frame(matrix(0,num.fish.storage,30)) # reset the temporary storage for release 
      temp.supplement<-as.data.frame(matrix(0,num.fish.storage,13)) # reset the temporary storage for supplement 

      harvest.names<-c("spp.harvest","harvest.num","harvest.mass","hL1","hL2","hL3",
                "hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12","hL13",
                "hL14","hL15")
                
      release.names<-c("spp.release","release.num","r.lt.125","r.125","r.150",
                 "r.175","r.200","r.230","r.255","r.280","r.305","r.330",
                 "r.355","r.380","r.405","r.430","r.455","r.480","r.510",
                 "r.530","r.560","r.585","r.610","r.635","r.660","r.685",
                 "r.710","r.735_815","r.840_915","r.gt.915")
      
      supplement.names<-c("s1","s2","s3","s4",
                          "s5","s6","s7","s8","s9","s10","s11","s12","s13")
      
      names(temp.harvest)<-harvest.names
      names(temp.release)<-release.names
      names(temp.supplement)<-supplement.names

      surv.data<-data.frame(matrix(0,1,75))
      names(surv.data)<-c("int.name","site","section","Date","num.anglers","angler.type","start.time",  
                        "interv.time","time.diff_hr","trip.type","sp.sought","success","state",       
                        "county","spp.harvest","harvest.num","harvest.mass","hL1","hL2","hL3",
                        "hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12","hL13",
                        "hL14","hL15","spp.release","release.num","r.lt.125","r.125","r.150",
                        "r.175","r.200","r.230","r.255","r.280","r.305","r.330",
                        "r.355","r.380","r.405","r.430","r.455","r.480","r.510",
                        "r.530","r.560","r.585","r.610","r.635","r.660","r.685",
                        "r.710","r.735_815","r.840_915","r.gt.915","s1","s2","s3","s4",
                          "s5","s6","s7","s8","s9","s10","s11","s12","s13")
      
      int<-1 # interview counter, used in allocating harvest and release data to correct interview
      H<-1 
      R<-1 
      
      for(i in 1:length(foo) ){        # a loop to run through each line of the data file
        if(dotrack==TRUE){print(paste("Currently processing",datafiles[j],'Line number', i, sep=' '))}
        line.type<-substring(foo[i],1,1)
        if(line.type=='I'){
          line.stack<-rbind(line.stack,line.type) #track the type of file, used in error checking
    
          if(int>1){
            data.file.survey<-rbind(data.file.survey,surv.data)      # after the first iteration save the previous data
            H<-1                                                     #reset the survey and release counts
            R<-1
  
          }
      
        survey.line<-interview.creel(foo[i])   # extract the information using the above function
        int.name<-paste(survey.line$site,survey.line$Date,int,sep=".")  #create an individual survey name
        survey.line<-data.frame(int.name,survey.line)   # add the survey name into the data set
        survey.block<-survey.line[rep(seq.int(1,nrow(survey.line)), num.fish.storage), 1:ncol(survey.line)]   #replicate the survey information
        rownames(survey.block)<-NULL       #erase the row names created in the previous step
        surv.data<-data.frame(survey.block,temp.harvest,temp.release,temp.supplement)    # create the storage block of data 
        
        int<-int+1          # increase the interview
        }
      if(line.type=='H'){
        line.stack<-rbind(line.stack,line.type)
        surv.data[H,c("spp.harvest","harvest.num","harvest.mass","hL1","hL2","hL3",
                      "hL4","hL5","hL6","hL7","hL8","hL9","hL10","hL11","hL12","hL13",
                      "hL14","hL15")]<-harvest.creel(foo[i])
        H<-H+1
      }
      if(line.type=='R'){
        line.stack<-rbind(line.stack,line.type)
        surv.data[R,c("spp.release","release.num","r.lt.125","r.125","r.150",
                      "r.175","r.200","r.230","r.255","r.280","r.305","r.330",
                      "r.355","r.380","r.405","r.430","r.455","r.480","r.510",
                      "r.530","r.560","r.585","r.610","r.635","r.660","r.685",
                      "r.710","r.735_815","r.840_915","r.gt.915")]<-release.creel(foo[i])
        R<-R+1
      }
      if(line.type=='S'){
          line.stack<-rbind(line.stack,line.type)
          suppl<-supplement.creel(foo[i])
          suppl[suppl==""]<-NA
          
          
          surv.data[1:nrow(surv.data),paste(supplement.names)]<-suppl[rep(seq.int(1,nrow(survey.line)), num.fish.storage),]

      }  
        
        #print(surv.data)
        
      if(line.type=='C'){
        line.stack<-rbind(line.stack,line.type)
        creel.line<-count.creel(foo[i])
        data.file.count<-rbind(data.file.count,creel.line) # bind the data from the extraction to the new data file
      }
  
      if(i==length(foo)){
        data.file.survey<-rbind(data.file.survey,surv.data)      # make sure data is included at the last line
      }
    }  

  dup.ind<-duplicated(data.file.survey$int.name)
  data.file.survey.reva<-data.file.survey[!dup.ind,] # remove all the duplicates

  dup.data.file.survey<-data.file.survey[dup.ind,]   # duplicated dataset
  dup.ind<-which(dup.data.file.survey$spp.harvest != "0" |dup.data.file.survey$spp.release != "0")

  data.file.survey.revb<-rbind(data.file.survey.reva,dup.data.file.survey[dup.ind,])
  data.file.survey.rev<-data.file.survey.revb[order(data.file.survey.revb$int.name),]  
  }  
  out<-list(survey=data.file.survey.rev,count=data.file.count)
  return(out)
  }