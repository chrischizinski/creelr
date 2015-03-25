
'match_creel_code'<-function(dataset,colz="species",code_type=c("fish.codes","county.codes","state.codes"),lib.loc){
  load(paste(lib.loc))
  if(length(colz)!=length(code_type)) stop("Columns do not match the code_types")
  dataset.rev<-dataset
  for(i in length(colz)){
   if(code_type[i]=="fish.codes"){
      dataset.rev[,colz[i]]<-as.numeric(as.character(dataset.rev[,colz[i]]))
      y.ind<-which(dataset.rev[,colz[i]]%in%fishcodes$Code)
      f.ind<-match(dataset.rev[y.ind,colz[i]], fishcodes$Code)
      dataset.rev[y.ind,colz[i]]<- trim(paste(fishcodes$Name[f.ind]))
   }
   if(code_type[i]=="county.codes"){
    dataset.rev[,colz[i]]<-as.numeric(as.character(dataset.rev[,colz[i]]))
    dataset.rev[,colz[i]]<-trim(as.character(countycodes$County[match(dataset.rev[,colz[i]],countycodes$Code)]))
   }
   if(code_type[i]=="state.codes"){
      dataset.rev[,colz[i]]<-as.numeric(as.character(dataset.rev[,colz[i]]))
      y.ind<-which(dataset.rev[,colz[i]]%in%statecodes$Code)
      s.ind<-match(dataset.rev[y.ind,colz[i]], statecodes$Code) 
      dataset.rev[y.ind,colz[i]]<- trim(paste(statecodes$State[s.ind]))
   }
  }
  return(dataset.rev)
}
