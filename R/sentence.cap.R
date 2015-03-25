sentence.cap<-function(x){
  require(stringr)
  s<-str_split_fixed(x," ",n=2)
  s[,2]<-tolower(s[,2])
  s<-apply(s,1,paste,collapse=" ")
  s<-trim(s)
  return(s)
}