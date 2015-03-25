'check.installed.packages'<-function(.package){
  if(!all(.package%in%rownames(installed.packages()))){
    install.packages(paste(.package))
  }else(print("All packages are installed"))
}