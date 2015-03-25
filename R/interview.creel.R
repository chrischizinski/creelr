'interview.creel'<-function(foo.inc){
    line.type<-substring(foo.inc,1,1)  # need to implement an error check in the future
    site<-substring(foo.inc,2,5)
    section<-substring(foo.inc,6,7)
    month<-sprintf("%02d",as.numeric(substring(foo.inc,8,9)))
    day<-sprintf("%02d",as.numeric(substring(foo.inc,10,11)))
    year<-as.numeric(substring(foo.inc,12,13))+2000
    Date<-as.Date(paste(year,month,day,sep='-'))
    num.anglers<-as.numeric(substring(foo.inc,14,15))
    angler.type<-substring(foo.inc,16,16)
    start.time<-paste(substring(foo.inc,17,18),substring(foo.inc,19,20),sep=':')
    interv.time<-paste(substring(foo.inc,21,22),substring(foo.inc,23,24),sep=':')
    hours<-as.numeric(substring(foo.inc,25,26))
    minutes<-as.numeric(substring(foo.inc,27,28))/60
    time.diff_hr<-hours+minutes
    trip.type<- substring(foo.inc,29,29)
    sp.sought<-substring(foo.inc,30,32)
    success<- substring(foo.inc,33,33)
    state<- substring(foo.inc,34,35)
    county<-substring(foo.inc,36,37)


    new.line<-data.frame(site,section,Date,num.anglers,angler.type,start.time,interv.time,
                   time.diff_hr, trip.type,sp.sought,success,state,county)
    return(new.line)
}