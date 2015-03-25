"count.creel"<-function(foo.inc){
    line.type<-substring(foo.inc,1,1)  # need to implement an error check in the future
    site<-substring(foo.inc,2,5)
    section<-substring(foo.inc,6,7)
    month<-sprintf("%02d",as.numeric(substring(foo.inc,8,9)))
    day<-sprintf("%02d",as.numeric(substring(foo.inc,10,11)))
    year<-as.numeric(substring(foo.inc,12,13))+2000
    Date<-as.Date(paste(year,month,day,sep='-'))
    start.point<-substring(foo.inc,14,15)
    clerk.init<-substring(foo.inc,16,18)
    suppl<-substring(foo.inc,19,23)
    user<-substring(foo.inc,24,28)

    time.1<-paste(substring(foo.inc,29,30),substring(foo.inc,31,32),sep=":")
    if(time.1 == "  :  "){time.1<-NA
    } else{
    time.1<-strptime(paste(Date,time.1,sep=" "),"%Y-%m-%d %H:%M")}
    bank_ang.1<-as.numeric(substring(foo.inc,33,36))
    boats.1<-as.numeric(substring(foo.inc,37,40))
    boats_ang.1<-as.numeric(substring(foo.inc,41,44))
    spr.angl.1<-0
    bow.angl.1<-0
    spec.angl.1<-0
    non_ang.1<-as.numeric(substring(foo.inc,45,48))
    weather.1<-substring(foo.inc,49,49)
    

    time.2<-paste(substring(foo.inc,50,51),substring(foo.inc,52,53),sep=":")
    if(time.2 == "  :  "){time.2<-NA
    } else{
    time.2<-strptime(paste(Date,time.2,sep=" "),"%Y-%m-%d %H:%M")}
    bank_ang.2<-as.numeric(substring(foo.inc,54,57))
    boats.2<-as.numeric(substring(foo.inc,58,61))
    boats_ang.2<-as.numeric(substring(foo.inc,62,65))
    spr.angl.2<-NA
    bow.angl.2<-NA
    spec.angl.2<-NA
    non_ang.2<-as.numeric(substring(foo.inc,66,69))
    weather.2<-substring(foo.inc,70,70)

    time.3<-paste(substring(foo.inc,71,72),substring(foo.inc,73,74),sep=":")
    if(time.3 == "  :  "){time.3<-NA
    } else{
    time.3<-strptime(paste(Date,time.3,sep=" "),"%Y-%m-%d %H:%M")}
    bank_ang.3<-as.numeric(substring(foo.inc,75,78))
    boats.3<-as.numeric(substring(foo.inc,79,82))
    boats_ang.3<-as.numeric(substring(foo.inc,83,86))
    spr.angl.3<-NA
    bow.angl.3<-NA
    spec.angl.3<-NA
    non_ang.3<-as.numeric(substring(foo.inc,87,90))
    weather.3<-substring(foo.inc,91,91)

    time.4<-paste(substring(foo.inc,92,93),substring(foo.inc,94,95),sep=":")
    if(time.4 == "  :  "){time.4<-NA
    } else{
    time.4<-strptime(paste(Date,time.4,sep=" "),"%Y-%m-%d %H:%M")}
    bank_ang.4<-as.numeric(substring(foo.inc,96,99))
    boats.4<-as.numeric(substring(foo.inc,100,103))
    boats_ang.4<-as.numeric(substring(foo.inc,104,107))
    spr.angl.4<-NA
    bow.angl.4<-NA
    spec.angl.4<-NA
    non_ang.4<-as.numeric(substring(foo.inc,108,111))
    weather.4<-substring(foo.inc,112,112)

    temp.f<-as.numeric(substring(foo.inc,113,115))
    water.lvl<-substring(foo.inc,116,116)
    turbid<-as.numeric(substring(foo.inc,117,120))
    ice<-substring(foo.inc,121,121)
    veg<-substring(foo.inc,122,122)
    veg.type<-substring(foo.inc,123,123)

    count.line<-data.frame(site,section,month,day,year,Date,start.point,
               suppl,user,
               time.1,bank_ang.1,boats.1,boats_ang.1,spr.angl.1,bow.angl.1,spec.angl.1,non_ang.1,weather.1,
               time.2,bank_ang.2,boats.2,boats_ang.2,spr.angl.2,bow.angl.2,spec.angl.2,non_ang.2,weather.2,
               time.3,bank_ang.3,boats.3,boats_ang.3,spr.angl.3,bow.angl.3,spec.angl.3,non_ang.3,weather.3,
               time.4,bank_ang.4,boats.4,boats_ang.4,spr.angl.4,bow.angl.4,spec.angl.4,non_ang.4,weather.4,
               temp.f,water.lvl,turbid,ice,veg,veg.type,clerk.init)

    return(count.line)
}
