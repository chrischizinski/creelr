'monthly.effort'<-function(surveyharvest,surveydata,angler_effort,start_endDates,subs_MH_ANGC=""){
  require(reshape)
  require(plyr)
  surveydata[,c(15:31,33:61)]<-apply(surveydata[,c(15:31,33:61)],c(1,2),as.numeric) # make sure the data is numeric

  surveyharvest$Month<-factor(surveyharvest$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  surveydata$Month<-factor(surveydata$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  angler_effort$Month<-factor(angler_effort$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# determine weekday or weekends in the sampling period
  dayz<-seq(start.date,end.date,by=1)
  day_names<-weekdays(dayz,abbreviate=TRUE)
  day_type<-ifelse(day_names%in%c('Sat','Sun'),'weekend','weekday') # weekdays or weekends
  day_type[which(dayz%in%holidays)]<-'weekend'  # account for the holidays

  days_sampling_period<-data.frame(dayz,Month=months(dayz,abbreviate=TRUE),day_names,day_type=factor(day_type),count=1)  
  days_sampling_period$Month<-factor(days_sampling_period$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  
  id_vars<-c("Month", "day_type")
  measure_vars<-c("count")
  days_sampling_period.m<-melt(days_sampling_period,id.vars=id_vars,measure.vars=measure_vars)
  day_type_month<-cast(days_sampling_period.m,Month + day_type~variable,length)
  day_month<-cast(days_sampling_period.m,Month~variable,length)

#Calculate weighing factors
  day.comb<-merge(day_type_month,day_month,by=c("Month"))
  day.comb$day_type.weight<-day.comb$count.x/day.comb$count.y
  id_vars<-c("Month", "day_type")
  measure_vars<-c("TotalAngEffort","BankAngEffort","BoatAngEffort","SprAngEffort","BowAngEffort","SpecAngEffort")
  'sderr'<-function(x){sd(x)/sqrt(length(x))}      # define standard error function
#m_effort<-melt(angler_effort,id.vars=id_vars,measure.vars=measure_vars) # reshape the data

## added this to correct errors with NA's that dustin was having  
  m_effort<-melt(angler_effort,id.vars=id_vars,measure.vars=measure_vars,na.rm=TRUE) # reshape the data
  #m_effort_stats<-cast(m_effort, Month + day_type~variable,function(x) c(mean= mean(x), var=var(x)))
  m_effort_stats<-ddply(m_effort,.(Month,day_type,variable),summarize,mean=mean(value),var=var(value))
  m_effort_stats<-reshape(m_effort_stats,v.names=c("mean","var"),timevar="variable", idvar=c("Month","day_type"),direction="wide")
  names(m_effort_stats)<-c("Month","day_type","TotalAngEffort_mean","TotalAngEffort_var","BankAngEffort_mean","BankAngEffort_var",
                           "BoatAngEffort_mean","BoatAngEffort_var","SprAngEffort_mean","SprAngEffort_var",
                           "BowAngEffort_mean","BowAngEffort_var","SpecAngEffort_mean","SpecAngEffort_var")
  #m_effort_stats<-cast(m_effort, Month + day_type~variable,function(x) c(mean= mean(x), var=var(x)))
  
  # Calculate the number of days surveyed by day type
  survey_days.data<-data.frame(angler_effort[,c("Month","day_type")])   # Based on count data
  survey_days.data$count<-1
  survey_days.m<-melt(survey_days.data,id.vars=id_vars)
  survey.day_type_m<-cast(survey_days.m,Month+day_type~variable,sum) # total number of weekdays and weekends in the sampling period
  names(survey.day_type_m)[3]<-"surv.count"

  m_effort_stats.revb<-merge(m_effort_stats,day.comb, by=c("Month","day_type"))
  m_effort_stats.revb$day_type.weight<-m_effort_stats.revb$count.x/m_effort_stats.revb$count.y
  m_effort_stats.rev<-merge(m_effort_stats.revb,survey.day_type_m, by=c("Month","day_type"))
    
  survey.dayz.melt<-melt(survey.day_type_m,id.vars=indvars)   # Calculation of confidence intervals
  survey.dayz.gt<-cast(survey.dayz.melt,...~day_type)
  survey.dayz.gt$nh_1<-with(survey.dayz.gt,ifelse(weekday>weekend,weekend-1,weekday-1))
  survey.dayz.gt$sumnh<-survey.dayz.gt[,3]+survey.dayz.gt[,4]
  survey.dayz.gt$DF<- round(((survey.dayz.gt$sumnh - survey.dayz.gt$nh_1)/2),0)+survey.dayz.gt$nh_1
  survey.dayz.gt$DF<-ifelse(survey.dayz.gt$DF>30,30,survey.dayz.gt$DF)
  survey.dayz.gt$T_CL<-0
  survey.dayz.gt$T_CL[which(survey.dayz.gt$DF>0 & !is.na(survey.dayz.gt$DF))]<- qt(p=.95, df = survey.dayz.gt$DF[which(survey.dayz.gt$DF>0 & !is.na(survey.dayz.gt$DF))]) 
  
  mean_daily_effort_all<-NULL
  mean_daily_effort_all$Month<-m_effort_stats.rev$Month
  # Mean daily effort
  mean_daily_effort_all$mday.all<-with(m_effort_stats.rev,TotalAngEffort_mean*day_type.weight)
  mean_daily_effort_all$mday.bnk<-with(m_effort_stats.rev,BankAngEffort_mean*day_type.weight)
  mean_daily_effort_all$mday.bot<-with(m_effort_stats.rev,BoatAngEffort_mean*day_type.weight)
  mean_daily_effort_all$mday.spr<-with(m_effort_stats.rev,SprAngEffort_mean*day_type.weight)
  mean_daily_effort_all$mday.bow<-with(m_effort_stats.rev,BowAngEffort_mean*day_type.weight)
  mean_daily_effort_all$mday.spc<-with(m_effort_stats.rev,SpecAngEffort_mean*day_type.weight)
  # Variance daily effort
  mean_daily_effort_all$vday.all<-with(m_effort_stats.rev,(day_type.weight^2)*(TotalAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(TotalAngEffort_var/count.y))
  mean_daily_effort_all$vday.bnk<-with(m_effort_stats.rev,(day_type.weight^2)*(BankAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(BankAngEffort_var/count.y))
  mean_daily_effort_all$vday.bot<-with(m_effort_stats.rev,(day_type.weight^2)*(BoatAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(BoatAngEffort_var/count.y))
  mean_daily_effort_all$vday.spr<-with(m_effort_stats.rev,(day_type.weight^2)*(SprAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(SprAngEffort_var/count.y))
  mean_daily_effort_all$vday.bow<-with(m_effort_stats.rev,(day_type.weight^2)*(BowAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(BowAngEffort_var/count.y))
  mean_daily_effort_all$vday.spc<-with(m_effort_stats.rev,(day_type.weight^2)*(SpecAngEffort_var/surv.count)) - with(m_effort_stats.rev,day_type.weight*(SpecAngEffort_var/count.y))
  #mean_daily_effort_all<-as.data.frame(mean_daily_effort_all)
  #mean_daily_effort_all<-recast(as.data.frame(mean_daily_effort_all),id.var="Month", Month~variable,sum)
  mean_daily_effort_all<-ddply(as.data.frame(mean_daily_effort_all),.(Month),summarise,mday.all=sum(mday.all),mday.bnk=sum(mday.bnk),mday.bot=sum(mday.bot),
                               mday.spr=sum(mday.spr),mday.bow=sum(mday.bow),mday.spc=sum(mday.spc), 
                               vday.all=sum(vday.all),vday.bnk=sum(vday.bnk),vday.bot=sum(vday.bot),
                               vday.spr=sum(vday.spr),vday.bow=sum(vday.bow),vday.spc=sum(vday.spc))
  
  # Monthly effort
  monthly_effort_all<-NULL
  monthly_effort_all$Month<-mean_daily_effort_all$Month
  monthly_effort_all<-as.data.frame(monthly_effort_all)
  if(nrow(subset(surveyharvest,surveyharvest$trip.type==1))==0){
  MH_ANGC<-data.frame(Month=unique(days_sampling_period$Month),MH_ANGC=subs_MH_ANGC)
  
  }else{ 
    MH_ANGC<-ddply(subset(surveyharvest,surveyharvest$trip.type==1),.(Month),summarize,MH_ANGC=mean(time.diff_hr))
    
#     MH_ANGC<- recast(subset(surveyharvest,surveyharvest$trip.type==1),id.var="Month",measure.var="time.diff_hr",Month~variable,mean)
#     names(MH_ANGC)[2]<-"MH_ANGC"
  }
  
  # If a month does not have any complete trips then will substitue in the grand mean. 
#   if(length(unique(days_sampling_period$Month)[!unique(days_sampling_period$Month)%in% MH_ANGC$Month])>0){
#     MH_ANGC<-rbind(MH_ANGC,data.frame(Month=unique(days_sampling_period$Month)[!unique(days_sampling_period$Month)%in% MH_ANGC$Month],MH_ANGC=mean(MH_ANGC$MH_ANGC)))
#   }
  
  if(!all(as.character(unique(days_sampling_period$Month)) %in% as.character(MH_ANGC$Month[!is.na(MH_ANGC$MH_ANGC)]))){
    MH_ANGC.a<-MH_ANGC[!is.na(MH_ANGC$MH_ANGC),]
    mean.time<-mean(MH_ANGC.a$MH_ANGC)
    missing.months<-as.character(unique(days_sampling_period$Month))[which(!as.character(unique(days_sampling_period$Month)) %in% as.character(MH_ANGC$Month[!is.na(MH_ANGC$MH_ANGC)]))]
    MH_ANGC<-rbind(MH_ANGC.a,data.frame(Month=missing.months,MH_ANGC=mean.time))
    }
  
  
  
  
  monthly_effort_all<-merge(MH_ANGC,monthly_effort_all,by="Month")
  monthly_effort_all<-monthly_effort_all[order(monthly_effort_all$Month),]
  monthly_effort_all<- data.frame(monthly_effort_all,
                      mean_daily_effort_all[,c("mday.all","mday.bnk","mday.bot","mday.spr","mday.bow","mday.spc")]*m_effort_stats.rev$count.y[match(monthly_effort_all$Month,m_effort_stats.rev$Month)],
                      mean_daily_effort_all[,c("vday.all","vday.bnk","vday.bot","vday.spr","vday.bow","vday.spc")]*(m_effort_stats.rev$count.y[match(monthly_effort_all$Month,m_effort_stats.rev$Month)])^2)
  monthly_effort_namez<-names(monthly_effort_all)
  monthly_effort_namez<-gsub("mday","mmon",monthly_effort_namez)
  monthly_effort_namez<-gsub("vday","vmon",monthly_effort_namez)
  names(monthly_effort_all)<-monthly_effort_namez
  #standard error daily effort
  monthly_effort_all$semon.all<-sqrt(monthly_effort_all$vmon.all)
  monthly_effort_all$semon.bnk<-sqrt(monthly_effort_all$vmon.bnk)
  monthly_effort_all$semon.bot<-sqrt(monthly_effort_all$vmon.bot)
  monthly_effort_all$semon.spr<-sqrt(monthly_effort_all$vmon.spr)
  monthly_effort_all$semon.bow<-sqrt(monthly_effort_all$vmon.bow)
  monthly_effort_all$semon.spc<-sqrt(monthly_effort_all$vmon.spc)

  #RSE daily effort
  monthly_effort_all$rsemon.all<-with(monthly_effort_all,ifelse(is.na(mmon.all)|mmon.all==0,NA,((semon.all/mmon.all)*100)))
  monthly_effort_all$rsemon.bnk<-with(monthly_effort_all,ifelse(is.na(mmon.bnk)|mmon.bnk==0,NA,((semon.bnk/mmon.bnk)*100)))
  monthly_effort_all$rsemon.bot<-with(monthly_effort_all,ifelse(is.na(mmon.bot)|mmon.bot==0,NA,((semon.bot/mmon.bot)*100)))
  monthly_effort_all$rsemon.spr<-with(monthly_effort_all,ifelse(is.na(mmon.spr)|mmon.spr==0,NA,((semon.spr/mmon.spr)*100)))
  monthly_effort_all$rsemon.bow<-with(monthly_effort_all,ifelse(is.na(mmon.bow)|mmon.bow==0,NA,((semon.bow/mmon.bow)*100)))
  monthly_effort_all$rsemon.spc<-with(monthly_effort_all,ifelse(is.na(mmon.spc)|mmon.spc==0,NA,((semon.spc/mmon.spc)*100)))
  #90% Confidence Intervals
  monthly_effort_all$DF<- survey.dayz.gt$DF[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.all<- monthly_effort_all$semon.all*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.bnk<- monthly_effort_all$semon.bnk*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.bot<- monthly_effort_all$semon.bot*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.spr<- monthly_effort_all$semon.spr*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.bow<- monthly_effort_all$semon.bow*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  monthly_effort_all$CIday.spc<- monthly_effort_all$semon.spc*survey.dayz.gt$T_CL[match(monthly_effort_all$Month,survey.dayz.gt$Month)]
  
  #Projected anglers
  monthly_effort_all$proj_anglers.all<-with(monthly_effort_all,mmon.all/MH_ANGC)
  monthly_effort_all$proj_anglers.bnk<-with(monthly_effort_all,mmon.bnk/MH_ANGC)
  monthly_effort_all$proj_anglers.bot<-with(monthly_effort_all,mmon.bot/MH_ANGC)
  monthly_effort_all$proj_anglers.spr<-with(monthly_effort_all,mmon.spr/MH_ANGC)
  monthly_effort_all$proj_anglers.bow<-with(monthly_effort_all,mmon.bow/MH_ANGC)
  monthly_effort_all$proj_anglers.spc<-with(monthly_effort_all,mmon.spc/MH_ANGC)
  
  #Projected anglers
  monthly_effort_all$proj_anglers.all.var<-with(monthly_effort_all,vmon.all*(1/MH_ANGC)^2)
  monthly_effort_all$proj_anglers.bnk.var<-with(monthly_effort_all,vmon.bnk*(1/MH_ANGC)^2)
  monthly_effort_all$proj_anglers.bot.var<-with(monthly_effort_all,vmon.bot*(1/MH_ANGC)^2)
  monthly_effort_all$proj_anglers.spr.var<-with(monthly_effort_all,vmon.spr*(1/MH_ANGC)^2)
  monthly_effort_all$proj_anglers.bow.var<-with(monthly_effort_all,vmon.bow*(1/MH_ANGC)^2)
  monthly_effort_all$proj_anglers.spc.var<-with(monthly_effort_all,vmon.spc*(1/MH_ANGC)^2)
  
  ################# Yearly estimates 
  measurevars<-c("mmon.all","mmon.bnk","mmon.bot","vmon.all","vmon.bnk","vmon.bot",
                 "mmon.spr","mmon.bow","mmon.spc","vmon.spr","vmon.bow","vmon.spc",
                 "DF")


   yearly_effort_all<-data.frame(t(apply(monthly_effort_all[,measurevars],2,sum)))
   names(yearly_effort_all)<-gsub("mon","year",measurevars)
   #Yearly standard errors
   yearly_effort_all$seyear.all<-sqrt(yearly_effort_all$vyear.all)
   yearly_effort_all$seyear.bnk<-sqrt(yearly_effort_all$vyear.bnk)
   yearly_effort_all$seyear.bot<-sqrt(yearly_effort_all$vyear.bot)
   yearly_effort_all$seyear.spr<-sqrt(yearly_effort_all$vyear.spr)
   yearly_effort_all$seyear.bow<-sqrt(yearly_effort_all$vyear.bow)
   yearly_effort_all$seyear.spc<-sqrt(yearly_effort_all$vyear.spc)
   # RSE 
   yearly_effort_all$rseyear.all<-with(yearly_effort_all,ifelse(is.na(myear.all)|myear.all==0,NA,((seyear.all/myear.all)*100)))
   yearly_effort_all$rseyear.bnk<-with(yearly_effort_all,ifelse(is.na(myear.bnk)|myear.bnk==0,NA,((seyear.bnk/myear.bnk)*100)))
   yearly_effort_all$rseyear.bot<-with(yearly_effort_all,ifelse(is.na(myear.bot)|myear.bot==0,NA,((seyear.bnk/myear.bot)*100)))
   yearly_effort_all$rseyear.spr<-with(yearly_effort_all,ifelse(is.na(myear.spr)|myear.spr==0,NA,((seyear.spr/myear.spr)*100)))
   yearly_effort_all$rseyear.bow<-with(yearly_effort_all,ifelse(is.na(myear.bow)|myear.bow==0,NA,((seyear.bow/myear.bow)*100)))
   yearly_effort_all$rseyear.spc<-with(yearly_effort_all,ifelse(is.na(myear.spc)|myear.spc==0,NA,((seyear.spc/myear.spc)*100)))
   #confidence intervals
   yearly_effort_all$DF<-ifelse(yearly_effort_all$DF>30,30,yearly_effort_all$DF)
   yearly_effort_all$T_CL<-0
   yearly_effort_all$T_CL[which(yearly_effort_all$DF>0 & !is.na(yearly_effort_all$DF))]<- qt(p=.95, df = yearly_effort_all$DF[which(yearly_effort_all$DF>0 & !is.na(yearly_effort_all$DF))]) 
   yearly_effort_all$CIyear.all<-with(yearly_effort_all,seyear.all*T_CL)
   yearly_effort_all$CIyear.bnk<-with(yearly_effort_all,seyear.bnk*T_CL)
   yearly_effort_all$CIyear.bot<-with(yearly_effort_all,seyear.bot*T_CL)
   yearly_effort_all$CIyear.spr<-with(yearly_effort_all,seyear.spr*T_CL)
   yearly_effort_all$CIyear.bow<-with(yearly_effort_all,seyear.bow*T_CL)
   yearly_effort_all$CIyear.spc<-with(yearly_effort_all,seyear.spc*T_CL)
   #projected anglers 
   yearly_effort_all$proj_anglers.all<-yearly_effort_all$myear.all/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.bnk<-yearly_effort_all$myear.bnk/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.bot<-yearly_effort_all$myear.bot/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.spr<-yearly_effort_all$myear.spr/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.bow<-yearly_effort_all$myear.bow/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.spc<-yearly_effort_all$myear.spc/mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])
   yearly_effort_all$proj_anglers.all.var<-yearly_effort_all$vyear.all*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
   yearly_effort_all$proj_anglers.bnk.var<-yearly_effort_all$vyear.bnk*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
   yearly_effort_all$proj_anglers.bot.var<-yearly_effort_all$vyear.bot*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
   yearly_effort_all$proj_anglers.spr.var<-yearly_effort_all$vyear.spr*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
   yearly_effort_all$proj_anglers.bow.var<-yearly_effort_all$vyear.bow*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
   yearly_effort_all$proj_anglers.spc.var<-yearly_effort_all$vyear.spc*((1/(mean(surveyharvest$time.diff_hr[surveyharvest$trip.type==1])))^2)
  
  
  out<-list() # create an output list  
  angler.effort.out<-list(Daily.effort=mean_daily_effort_all,Monthly.effort=monthly_effort_all,Yearly.effort=yearly_effort_all)
  
  surveyharvest$Pressure<- round(surveyharvest$Pressure,digits=1)
  id.vars<-c("Month","weekday","day_type","angler.type","trip.type","sp.sought")
  measure.vars<-c("num.anglers","Pressure","harvest.num","release.num","ttlcat_number")
  surveyharvest.rev<-surveyharvest[!duplicated(surveyharvest$int.name),] # remove the duplicate interviews (needed to get proper numbers)
  
  # Table 6 in Brad Newcombs SAS code
  uniq.interv2.m<-melt(surveyharvest.rev, id.vars=id.vars, measure.vars=measure.vars)
  table6a<-list()     # information similar to Newcombs (SAS code) table 6a 
#   table6a$month<-cast(uniq.interv2.m, Month~variable,c(length,sum))[,c("Month","num.anglers_length","num.anglers_sum","Pressure_sum","harvest.num_sum","release.num_sum","ttlcat_number_sum")]
#   table6a$day_type<-cast(uniq.interv2.m, Month+day_type~variable,c(length,sum))[,c("Month","day_type","num.anglers_length","num.anglers_sum","Pressure_sum","harvest.num_sum","release.num_sum","ttlcat_number_sum")]
#   table6a$angler.type<-cast(uniq.interv2.m, Month+angler.type~variable,c(length,sum))[,c("Month","angler.type","num.anglers_length","num.anglers_sum","Pressure_sum","harvest.num_sum","release.num_sum","ttlcat_number_sum")]
#   table6a$trip.type<-cast(uniq.interv2.m, Month+trip.type~variable,c(length,sum))[,c("Month","trip.type","num.anglers_length","num.anglers_sum","Pressure_sum","harvest.num_sum","release.num_sum","ttlcat_number_sum")]
#   table6a$sp.sought<-cast(uniq.interv2.m, Month+sp.sought~variable,c(length,sum))[,c("Month","sp.sought","num.anglers_length","num.anglers_sum","Pressure_sum","harvest.num_sum","release.num_sum","ttlcat_number_sum")]
  
  table6a$month<-ddply(surveyharvest.rev, .(Month),summarize,
                        num.anglers_length=length(num.anglers),
                        num.anglers_sum=sum(num.anglers,na.rm=TRUE),
                        Pressure_sum=sum(Pressure,na.rm=TRUE),
                        harvest.num_sum=sum(harvest.num,na.rm=TRUE),
                        release.num_sum=sum(release.num,na.rm=TRUE),
                        ttlcat_number_sum=sum(ttlcat_number,na.rm=TRUE))
  
  table6a$day_type<-ddply(surveyharvest.rev, .(Month,day_type),summarize,
                       num.anglers_length=length(num.anglers),
                       num.anglers_sum=sum(num.anglers,na.rm=TRUE),
                       Pressure_sum=sum(Pressure,na.rm=TRUE),
                       harvest.num_sum=sum(harvest.num,na.rm=TRUE),
                       release.num_sum=sum(release.num,na.rm=TRUE),
                       ttlcat_number_sum=sum(ttlcat_number,na.rm=TRUE))
  
  table6a$angler.type<-ddply(surveyharvest.rev, .(Month,angler.type),summarize,
                          num.anglers_length=length(num.anglers),
                          num.anglers_sum=sum(num.anglers,na.rm=TRUE),
                          Pressure_sum=sum(Pressure,na.rm=TRUE),
                          harvest.num_sum=sum(harvest.num,na.rm=TRUE),
                          release.num_sum=sum(release.num,na.rm=TRUE),
                          ttlcat_number_sum=sum(ttlcat_number,na.rm=TRUE))
  
  table6a$trip.type<-ddply(surveyharvest.rev, .(Month,trip.type),summarize,
                             num.anglers_length=length(num.anglers),
                             num.anglers_sum=sum(num.anglers,na.rm=TRUE),
                             Pressure_sum=sum(Pressure,na.rm=TRUE),
                             harvest.num_sum=sum(harvest.num,na.rm=TRUE),
                             release.num_sum=sum(release.num,na.rm=TRUE),
                             ttlcat_number_sum=sum(ttlcat_number,na.rm=TRUE))
  
  table6a$sp.sought<-ddply(surveyharvest.rev, .(Month,sp.sought),summarize,
                           num.anglers_length=length(num.anglers),
                           num.anglers_sum=sum(num.anglers,na.rm=TRUE),
                           Pressure_sum=sum(Pressure,na.rm=TRUE),
                           harvest.num_sum=sum(harvest.num,na.rm=TRUE),
                           release.num_sum=sum(release.num,na.rm=TRUE),
                           ttlcat_number_sum=sum(ttlcat_number,na.rm=TRUE))

  id.vars<-c("int.name","Month","sp.sought","spp.harvest","spp.release")
   measure.vars<-c("harvest.num","harvest.mass" ,"release.num")
   interv2.m<-melt(surveydata, id.vars=,id.vars,measure.vars=measure.vars)
   interv2.m$value<-as.numeric(as.character(interv2.m$value))
   harv<-cast(interv2.m,Month+spp.harvest~variable,subset=variable!="release.num",sum) #,margins="grand_row"
   names(harv)[2]<-c("Species")
   release<-cast(interv2.m,Month+spp.release~variable,subset=variable=="release.num",sum)  #,margins="grand_row"
   names(release)[2]<-c("Species")
   table6b<-merge(harv,release, by=c("Month","Species"),all=TRUE)
   table6b[,2:ncol(table6b)]<-apply(table6b[,2:ncol(table6b)],c(1,2),function(x) ifelse(is.na(x),0,x))
   table6b[,3:ncol(table6b)]<-apply(table6b[,3:ncol(table6b)],c(1,2),function(x) as.numeric(x))
   table6b<-table6b[order(table6b$Species),]
   table6b[is.na(table6b)]<-0
   table6b$catch.num<- table6b$harvest.num+table6b$release.num
   #table6b<-table6b[table6b$Species!=0,]
   
   # Table 7 in Brad Newcombs SAS code 
   num.days.surveyed<-ddply(angler_effort,.(Month),summarize,days=length(TotalAngEffort)) 
#    num.days.surveyed<-recast(angler_effort,Month~variable,id.var="Month",measure.var="TotalAngEffort",length) 
#    names(num.days.surveyed)<-c("Month","days")
      
   Par_day<-data.frame(Month=table6a$month$Month,Parties_day=table6a$month$num.anglers_length/ num.days.surveyed$days[match(table6a$month$Month,num.days.surveyed$Month)])
   surveyharvest.rev$party.success<-ifelse(surveyharvest.rev$ttlcat_number>0,1,0)


# num.succ.parties<-recast(surveyharvest.rev,Month~variable,id.var="Month",measure.var="party.success",sum) 
   num.succ.parties<-ddply(surveyharvest.rev,.(Month),summarize,party.success=sum(party.success,na.rm=TRUE))
   sparty<-data.frame(Month=table6a$month$Month,Succ_party= num.succ.parties$party.success/ table6a$month$num.anglers_length[match(table6a$month$Month,num.succ.parties$Month)])
   Ang_day<-data.frame(Month=table6a$month$Month,Anglers_day=table6a$month$num.anglers_sum/ num.days.surveyed$days[match(table6a$month$Month,num.days.surveyed$Month)])
   
   Tot_hrs<-ddply(surveyharvest.rev,.(Month),summarize,Tot_hrs=sum(Pressure,na.rm=TRUE))
#    Tot_hrs<-recast(surveyharvest.rev,Month~variable,id.var="Month",measure.var="Pressure",sum) 
   Tot_hrs_trptype.a<-ddply(surveyharvest.rev,.(Month,trip.type),summarize, Pressure=sum(Pressure,na.rm=TRUE))
#    Tot_hrs_trptype.a<-recast(surveyharvest.rev,Month+trip.type~variable,id.var=c("Month","trip.type"),measure.var="Pressure",sum)
   Tot_hrs_trptype<- Tot_hrs_trptype.a
   Tot_hrs_trptype$Pressure<- Tot_hrs_trptype.a$Pressure/table6a$trip.type$num.anglers_sum[match(paste(Tot_hrs_trptype$Month,Tot_hrs_trptype$trip.type,sep=""),paste(table6a$trip.type$Month,table6a$trip.type$trip.type,sep=""))]
   Party.descript<-list(Parties_day=Par_day,Successful_parties=sparty,Anglers_day=Ang_day,Total_hours= Tot_hrs,Total_hour_triptype=Tot_hrs_trptype)

   #tables 14A in  Brad Newcombs SAS code
    pressure.m<-melt(surveyharvest.rev,id.var=c("Month", "sp.sought"),measure.vars="Pressure",na.rm=TRUE)
    pressure_month<-cast(pressure.m,sp.sought+Month~variable,sum)
    pressure_ttl_month<-cast(pressure.m,Month~variable,sum)
    names(pressure_ttl_month)[2]<-"ttl_pressure"
    pressure_month<-merge(pressure_month,pressure_ttl_month,by="Month")
    pressure_month$perc<-pressure_month$Pressure/pressure_month$ttl_pressure
    pressure_month<-merge(pressure_month,MH_ANGC,by="Month")
    
    monthly_effort_all.b<-with(angler.effort.out$Monthly.effort, data.frame(Month=Month,mmon.all,
                             mmon.bnk, mmon.bot,mmon.ice=mmon.spc,mmon.bow,mmon.spr))  # Special angler becomes ice here!
    
    pressure_effort<-merge(pressure_month,monthly_effort_all.b,by="Month")
    pressure_effort$mon.press.all<- with(pressure_effort,perc*mmon.all) # Monthly effort for all anglers
    pressure_effort$mon.press.bnk<- with(pressure_effort,perc*mmon.bnk) # Monthly effort for bank anglers
    pressure_effort$mon.press.bot<- with(pressure_effort,perc*mmon.bot) # Monthly effort for boat anglers
    pressure_effort$mon.press.ice<- with(pressure_effort,perc*mmon.ice)# Monthly effort for ice anglers  
    pressure_effort$mon.press.bow<- with(pressure_effort,perc*mmon.bow) # Monthly effort for bow anglers
    pressure_effort$mon.press.spr<- with(pressure_effort,perc*mmon.spr) # Monthly effort for spear anglers
    
    pressure_effort$mon.angl.all<- with(pressure_effort,perc*(mmon.all/MH_ANGC)) #Monthly number of anglers
    pressure_effort$mon.angl.bnk<- with(pressure_effort,perc*(mmon.bnk/MH_ANGC)) #Monthly number of anglers
    pressure_effort$mon.angl.bot<- with(pressure_effort,perc*(mmon.bot/MH_ANGC)) #Monthly number of anglers
    pressure_effort$mon.angl.ice<- with(pressure_effort,perc*(mmon.ice/MH_ANGC)) #Monthly number of anglers
    pressure_effort$mon.angl.bow<- with(pressure_effort,perc*(mmon.bow/MH_ANGC)) #Monthly number of anglers
    pressure_effort$mon.angl.spr<- with(pressure_effort,perc*(mmon.spr/MH_ANGC)) #Monthly number of anglers
    
    pressure_effort.rev<-pressure_effort[,-c(4:12)]
    pressure_effort.rev<-ddply(pressure_effort.rev,.(Month,sp.sought),summarize,
                                Pressure=sum(Pressure),
                                mon.press.all=sum(mon.press.all),
                                mon.press.bnk=sum(mon.press.bnk),
                                mon.press.bot=sum(mon.press.bot),
                                mon.press.ice=sum(mon.press.ice),
                                mon.press.bow=sum(mon.press.bow),
                                mon.press.spr=sum(mon.press.spr),
                                mon.angl.all=sum(mon.angl.all),
                                mon.angl.bnk=sum(mon.angl.bnk),
                                mon.angl.bot=sum(mon.angl.bot),
                                mon.angl.ice=sum(mon.angl.ice),
                                mon.angl.bow=sum(mon.angl.bow),
                                mon.angl.spr=sum(mon.angl.spr))

    # Yearly estimates (Table 14B in Brad's SAS code)
    pressure_yr<-ddply(pressure_effort.rev,.(sp.sought),summarize,
                               Pressure=sum(Pressure),
                       mon.press.all=sum(mon.press.all),
                       mon.press.bnk=sum(mon.press.bnk),
                       mon.press.bot=sum(mon.press.bot),
                       mon.press.ice=sum(mon.press.ice),
                       mon.press.bow=sum(mon.press.bow),
                       mon.press.spr=sum(mon.press.spr))


#         pressure_yr<-cast(pressure.m,sp.sought~variable,sum)
    pressure_yr$perc<-pressure_yr$Pressure/sum(pressure_yr$Pressure)
    year_effort_all.b<-with(angler.effort.out$Yearly.effort, data.frame(myear.all,
                             myear.bnk, myear.bot,myear.ice=myear.spc,myear.bow,myear.spr))    #Special becomes ICE
    pressure_effort_yr<-cbind(pressure_yr,year_effort_all.b)
    pressure_effort_yr$year.press.all<- with(pressure_effort_yr,perc*myear.all) # Monthly effort for all anglers
    pressure_effort_yr$year.press.bnk<- with(pressure_effort_yr,perc*myear.bnk) # Monthly effort for bank anglers
    pressure_effort_yr$year.press.bot<- with(pressure_effort_yr,perc*myear.bot) # Monthly effort for boat anglers
    pressure_effort_yr$year.press.ice<- with(pressure_effort_yr,perc*myear.ice)# Monthly effort for ice anglers  
    pressure_effort_yr$year.press.bow<- with(pressure_effort_yr,perc*myear.bow) # Monthly effort for bow anglers
    pressure_effort_yr$year.press.spr<- with(pressure_effort_yr,perc*myear.spr) # Monthly effort for spear anglers
    
    #MH_ANGC<-   #mean hours for complete trips
    pressure_effort_yr$year.angl.all<- with(pressure_effort_yr,perc*(myear.all/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    pressure_effort_yr$year.angl.bnk<- with(pressure_effort_yr,perc*(myear.bnk/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    pressure_effort_yr$year.angl.bot<- with(pressure_effort_yr,perc*(myear.bot/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    pressure_effort_yr$year.angl.ice<- with(pressure_effort_yr,perc*(myear.ice/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    pressure_effort_yr$year.angl.bow<- with(pressure_effort_yr,perc*(myear.bow/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    pressure_effort_yr$year.angl.spr<- with(pressure_effort_yr,perc*(myear.spr/mean(surveyharvest.rev$time.diff_hr[surveyharvest.rev$trip.type==1]))) #Monthly number of anglers
    
    pressure_effort_yr.rev<-pressure_effort_yr[,-c(3:9)]
    #pressure_effort_yr.rev<-recast(pressure_effort_yr.rev,id.var="sp.sought",sp.sought~variable,sum)
       
    pressure.tables<-list(Table6a=table6a,Table6B=table6b,Table7=Party.descript,Table14a=pressure_effort.rev,Table14B=pressure_effort_yr.rev,Angler_effort=angler.effort.out)
    
    # Harvest release, and catch calculations
    surveydata.m<-melt(surveydata, id.var=c("Date","day_type","time.diff_hr","trip.type", "spp.harvest", "spp.release"),measure.var=c("num.anglers","harvest.num","harvest.mass","release.num"))
    ttl.catches<-cast(surveydata.m,Date+day_type+trip.type+time.diff_hr+spp.harvest+spp.release~variable,sum)
    ttl.catches<-ttl.catches[!(ttl.catches$spp.harvest==0 & ttl.catches$spp.release==0),]
    if(nrow(ttl.catches)>0){
    
    harv.catches<-ttl.catches[ttl.catches$spp.harvest!=0,c("Date","day_type","time.diff_hr","trip.type", "spp.harvest","num.anglers","harvest.num","harvest.mass")]
    if(nrow(harv.catches)==0) harv.catches[1,]<-NA  # If there is no harvest then create a line with NA's, will be removed later
    harv.catches$release.num<-0
    names(harv.catches)[5]<-"species"
    
    rel.catches<-ttl.catches[ttl.catches$spp.release!=0,c("Date","day_type","time.diff_hr","trip.type", "spp.release","num.anglers","time.diff_hr","release.num")]
    if(nrow(rel.catches)==0) rel.catches[1,]<-NA     # If there is no release then create a line with NA's, will be removed later
    rel.catches$harvest.num<-0
    rel.catches$harvest.mass<-0
    names(rel.catches)[5]<-"species"
    rel.catches<-rel.catches[,c("Date","day_type","time.diff_hr","trip.type", "species","num.anglers","harvest.num","harvest.mass","release.num")]
    
    byspec<-ddply(as.data.frame(rbind(harv.catches,rel.catches)),.(Date,day_type,trip.type,species,time.diff_hr),
                  num.anglers=sum(num.anglers,na.rm=TRUE),
                  harvest.num=sum(harvest.num,na.rm=TRUE),
                  harvest.mass=sum(harvest.mass,na.rm=TRUE),
                  release.num=sum(release.num,na.rm=TRUE)    
                  )
    byspec<-byspec[!is.na(byspec$Date),]
    byspec$harv_ang<- byspec$harvest.num/byspec$num.anglers
    byspec$rel_ang<- byspec$release.num/byspec$num.anglers
    byspec$catch.num<-byspec$harvest.num+byspec$release.num
    byspec$cat_ang<- byspec$catch.num/byspec$num.anglers
    
    byspec.m<-melt(as.data.frame(byspec),id.var=c("Date","species"),measure.var=c("harvest.num","harvest.mass","release.num","catch.num"))
    sum_daily_catch<-cast(byspec.m,Date+species~variable,sum)
    sum_daily_catch<-sum_daily_catch[!is.na(sum_daily_catch$Date),]
    names(sum_daily_catch)<-c("Date","species","dayhct","dayhwt","dayrct","daycat")
    
    sum_daily_pressure<-ddply(surveyharvest.rev,.(Date,day_type),summarize,daypress=sum(Pressure))
  
  intvday<-merge(sum_daily_catch,sum_daily_pressure,by="Date",all=TRUE)
  intvday[is.na(intvday)]<-0
  daily<-ddply(intvday,.(Date),summarize,
               dayhct=sum(dayhct,na.rm=TRUE),
               dayhwt=sum(dayhwt,na.rm=TRUE),
               dayrct=sum(dayrct,na.rm=TRUE),
               daycat=sum(daycat,na.rm=TRUE))

  daily$species<-999      #  Here 
  daily<-merge(daily,sum_daily_pressure,by="Date")
  
  intvday1<-rbind.fill(intvday,daily)
  rownames(intvday1)<-NULL
  intvday1<-intvday1[order(intvday1$Date,intvday1$species),]
  intvday1$hcpe<-intvday1$dayhct/intvday1$daypress
  intvday1$wcpe<-intvday1$dayhwt/intvday1$daypress
  intvday1$rcpe<-intvday1$dayrct/intvday1$daypress
  intvday1$ccpe<-intvday1$daycat/intvday1$daypress
  
#   surveyharvest.cws.ttl<-recast(surveyharvest.rev,id.var=c("Date","sp.sought"),measure.var=c("cws.harvest","cws.mass","cws.release","cws.catch","Pressure"), Date+sp.sought~variable,sum)
  surveyharvest.cws.ttl<-ddply(surveyharvest.rev,.(Date,sp.sought),summarize, 
                               cws.harvest=sum(cws.harvest,na.rm=TRUE),
                               cws.mass=sum(cws.mass,na.rm=TRUE),
                               cws.release=sum(cws.release,na.rm=TRUE),
                               cws.catch=sum(cws.catch,na.rm=TRUE),
                               Pressure =sum(Pressure,na.rm=TRUE))
    
    
  names(surveyharvest.cws.ttl)[c(2,7)]<-c("species","cws.press")
  surveyharvest.cws.ttl$cws.hcpe<-surveyharvest.cws.ttl$cws.harvest/surveyharvest.cws.ttl$cws.press
  surveyharvest.cws.ttl$cws.wcpe<-surveyharvest.cws.ttl$cws.mass/surveyharvest.cws.ttl$cws.press
  surveyharvest.cws.ttl$cws.rcpe<-surveyharvest.cws.ttl$cws.release/surveyharvest.cws.ttl$cws.press
  surveyharvest.cws.ttl$cws.ccpe<-surveyharvest.cws.ttl$cws.catch/surveyharvest.cws.ttl$cws.press
  
  intvday<-merge(intvday1,surveyharvest.cws.ttl, by=c("Date","species"),all=T)
  
  spp.harv.ind<-which(substr(names(surveyharvest.rev),1,12)=="harvest.spp.")
  spp.rel.ind<-which(substr(names(surveyharvest.rev),1,12)=="release.spp.")
  species.caught<- surveyharvest.rev[,c(spp.harv.ind,spp.rel.ind)]

  datesp<-expand.grid(unique(angler_effort$Date),unique(c(as.matrix(species.caught),999)))
  names(datesp)<-c("Date","species")
  datesp<-datesp[datesp$species!=0,]
  datesp<-datesp[order(datesp$Date,datesp$species),]
  rownames(datesp)<-NULL
  ang.effort<-angler_effort[,c("Date","TotalAngEffort","BankAngEffort","BoatAngEffort")]
  ang.effort$IceAngEffort<-angler_effort$SpecAngEffort    # Eventually will need to replace this. 
  ang.effort$BowAngEffort<-angler_effort$BowAngEffort
  ang.effort$SpearAngEffort<-angler_effort$SprAngEffort
  
  datesp<-merge(datesp,ang.effort,by="Date",all=T)
  datesp<-merge(datesp,angler_effort[!duplicated(angler_effort$Date),c("Date","day_type")],all.x=T)  # add in day_type
  datesp$Month<-months(datesp$Date,abbreviate=T)
  
  intvday.reva<-intvday[,c("Date","species","hcpe","wcpe","rcpe","ccpe","cws.hcpe","cws.wcpe","cws.rcpe","cws.ccpe")]
  intvday.rev<-intvday.reva[intvday.reva$species!=0,]
  catday<-merge(datesp,intvday.rev,by=c("Date","species"),all.x=T)  # removes species sought but not caught
  catday$hcpe[is.na(catday$hcpe)]<-0  # replace missing harvest, release, and caught with 0's
  catday$wcpe[is.na(catday$wcpe)]<-0
  catday$rcpe[is.na(catday$rcpe)]<-0
  catday$ccpe[is.na(catday$ccpe)]<-0
  
  daily_harvest<-catday$hcpe*catday[,c("TotalAngEffort","BankAngEffort","BoatAngEffort","IceAngEffort","BowAngEffort","SpearAngEffort")]
  names(daily_harvest)<-c("dharall","dharbk","dharbt","dharice","dharbow","dharspr")
  
  daily_mass<-catday$wcpe*catday[,c("TotalAngEffort","BankAngEffort","BoatAngEffort","IceAngEffort","BowAngEffort","SpearAngEffort")]
  names(daily_mass)<-c("dwgtall","dwgtbk","dwgtbt","dwgtice","dwgtbow","dwgtspr")
  
  daily_release<-catday$rcpe*catday[,c("TotalAngEffort","BankAngEffort","BoatAngEffort","IceAngEffort","BowAngEffort","SpearAngEffort")]
  names(daily_release)<-c("drelall","drelbk","drelbt","drelice","drelbow","drelspr")
  
  daily_catch<-catday$ccpe*catday[,c("TotalAngEffort","BankAngEffort","BoatAngEffort","IceAngEffort","BowAngEffort","SpearAngEffort")]
  names(daily_catch)<-c("dcatall","dcatbk","dcatbt","dcatice","dcatbow","dcatspr")
  
  catday.rev<-cbind(catday,daily_harvest,daily_mass,daily_release,daily_catch)
    
  id.var<-c("Month","day_type","species")
  measure.var1<-c("cws.hcpe","cws.wcpe","cws.rcpe","cws.ccpe")
  measure.var2<-c("hcpe","wcpe","rcpe","ccpe","dharall","dharbk","dharbt","dharice","dharbow",
                  "dharspr","dwgtall","dwgtbk","dwgtbt","dwgtice","dwgtbow","dwgtspr","drelall",
                  "drelbk","drelbt","drelice","drelbow","drelspr","dcatall","dcatbk","dcatbt",
                  "dcatice","dcatbow","dcatspr")
  if(any(!is.na(catday.rev$cws.ccpe))){
    catday.m1<-melt(catday.rev,id.var=id.var,measure.var=measure.var1,na.rm=TRUE)
    }else{
      catday.m1<-melt(catday.rev,id.var=id.var,measure.var=measure.var1)  
      }
 
          
  step5b<-cast(catday.m1,Month+day_type+species~variable, c(sd,length,mean,var))
  step5b$cws.hcpe_se<- step5b$cws.hcpe_sd/sqrt(step5b$cws.hcpe_length)
  step5b$cws.wcpe_se<- step5b$cws.hcpe_sd/sqrt(step5b$cws.hcpe_length)
  step5b$cws.rcpe_se<- step5b$cws.hcpe_sd/sqrt(step5b$cws.hcpe_length)
  step5b$cws.ccpe_se<- step5b$cws.hcpe_sd/sqrt(step5b$cws.hcpe_length)
  step5b<-step5b[,c("Month","day_type","species","cws.hcpe_se","cws.hcpe_mean","cws.hcpe_var",
                    "cws.wcpe_se","cws.wcpe_mean","cws.wcpe_var",
                    "cws.rcpe_se","cws.rcpe_mean","cws.rcpe_var",
                    "cws.ccpe_se","cws.ccpe_mean","cws.ccpe_var")]
    
 
      
  step5c<-ddply(catday.rev,.(Month,day_type,species),summarize,
        hcpe_se=se.rmna(hcpe),
        hcpe_mean=mean(hcpe,na.rm=TRUE),
        hcpe_var=var(hcpe,na.rm=TRUE),
        wcpe_se=se.rmna(wcpe),
        wcpe_mean=mean(wcpe,na.rm=TRUE),
        wcpe_var=var(wcpe,na.rm=TRUE),
        rcpe_se=se.rmna(rcpe),
        rcpe_mean=mean(rcpe,na.rm=TRUE),
        rcpe_var=var(rcpe,na.rm=TRUE),
        ccpe_se=se.rmna(ccpe),
        ccpe_mean=mean(ccpe,na.rm=TRUE),
        ccpe_var=var(ccpe,na.rm=TRUE),
        dharall_se=se.rmna(dharall),
        dharall_mean=mean(dharall,na.rm=TRUE),
        dharall_var=var(dharall,na.rm=TRUE),
        dharbk_se=se.rmna(dharbk),
        dharbk_mean=mean(dharbk,na.rm=TRUE),
        dharbk_var=var(dharbk,na.rm=TRUE),
        dharbt_se=se.rmna(dharbt),
        dharbt_mean=mean(dharbt,na.rm=TRUE),
        dharbt_var=var(dharbt,na.rm=TRUE),
        dharice_se=se.rmna(dharice),
        dharice_mean=mean(dharice,na.rm=TRUE),
        dharice_var=var(dharice,na.rm=TRUE),
        dharbow_se=se.rmna(dharbow),
        dharbow_mean=mean(dharbow,na.rm=TRUE),
        dharbow_var=var(dharbow,na.rm=TRUE),
        dharspr_se=se.rmna(dharspr),
        dharspr_mean=mean(dharspr,na.rm=TRUE),
        dharspr_var=var(dharspr,na.rm=TRUE),
        dwgtall_se=se.rmna(dwgtall),
        dwgtall_mean=mean(dwgtall,na.rm=TRUE),
        dwgtall_var=var(dwgtall,na.rm=TRUE),
        dwgtbk_se=se.rmna(dwgtbk),
        dwgtbk_mean=mean(dwgtbk,na.rm=TRUE),
        dwgtbk_var=var(dwgtbk,na.rm=TRUE),
        dwgtbt_se=se.rmna(dwgtbt),
        dwgtbt_mean=mean(dwgtbt,na.rm=TRUE),
        dwgtbt_var=var(dwgtbt,na.rm=TRUE),
        dwgtice_se=se.rmna(dwgtice),
        dwgtice_mean=mean(dwgtice,na.rm=TRUE),
        dwgtice_var=var(dwgtice,na.rm=TRUE),
        dwgtbow_se=se.rmna(dwgtbow),
        dwgtbow_mean=mean(dwgtbow,na.rm=TRUE),
        dwgtbow_var=var(dwgtbow,na.rm=TRUE),
        dwgtspr_se=se.rmna(dwgtspr),
        dwgtspr_mean=mean(dwgtspr,na.rm=TRUE),
        dwgtspr_var=var(dwgtspr,na.rm=TRUE),
        drelall_se=se.rmna(drelall),
        drelall_mean=mean(drelall,na.rm=TRUE),
        drelall_var=var(drelall,na.rm=TRUE),
        drelbk_se=se.rmna(drelbk),
        drelbk_mean=mean(drelbk,na.rm=TRUE),
        drelbk_var=var(drelbk,na.rm=TRUE),
        drelbt_se=se.rmna(drelbt),
        drelbt_mean=mean(drelbt,na.rm=TRUE),
        drelbt_var=var(drelbt,na.rm=TRUE),
        drelice_se=se.rmna(drelice),
        drelice_mean=mean(drelice,na.rm=TRUE),
        drelice_var=var(drelice,na.rm=TRUE),
        drelbow_se=se.rmna(drelbow),
        drelbow_mean=mean(drelbow,na.rm=TRUE),
        drelbow_var=var(drelbow,na.rm=TRUE),
        drelspr_se=se.rmna(drelspr),
        drelspr_mean=mean(drelspr,na.rm=TRUE),
        drelspr_var=var(drelspr,na.rm=TRUE),
        dcatall_se=se.rmna(dcatall),
        dcatall_mean=mean(dcatall,na.rm=TRUE),
        dcatall_var=var(dcatall,na.rm=TRUE),
        dcatbk_se=se.rmna(dcatbk),
        dcatbk_mean=mean(dcatbk,na.rm=TRUE),
        dcatbk_var=var(dcatbk,na.rm=TRUE),
        dcatbt_se=se.rmna(dcatbt),
        dcatbt_mean=mean(dcatbt,na.rm=TRUE),
        dcatbt_var=var(dcatbt,na.rm=TRUE),
        dcatspr_se=se.rmna(dcatspr),
        dcatspr_mean=mean(dcatspr,na.rm=TRUE),
        dcatspr_var=var(dcatspr,na.rm=TRUE),
        dcatbow_se=se.rmna(dcatbow),
        dcatbow_mean=mean(dcatbow,na.rm=TRUE),
        dcatbow_var=var(dcatbow,na.rm=TRUE),
        dcatice_se=se.rmna(dcatice),
        dcatice_mean=mean(dcatice,na.rm=TRUE),
        dcatice_var=var(dcatice,na.rm=TRUE)
        )
     
#   catday.m2<-melt(catday.rev,id.var=id.var,measure.var=measure.var2,na.rm=TRUE)
#   step5c<-cast(catday.m2,Month+day_type+species~variable, c(se=function(x) sd(x)/sqrt(length(x)),mean,var))
#   names(step5c)[seq(4,(ncol(step5c)),by=3)]<- paste(measure.var2,"se",sep="_")
#   
  names(survey.day_type_m)[3]<-"surv.count"
  day.comb.ordered.rev<-merge(day.comb,survey.day_type_m,by=c("Month","day_type"))
  step5a<-merge(step5c,step5b, by=c("Month","day_type","species"),all=T)
  step5<-merge(step5a,day.comb.ordered.rev, by =c("Month", "day_type"),all=T)
  step5<-step5[order(step5$Month, step5$species, step5$day_type),]  # same as Brad's Step 5C dataset
  
  step5$cws.hcpe_se[is.na(step5$cws.hcpe_se)] <-0
  step5$cws.hcpe_mean[is.na(step5$cws.hcpe_mean)] <-0
  step5$cws.hcpe_var[is.na(step5$cws.hcpe_var)]<-0
  step5$cws.wcpe_se[is.na(step5$cws.wcpe_se)] <-0
  step5$cws.wcpe_mean[is.na(step5$cws.wcpe_mean)]<-0
  step5$cws.wcpe_var[is.na(step5$cws.wcpe_var)] <-0
  step5$cws.rcpe_se[is.na(step5$cws.rcpe_se)] <-0
  step5$cws.rcpe_mean[is.na(step5$cws.rcpe_mean)] <-0
  step5$cws.rcpe_var[is.na(step5$cws.rcpe_var)]<-0 
  step5$cws.ccpe_se[is.na(step5$cws.ccpe_se)] <-0
  step5$cws.ccpe_mean[is.na(step5$cws.ccpe_mean)] <-0
  step5$cws.ccpe_var[is.na(step5$cws.ccpe_var)]<-0                        
  
  # Mean daily catch estimates
  mdaycat<-NULL
  mdaycat$Month<-step5$Month
  mdaycat$species<-step5$species
  
  # Total catches
  mdaycat$mdaymc.all<-with(step5,dcatall_mean*day_type.weight)
  mdaycat$mdaymc.bot<-with(step5,dcatbt_mean*day_type.weight)
  mdaycat$mdaymc.bnk<-with(step5,dcatbk_mean*day_type.weight)
  mdaycat$mdaymc.ice<-with(step5,dcatice_mean*day_type.weight)
  mdaycat$mdaymc.bow<-with(step5,dcatbow_mean*day_type.weight)
  mdaycat$mdaymc.spr<-with(step5,dcatspr_mean*day_type.weight)
  
  # Releases numbers
  mdaycat$mdaymr.all<-with(step5,drelall_mean*day_type.weight)
  mdaycat$mdaymr.bot<-with(step5,drelbt_mean*day_type.weight)
  mdaycat$mdaymr.bnk<-with(step5,drelbk_mean*day_type.weight)
  mdaycat$mdaymr.ice<-with(step5,drelice_mean*day_type.weight)
  mdaycat$mdaymr.bow<-with(step5,drelbow_mean*day_type.weight)
  mdaycat$mdaymr.spr<-with(step5,drelspr_mean*day_type.weight)
  
  # Harvest numbers
  mdaycat$mdaymh.all<-with(step5,dharall_mean*day_type.weight)
  mdaycat$mdaymh.bot<-with(step5,dharbt_mean*day_type.weight)
  mdaycat$mdaymh.bnk<-with(step5,dharbk_mean*day_type.weight)
  mdaycat$mdaymh.ice<-with(step5,dharice_mean*day_type.weight)
  mdaycat$mdaymh.bow<-with(step5,dharbow_mean*day_type.weight)
  mdaycat$mdaymh.spr<-with(step5,dharspr_mean*day_type.weight)
  
  # Harvest mass
  mdaycat$mdaymw.all<-with(step5,dwgtall_mean*day_type.weight)
  mdaycat$mdaymw.bot<-with(step5,dwgtbt_mean*day_type.weight)
  mdaycat$mdaymw.bnk<-with(step5,dwgtbk_mean*day_type.weight)
  mdaycat$mdaymw.ice<-with(step5,dwgtice_mean*day_type.weight)
  mdaycat$mdaymw.bow<-with(step5,dwgtbow_mean*day_type.weight)
  mdaycat$mdaymw.spr<-with(step5,dwgtspr_mean*day_type.weight)
  
  # CPE - All effort
  mdaycat$mdayrcpe<-with(step5,rcpe_mean*day_type.weight)
  mdaycat$mdayhcpe<-with(step5,hcpe_mean*day_type.weight)
  mdaycat$mdaywcpe<-with(step5,wcpe_mean*day_type.weight)
  mdaycat$mdayccpe<-with(step5,ccpe_mean*day_type.weight)
    
  # CPE - Caught while sought
  mdaycat$cws.mdayrcpe<-with(step5,cws.rcpe_mean*day_type.weight)
  mdaycat$cws.mdayhcpe<-with(step5,cws.hcpe_mean*day_type.weight)
  mdaycat$cws.mdaywcpe<-with(step5,cws.wcpe_mean*day_type.weight)
  mdaycat$cws.mdayccpe<-with(step5,cws.ccpe_mean*day_type.weight)
  
  #Variance estimates
  # Catch 
  mdaycat$vdaymc.all<-with(step5,(day_type.weight^2)*(dcatall_var/surv.count)) - with(step5,day_type.weight*(dcatall_var/count.y)) 
  mdaycat$vdaymc.bot<-with(step5,(day_type.weight^2)*(dcatbt_var/surv.count)) - with(step5,day_type.weight*(dcatbt_var/count.y)) 
  mdaycat$vdaymc.bnk<-with(step5,(day_type.weight^2)*(dcatbk_var/surv.count)) - with(step5,day_type.weight*(dcatbk_var/count.y)) 
  mdaycat$vdaymc.ice<-with(step5,(day_type.weight^2)*(dcatice_var/surv.count)) - with(step5,day_type.weight*(dcatice_var/count.y)) 
  mdaycat$vdaymc.bow<-with(step5,(day_type.weight^2)*(dcatbow_var/surv.count)) - with(step5,day_type.weight*(dcatbow_var/count.y)) 
  mdaycat$vdaymc.spr<-with(step5,(day_type.weight^2)*(dcatspr_var/surv.count)) - with(step5,day_type.weight*(dcatspr_var/count.y)) 
  
  # Release 
  mdaycat$vdaymr.all<-with(step5,(day_type.weight^2)*(drelall_var/surv.count)) - with(step5,day_type.weight*(drelall_var/count.y)) 
  mdaycat$vdaymr.bot<-with(step5,(day_type.weight^2)*(drelbt_var/surv.count)) - with(step5,day_type.weight*(drelbt_var/count.y)) 
  mdaycat$vdaymr.bnk<-with(step5,(day_type.weight^2)*(drelbk_var/surv.count)) - with(step5,day_type.weight*(drelbk_var/count.y)) 
  mdaycat$vdaymr.ice<-with(step5,(day_type.weight^2)*(drelice_var/surv.count)) - with(step5,day_type.weight*(drelice_var/count.y)) 
  mdaycat$vdaymr.bow<-with(step5,(day_type.weight^2)*(drelbow_var/surv.count)) - with(step5,day_type.weight*(drelbow_var/count.y)) 
  mdaycat$vdaymr.spr<-with(step5,(day_type.weight^2)*(drelspr_var/surv.count)) - with(step5,day_type.weight*(drelspr_var/count.y)) 
  
  # Harvest numbers 
  mdaycat$vdaymh.all<-with(step5,(day_type.weight^2)*(dharall_var/surv.count)) - with(step5,day_type.weight*(dharall_var/count.y)) 
  mdaycat$vdaymh.bot<-with(step5,(day_type.weight^2)*(dharbt_var/surv.count)) - with(step5,day_type.weight*(dharbt_var/count.y)) 
  mdaycat$vdaymh.bnk<-with(step5,(day_type.weight^2)*(dharbk_var/surv.count)) - with(step5,day_type.weight*(dharbk_var/count.y)) 
  mdaycat$vdaymh.ice<-with(step5,(day_type.weight^2)*(dharice_var/surv.count)) - with(step5,day_type.weight*(dharice_var/count.y)) 
  mdaycat$vdaymh.bow<-with(step5,(day_type.weight^2)*(dharbow_var/surv.count)) - with(step5,day_type.weight*(dharbow_var/count.y)) 
  mdaycat$vdaymh.spr<-with(step5,(day_type.weight^2)*(dharspr_var/surv.count)) - with(step5,day_type.weight*(dharspr_var/count.y)) 
  
 # Harvest mass 
  mdaycat$vdaymw.all<-with(step5,(day_type.weight^2)*(dwgtall_var/surv.count)) - with(step5,day_type.weight*(dwgtall_var/count.y)) 
  mdaycat$vdaymw.bot<-with(step5,(day_type.weight^2)*(dwgtbt_var/surv.count)) - with(step5,day_type.weight*(dwgtbt_var/count.y)) 
  mdaycat$vdaymw.bnk<-with(step5,(day_type.weight^2)*(dwgtbk_var/surv.count)) - with(step5,day_type.weight*(dwgtbk_var/count.y)) 
  mdaycat$vdaymw.ice<-with(step5,(day_type.weight^2)*(dwgtice_var/surv.count)) - with(step5,day_type.weight*(dwgtice_var/count.y)) 
  mdaycat$vdaymw.bow<-with(step5,(day_type.weight^2)*(dwgtbow_var/surv.count)) - with(step5,day_type.weight*(dwgtbow_var/count.y)) 
  mdaycat$vdaymw.spr<-with(step5,(day_type.weight^2)*(dwgtspr_var/surv.count)) - with(step5,day_type.weight*(dwgtspr_var/count.y)) 
  
  # CPE - All effort
  mdaycat$vdayrcpe<-with(step5,(day_type.weight^2)*(rcpe_var/surv.count)) - with(step5,day_type.weight*(rcpe_var/count.y)) 
  mdaycat$vdayhcpe<-with(step5,(day_type.weight^2)*(hcpe_var/surv.count)) - with(step5,day_type.weight*(hcpe_var/count.y)) 
  mdaycat$vdaywcpe<-with(step5,(day_type.weight^2)*(wcpe_var/surv.count)) - with(step5,day_type.weight*(wcpe_var/count.y)) 
  mdaycat$vdayccpe<-with(step5,(day_type.weight^2)*(ccpe_var/surv.count)) - with(step5,day_type.weight*(ccpe_var/count.y)) 
  
   # CPE - Caught while sought
  mdaycat$cws.vdayrcpe<-with(step5,(day_type.weight^2)*(cws.rcpe_var/surv.count)) - with(step5,day_type.weight*(cws.rcpe_var/count.y)) 
  mdaycat$cws.vdayhcpe<-with(step5,(day_type.weight^2)*(cws.hcpe_var/surv.count)) - with(step5,day_type.weight*(cws.hcpe_var/count.y)) 
  mdaycat$cws.vdaywcpe<-with(step5,(day_type.weight^2)*(cws.wcpe_var/surv.count)) - with(step5,day_type.weight*(cws.wcpe_var/count.y)) 
  mdaycat$cws.vdayccpe<-with(step5,(day_type.weight^2)*(cws.ccpe_var/surv.count)) - with(step5,day_type.weight*(cws.ccpe_var/count.y)) 
  
  mdaycat<-as.data.frame(mdaycat)
  mdaycat.m<-melt(mdaycat,id.var=c("Month","species"),na.rm=TRUE)
  mdaycat.rev<-cast(mdaycat.m,Month+species~variable,sum)
  mdaycat.rev$days_month<-step5$count.y[match(mdaycat.rev$Month,step5$Month)]
    
  monthcat<-NULL
  monthcat$Month<-mdaycat.rev$Month
  monthcat$species<-mdaycat.rev$species
  #estimates  for catch
  monthcat$moncat.all <-  with(mdaycat.rev,mdaymc.all*days_month)
  monthcat$moncat.bot <-  with(mdaycat.rev,mdaymc.bot*days_month)
  monthcat$moncat.bnk <-  with(mdaycat.rev,mdaymc.bnk*days_month)
  monthcat$moncat.ice <-  with(mdaycat.rev,mdaymc.ice*days_month)
  monthcat$moncat.bow <-  with(mdaycat.rev,mdaymc.bow*days_month)
  monthcat$moncat.spr <-  with(mdaycat.rev,mdaymc.spr*days_month)
  #variances for catch
  monthcat$moncat_var.all <-  with(mdaycat.rev,vdaymc.all*(days_month^2))
  monthcat$moncat_var.bot <-  with(mdaycat.rev,vdaymc.bot*(days_month^2))
  monthcat$moncat_var.bnk <-  with(mdaycat.rev,vdaymc.bnk*(days_month^2))
  monthcat$moncat_var.ice <-  with(mdaycat.rev,vdaymc.ice*(days_month^2))
  monthcat$moncat_var.bow <-  with(mdaycat.rev,vdaymc.bow*(days_month^2))
  monthcat$moncat_var.spr <-  with(mdaycat.rev,vdaymc.spr*(days_month^2))
  
  #standard deviations for catch
  monthcat$moncat_sd.all <-  sqrt(monthcat$moncat_var.all)
  monthcat$moncat_sd.bot <-  sqrt(monthcat$moncat_var.bot)
  monthcat$moncat_sd.bnk <-  sqrt(monthcat$moncat_var.bnk)
  monthcat$moncat_sd.ice <-  sqrt(monthcat$moncat_var.ice)
  monthcat$moncat_sd.bow <-  sqrt(monthcat$moncat_var.bow)
  monthcat$moncat_sd.spr <-  sqrt(monthcat$moncat_var.spr)
  
  #coefficient of variations for catch RSE
  monthcat$moncat_RSE.all <-  with(monthcat,ifelse(is.na(moncat.all)|moncat.all==0,NA,((moncat_sd.all/moncat.all)*100)))
  monthcat$moncat_RSE.bot <-  with(monthcat,ifelse(is.na(moncat.bot)|moncat.bot==0,NA,((moncat_sd.bot/moncat.bot)*100)))
  monthcat$moncat_RSE.bnk <-  with(monthcat,ifelse(is.na(moncat.bnk)|moncat.bnk==0,NA,((moncat_sd.bnk/moncat.bnk)*100)))
  monthcat$moncat_RSE.ice <-  with(monthcat,ifelse(is.na(moncat.ice)|moncat.ice==0,NA,((moncat_sd.ice/moncat.ice)*100)))
  monthcat$moncat_RSE.bow <-  with(monthcat,ifelse(is.na(moncat.bow)|moncat.bow==0,NA,((moncat_sd.bow/moncat.bow)*100)))
  monthcat$moncat_RSE.spr <-  with(monthcat,ifelse(is.na(moncat.spr)|moncat.spr==0,NA,((moncat_sd.spr/moncat.spr)*100)))
  
  #estimates  for release
  monthcat$monrel.all <-  with(mdaycat.rev,mdaymr.all*days_month)
  monthcat$monrel.bot <-  with(mdaycat.rev,mdaymr.bot*days_month)
  monthcat$monrel.bnk <-  with(mdaycat.rev,mdaymr.bnk*days_month)
  monthcat$monrel.ice <-  with(mdaycat.rev,mdaymr.ice*days_month)
  monthcat$monrel.bow <-  with(mdaycat.rev,mdaymr.bow*days_month)
  monthcat$monrel.spr <-  with(mdaycat.rev,mdaymr.spr*days_month)
  #variances for release
  monthcat$monrel_var.all <-  with(mdaycat.rev,vdaymr.all*(days_month^2))
  monthcat$monrel_var.bot <-  with(mdaycat.rev,vdaymr.bot*(days_month^2))
  monthcat$monrel_var.bnk <-  with(mdaycat.rev,vdaymr.bnk*(days_month^2))
  monthcat$monrel_var.ice <-  with(mdaycat.rev,vdaymr.ice*(days_month^2))
  monthcat$monrel_var.bow <-  with(mdaycat.rev,vdaymr.bow*(days_month^2))
  monthcat$monrel_var.spr <-  with(mdaycat.rev,vdaymr.spr*(days_month^2))
  
  #standard deviations for release
  monthcat$monrel_sd.all <-  sqrt(monthcat$monrel_var.all)
  monthcat$monrel_sd.bot <-  sqrt(monthcat$monrel_var.bot)
  monthcat$monrel_sd.bnk <-  sqrt(monthcat$monrel_var.bnk)
  monthcat$monrel_sd.ice <-  sqrt(monthcat$monrel_var.ice)
  monthcat$monrel_sd.bow <-  sqrt(monthcat$monrel_var.bow)
  monthcat$monrel_sd.spr <-  sqrt(monthcat$monrel_var.spr)
  
  #coefficient of variations for release
  monthcat$monrel_cv.all <-  with(monthcat,ifelse(is.na(monrel.all)|monrel.all==0,NA,((monrel_sd.all/monrel.all)*100)))
  monthcat$monrel_cv.bot <-  with(monthcat,ifelse(is.na(monrel.bot)|monrel.bot==0,NA,((monrel_sd.bot/monrel.bot)*100)))
  monthcat$monrel_cv.bnk <-  with(monthcat,ifelse(is.na(monrel.bnk)|monrel.bnk==0,NA,((monrel_sd.bnk/monrel.bnk)*100)))
  monthcat$monrel_cv.ice <-  with(monthcat,ifelse(is.na(monrel.ice)|monrel.ice==0,NA,((monrel_sd.ice/monrel.ice)*100)))
  monthcat$monrel_cv.bow <-  with(monthcat,ifelse(is.na(monrel.bow)|monrel.bow==0,NA,((monrel_sd.bow/monrel.bow)*100)))
  monthcat$monrel_cv.spr <-  with(monthcat,ifelse(is.na(monrel.spr)|monrel.spr==0,NA,((monrel_sd.spr/monrel.spr)*100)))
  
   #estimates  for harvest
  monthcat$monhar.all <-  with(mdaycat.rev,mdaymh.all*days_month)
  monthcat$monhar.bot <-  with(mdaycat.rev,mdaymh.bot*days_month)
  monthcat$monhar.bnk <-  with(mdaycat.rev,mdaymh.bnk*days_month)
  monthcat$monhar.ice <-  with(mdaycat.rev,mdaymh.ice*days_month)
  monthcat$monhar.bow <-  with(mdaycat.rev,mdaymh.bow*days_month)
  monthcat$monhar.spr <-  with(mdaycat.rev,mdaymh.spr*days_month)
  #variances for harvest (numbers)
  monthcat$monhar_var.all <-  with(mdaycat.rev,vdaymh.all*(days_month^2))
  monthcat$monhar_var.bot <-  with(mdaycat.rev,vdaymh.bot*(days_month^2))
  monthcat$monhar_var.bnk <-  with(mdaycat.rev,vdaymh.bnk*(days_month^2))
  monthcat$monhar_var.ice <-  with(mdaycat.rev,vdaymh.ice*(days_month^2))
  monthcat$monhar_var.bow <-  with(mdaycat.rev,vdaymh.bow*(days_month^2))
  monthcat$monhar_var.spr <-  with(mdaycat.rev,vdaymh.spr*(days_month^2))
  
  #standard deviations for harvest
  monthcat$monhar_sd.all <-  sqrt(monthcat$monhar_var.all)
  monthcat$monhar_sd.bot <-  sqrt(monthcat$monhar_var.bot)
  monthcat$monhar_sd.bnk <-  sqrt(monthcat$monhar_var.bnk)
  monthcat$monhar_sd.ice <-  sqrt(monthcat$monhar_var.ice)
  monthcat$monhar_sd.bow <-  sqrt(monthcat$monhar_var.bow)
  monthcat$monhar_sd.spr <-  sqrt(monthcat$monhar_var.spr)
  
  #coefficient of variations for harvest
  monthcat$monhar_cv.all <-  with(monthcat,ifelse(is.na(monhar.all)|monhar.all==0,NA,((monhar_sd.all/monhar.all)*100)))
  monthcat$monhar_cv.bot <-  with(monthcat,ifelse(is.na(monhar.bot)|monhar.bot==0,NA,((monhar_sd.bot/monhar.bot)*100)))
  monthcat$monhar_cv.bnk <-  with(monthcat,ifelse(is.na(monhar.bnk)|monhar.bnk==0,NA,((monhar_sd.bnk/monhar.bnk)*100)))
  monthcat$monhar_cv.ice <-  with(monthcat,ifelse(is.na(monhar.ice)|monhar.ice==0,NA,((monhar_sd.ice/monhar.ice)*100)))
  monthcat$monhar_cv.bow <-  with(monthcat,ifelse(is.na(monhar.bow)|monhar.bow==0,NA,((monhar_sd.bow/monhar.bow)*100)))
  monthcat$monhar_cv.spr <-  with(monthcat,ifelse(is.na(monhar.spr)|monhar.spr==0,NA,((monhar_sd.spr/monhar.spr)*100)))
  
  #estimates  for harvest  (weight)
  monthcat$monwgt.all <-  with(mdaycat.rev,mdaymw.all*days_month)
  monthcat$monwgt.bot <-  with(mdaycat.rev,mdaymw.bot*days_month)
  monthcat$monwgt.bnk <-  with(mdaycat.rev,mdaymw.bnk*days_month)
  monthcat$monwgt.ice <-  with(mdaycat.rev,mdaymw.ice*days_month)
  monthcat$monwgt.bow <-  with(mdaycat.rev,mdaymw.bow*days_month)
  monthcat$monwgt.spr <-  with(mdaycat.rev,mdaymw.spr*days_month)
  #variances for harvest
  monthcat$monwgt_var.all <-  with(mdaycat.rev,vdaymw.all*(days_month^2))
  monthcat$monwgt_var.bot <-  with(mdaycat.rev,vdaymw.bot*(days_month^2))
  monthcat$monwgt_var.bnk <-  with(mdaycat.rev,vdaymw.bnk*(days_month^2))
  monthcat$monwgt_var.ice <-  with(mdaycat.rev,vdaymw.ice*(days_month^2))
  monthcat$monwgt_var.bow <-  with(mdaycat.rev,vdaymw.bow*(days_month^2))
  monthcat$monwgt_var.spr <-  with(mdaycat.rev,vdaymw.spr*(days_month^2))
  
  #standard deviations for harvest
  monthcat$monwgt_sd.all <-  sqrt(monthcat$monwgt_var.all)
  monthcat$monwgt_sd.bot <-  sqrt(monthcat$monwgt_var.bot)
  monthcat$monwgt_sd.bnk <-  sqrt(monthcat$monwgt_var.bnk)
  monthcat$monwgt_sd.ice <-  sqrt(monthcat$monwgt_var.ice)
  monthcat$monwgt_sd.bow <-  sqrt(monthcat$monwgt_var.bow)
  monthcat$monwgt_sd.spr <-  sqrt(monthcat$monwgt_var.spr)
  
  #coefficient of variations for harvest
  monthcat$monhar_RSE.all <-  with(monthcat,ifelse(is.na(monwgt.all)|monwgt.all==0,NA,((monwgt_sd.all/monwgt.all)*100)))
  monthcat$monhar_RSE.bot <-  with(monthcat,ifelse(is.na(monwgt.bot)|monwgt.bot==0,NA,((monwgt_sd.bot/monwgt.bot)*100)))
  monthcat$monhar_RSE.bnk <-  with(monthcat,ifelse(is.na(monwgt.bnk)|monwgt.bnk==0,NA,((monwgt_sd.bnk/monwgt.bnk)*100)))
  monthcat$monhar_RSE.ice <-  with(monthcat,ifelse(is.na(monwgt.ice)|monwgt.ice==0,NA,((monwgt_sd.ice/monwgt.ice)*100)))
  monthcat$monhar_RSE.bow <-  with(monthcat,ifelse(is.na(monwgt.bow)|monwgt.bow==0,NA,((monwgt_sd.bow/monwgt.bow)*100)))
  monthcat$monhar_RSE.spr <-  with(monthcat,ifelse(is.na(monwgt.spr)|monwgt.spr==0,NA,((monwgt_sd.spr/monwgt.spr)*100)))
  
  # Catch per unit efforts
  # standard deviations
  monthcat$sd_rcpe <- sqrt(mdaycat.rev$vdayrcpe)
  monthcat$sd_hcpe <- sqrt(mdaycat.rev$vdayhcpe)
  monthcat$sd_wcpe <- sqrt(mdaycat.rev$vdaywcpe)
  monthcat$sd_ccpe <- sqrt(mdaycat.rev$vdayccpe)
  monthcat$cws.sd_rcpe <- sqrt(mdaycat.rev$cws.vdayrcpe)
  monthcat$cws.sd_hcpe <- sqrt(mdaycat.rev$cws.vdayhcpe)
  monthcat$cws.sd_wcpe <- sqrt(mdaycat.rev$cws.vdaywcpe)
  monthcat$cws.sd_ccpe <- sqrt(mdaycat.rev$cws.vdayccpe)
  
   # coefficient of variation
  monthcat$RSE_rcpe <-ifelse(is.na(mdaycat.rev$mdayrcpe)|mdaycat.rev$mdayrcpe==0,NA,((monthcat$sd_rcpe/mdaycat.rev$mdayrcpe)*100)) 
  monthcat$RSE_hcpe <-ifelse(is.na(mdaycat.rev$mdayhcpe)|mdaycat.rev$mdayhcpe==0,NA,((monthcat$sd_hcpe/mdaycat.rev$mdayhcpe)*100)) 
  monthcat$RSE_wcpe <-ifelse(is.na(mdaycat.rev$mdaywcpe)|mdaycat.rev$mdaywcpe==0,NA,((monthcat$sd_wcpe/mdaycat.rev$mdaywcpe)*100)) 
  monthcat$RSE_ccpe <- ifelse(is.na(mdaycat.rev$mdayccpe)|mdaycat.rev$mdayccpe==0,NA,((monthcat$sd_ccpe/mdaycat.rev$mdayccpe)*100))
  monthcat$cws.RSE_rcpe <-ifelse(is.na(mdaycat.rev$cws.mdayrcpe)|mdaycat.rev$cws.mdayrcpe==0,NA,((monthcat$cws.sd_rcpe/mdaycat.rev$cws.mdayrcpe)*100)) 
  monthcat$cws.RSE_hcpe <-ifelse(is.na(mdaycat.rev$cws.mdayhcpe)|mdaycat.rev$cws.mdayhcpe==0,NA,((monthcat$cws.sd_hcpe/mdaycat.rev$cws.mdayhcpe)*100)) 
  monthcat$cws.RSE_wcpe <-ifelse(is.na(mdaycat.rev$cws.mdaywcpe)|mdaycat.rev$cws.mdaywcpe==0,NA,((monthcat$cws.sd_wcpe/mdaycat.rev$cws.mdaywcpe)*100)) 
  monthcat$cws.RSE_ccpe <-ifelse(is.na(mdaycat.rev$cws.mdayccpe)|mdaycat.rev$cws.mdayccpe==0,NA,((monthcat$cws.sd_ccpe/mdaycat.rev$cws.mdayccpe)*100)) 
  
  survey.dayz.gt<-cast(survey.dayz.melt,...~day_type)
  survey.dayz.gt$nh_1<-with(survey.dayz.gt,ifelse(weekday>weekend,weekend-1,weekday-1))
  survey.dayz.gt$sumnh<-survey.dayz.gt[,3]+survey.dayz.gt[,4]
  survey.dayz.gt$DF<- round(((survey.dayz.gt$sumnh - survey.dayz.gt$nh_1)/2),0)+survey.dayz.gt$nh_1
  survey.dayz.gt$T_CL<-matrix(0,length(survey.dayz.gt$DF))
  survey.dayz.gt$T_CL[which(survey.dayz.gt$DF>0 & !is.na(survey.dayz.gt$DF))]<- qt(p=.95, df = survey.dayz.gt$DF[which(survey.dayz.gt$DF>0 & !is.na(survey.dayz.gt$DF))]) 
   
  # 90% CI
  monthcat$moncat_CI.all <- monthcat$moncat_sd.all *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$moncat_CI.bot <- monthcat$moncat_sd.bot *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$moncat_CI.bnk <- monthcat$moncat_sd.bnk *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$moncat_CI.ice <- monthcat$moncat_sd.ice *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$moncat_CI.bow <- monthcat$moncat_sd.bow *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$moncat_CI.spr <- monthcat$moncat_sd.spr *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  
  monthcat$monrel_CI.all <- monthcat$monrel_sd.all *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monrel_CI.bot <- monthcat$monrel_sd.bot *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monrel_CI.bnk <- monthcat$monrel_sd.bnk *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monrel_CI.ice <- monthcat$monrel_sd.ice *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monrel_CI.bow <- monthcat$monrel_sd.bow *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monrel_CI.spr <- monthcat$monrel_sd.spr *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
 
  monthcat$monhar_CI.all <- monthcat$monhar_sd.all *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monhar_CI.bot <- monthcat$monhar_sd.bot *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monhar_CI.bnk <- monthcat$monhar_sd.bnk *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monhar_CI.ice <- monthcat$monhar_sd.ice *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monhar_CI.bow <- monthcat$monhar_sd.bow *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monhar_CI.spr <- monthcat$monhar_sd.spr *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  
  monthcat$monwgt_CI.all <- monthcat$monwgt_sd.all *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monwgt_CI.bot <- monthcat$monwgt_sd.bot *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monwgt_CI.bnk <- monthcat$monwgt_sd.bnk *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monwgt_CI.ice <- monthcat$monwgt_sd.ice *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$monwgt_CI.bow <- monthcat$monwgt_sd.bow *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  monthcat$monwgt_CI.spr <- monthcat$monwgt_sd.spr *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)]
  
  monthcat$cv_rcpe_CI  <- monthcat$sd_rcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cv_hcpe_CI  <- monthcat$sd_hcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cv_wcpe_CI  <- monthcat$sd_wcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cv_ccpe_CI  <- monthcat$sd_ccpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  
  monthcat$cws.cv_rcpe_CI  <- monthcat$cws.sd_rcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cws.cv_hcpe_CI  <- monthcat$cws.sd_hcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cws.cv_wcpe_CI  <- monthcat$cws.sd_wcpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  monthcat$cws.cv_ccpe_CI  <- monthcat$cws.sd_ccpe *survey.dayz.gt$T_CL[match(monthcat$Month,survey.dayz.gt$Month)] 
  
  monthcat<-as.data.frame(monthcat)
  monthcat.rev<-merge(monthcat,survey.dayz.gt,by="Month")
  
  measure.vars<-c("moncat.all","moncat.bot","moncat.bnk","moncat.ice","moncat.bow","moncat.spr",
  "moncat_var.all","moncat_var.bot","moncat_var.bnk","moncat_var.ice","moncat_var.bow","moncat_var.spr",
  "monrel.all","monrel.bot","monrel.bnk","monrel.ice","monrel.bow","monrel.spr", "monrel_var.all",
  "monrel_var.bot","monrel_var.bnk","monrel_var.ice","monrel_var.bow","monrel_var.spr","monhar.all",
  "monhar.bot","monhar.bnk","monhar.ice","monhar.bow","monhar.spr","monhar_var.all","monhar_var.bot",
  "monhar_var.bnk","monhar_var.ice","monhar_var.bow","monhar_var.spr","monwgt.all","monwgt.bot",
  "monwgt.bnk","monwgt.ice","monwgt.bow","monwgt.spr","monwgt_var.all","monwgt_var.bot","monwgt_var.bnk",
  "monwgt_var.ice","monwgt_var.bow","monwgt_var.spr","DF")
  
  monthcat.m<-melt(monthcat.rev,id.var=c("species"),measure.var=measure.vars)   #reshape monthly catches
  
  # Yearly data
  yearcat<-cast(monthcat.m, species~variable, sum)  #sum monthly values by species
  
  yearcat_namez<-names(yearcat)
  yearcat_namez<-gsub("moncat","yearcat",yearcat_namez)     #replace the header names
  yearcat_namez<-gsub("monrel","yearrel",yearcat_namez)
  yearcat_namez<-gsub("monhar","yearhar",yearcat_namez)
  yearcat_namez<-gsub("monwgt","yearwgt",yearcat_namez)
  names(yearcat)<-yearcat_namez
  yearcat$T_CL<-0
  yearcat$T_CL[which(yearcat$DF>0 & !is.na(yearcat$DF))]<- qt(p=.95, df = yearcat$DF[which(yearcat$DF>0 & !is.na(yearcat$DF))]) 
  
  #SE 
  yearcat$yearcat_se.all<- sqrt(yearcat$yearcat_var.all)
  yearcat$yearcat_se.bot<- sqrt(yearcat$yearcat_var.bot)
  yearcat$yearcat_se.bnk<- sqrt(yearcat$yearcat_var.bnk)
  yearcat$yearcat_se.ice<- sqrt(yearcat$yearcat_var.ice)
  yearcat$yearcat_se.bow<- sqrt(yearcat$yearcat_var.bow)
  yearcat$yearcat_se.spr<- sqrt(yearcat$yearcat_var.spr)
  
  yearcat$yearrel_se.all<- sqrt(yearcat$yearrel_var.all)
  yearcat$yearrel_se.bot<- sqrt(yearcat$yearrel_var.bot)
  yearcat$yearrel_se.bnk<- sqrt(yearcat$yearrel_var.bnk)
  yearcat$yearrel_se.ice<- sqrt(yearcat$yearrel_var.ice)
  yearcat$yearrel_se.bow<- sqrt(yearcat$yearrel_var.bow)
  yearcat$yearrel_se.spr<- sqrt(yearcat$yearrel_var.spr)
  
  yearcat$yearhar_se.all<- sqrt(yearcat$yearhar_var.all)
  yearcat$yearhar_se.bot<- sqrt(yearcat$yearhar_var.bot)
  yearcat$yearhar_se.bnk<- sqrt(yearcat$yearhar_var.bnk)
  yearcat$yearhar_se.ice<- sqrt(yearcat$yearhar_var.ice)
  yearcat$yearhar_se.bow<- sqrt(yearcat$yearhar_var.bow)
  yearcat$yearhar_se.spr<- sqrt(yearcat$yearhar_var.spr)
  
  yearcat$yearwgt_se.all<- sqrt(yearcat$yearwgt_var.all)
  yearcat$yearwgt_se.bot<- sqrt(yearcat$yearwgt_var.bot)
  yearcat$yearwgt_se.bnk<- sqrt(yearcat$yearwgt_var.bnk)
  yearcat$yearwgt_se.ice<- sqrt(yearcat$yearwgt_var.ice)
  yearcat$yearwgt_se.bow<- sqrt(yearcat$yearwgt_var.bow)
  yearcat$yearwgt_se.spr<- sqrt(yearcat$yearwgt_var.spr)
  
  ####  90% CI Calculations
  yearcat$yearcat_CI.all<- yearcat$yearcat_se.all*yearcat$T_CL
  yearcat$yearcat_CI.bot<- yearcat$yearcat_se.bot*yearcat$T_CL
  yearcat$yearcat_CI.bnk<- yearcat$yearcat_se.bnk*yearcat$T_CL
  yearcat$yearcat_CI.ice<- yearcat$yearcat_se.ice*yearcat$T_CL
  yearcat$yearcat_CI.bow<- yearcat$yearcat_se.bow*yearcat$T_CL
  yearcat$yearcat_CI.spr<- yearcat$yearcat_se.spr*yearcat$T_CL
  
  yearcat$yearrel_CI.all<- yearcat$yearrel_se.all*yearcat$T_CL
  yearcat$yearrel_CI.bot<- yearcat$yearrel_se.bot*yearcat$T_CL
  yearcat$yearrel_CI.bnk<- yearcat$yearrel_se.bnk*yearcat$T_CL
  yearcat$yearrel_CI.ice<- yearcat$yearrel_se.ice*yearcat$T_CL
  yearcat$yearrel_CI.bow<- yearcat$yearrel_se.bow*yearcat$T_CL
  yearcat$yearrel_CI.spr<- yearcat$yearrel_se.spr*yearcat$T_CL
  
  yearcat$yearhar_CI.all<- yearcat$yearhar_se.all*yearcat$T_CL
  yearcat$yearhar_CI.bot<- yearcat$yearhar_se.bot*yearcat$T_CL
  yearcat$yearhar_CI.bnk<- yearcat$yearhar_se.bnk*yearcat$T_CL
  yearcat$yearhar_CI.ice<- yearcat$yearhar_se.ice*yearcat$T_CL
  yearcat$yearhar_CI.bow<- yearcat$yearhar_se.bow*yearcat$T_CL
  yearcat$yearhar_CI.spr<- yearcat$yearhar_se.spr*yearcat$T_CL
  
  yearcat$yearwgt_CI.all<- yearcat$yearwgt_se.all*yearcat$T_CL
  yearcat$yearwgt_CI.bot<- yearcat$yearwgt_se.bot*yearcat$T_CL
  yearcat$yearwgt_CI.bnk<- yearcat$yearwgt_se.bnk*yearcat$T_CL
  yearcat$yearwgt_CI.ice<- yearcat$yearwgt_se.ice*yearcat$T_CL
  yearcat$yearwgt_CI.bow<- yearcat$yearwgt_se.bow*yearcat$T_CL
  yearcat$yearwgt_CI.spr<- yearcat$yearwgt_se.spr*yearcat$T_CL
  
  ####  RSE 
  yearcat$yearcat_RSE.all<- with(yearcat,ifelse(is.na(yearcat.all)|yearcat.all==0,NA,((yearcat_se.all/yearcat.all)*100)))
  yearcat$yearcat_RSE.bot<- with(yearcat,ifelse(is.na(yearcat.bot)|yearcat.bot==0,NA,((yearcat_se.bot/yearcat.bot)*100)))
  yearcat$yearcat_RSE.bnk<- with(yearcat,ifelse(is.na(yearcat.bnk)|yearcat.bnk==0,NA,((yearcat_se.bnk/yearcat.bnk)*100)))
  yearcat$yearcat_RSE.ice<- with(yearcat,ifelse(is.na(yearcat.ice)|yearcat.ice==0,NA,((yearcat_se.ice/yearcat.ice)*100)))
  yearcat$yearcat_RSE.bow<- with(yearcat,ifelse(is.na(yearcat.bow)|yearcat.bow==0,NA,((yearcat_se.bow/yearcat.bow)*100)))
  yearcat$yearcat_RSE.spr<- with(yearcat,ifelse(is.na(yearcat.spr)|yearcat.spr==0,NA,((yearcat_se.spr/yearcat.spr)*100)))
  
  yearcat$yearrel_RSE.all<- with(yearcat,ifelse(is.na(yearrel.all)|yearrel.all==0,NA,((yearrel_se.all/yearrel.all)*100)))
  yearcat$yearrel_RSE.bot<- with(yearcat,ifelse(is.na(yearrel.bot)|yearrel.bot==0,NA,((yearrel_se.bot/yearrel.bot)*100)))
  yearcat$yearrel_RSE.bnk<- with(yearcat,ifelse(is.na(yearrel.bnk)|yearrel.bnk==0,NA,((yearrel_se.bnk/yearrel.bnk)*100)))
  yearcat$yearrel_RSE.ice<- with(yearcat,ifelse(is.na(yearrel.ice)|yearrel.ice==0,NA,((yearrel_se.ice/yearrel.ice)*100)))
  yearcat$yearrel_RSE.bow<- with(yearcat,ifelse(is.na(yearrel.bow)|yearrel.bow==0,NA,((yearrel_se.bow/yearrel.bow)*100)))
  yearcat$yearrel_RSE.spr<- with(yearcat,ifelse(is.na(yearrel.spr)|yearrel.spr==0,NA,((yearrel_se.spr/yearrel.spr)*100)))
  
  yearcat$yearhar_RSE.all<- with(yearcat,ifelse(is.na(yearhar.all)|yearhar.all==0,NA,((yearhar_se.all/yearhar.all)*100)))
  yearcat$yearhar_RSE.bot<- with(yearcat,ifelse(is.na(yearhar.bot)|yearhar.bot==0,NA,((yearhar_se.bot/yearhar.bot)*100)))
  yearcat$yearhar_RSE.bnk<- with(yearcat,ifelse(is.na(yearhar.bnk)|yearhar.bnk==0,NA,((yearhar_se.bnk/yearhar.bnk)*100)))
  yearcat$yearhar_RSE.ice<- with(yearcat,ifelse(is.na(yearhar.ice)|yearhar.ice==0,NA,((yearhar_se.ice/yearhar.ice)*100)))
  yearcat$yearhar_RSE.bow<- with(yearcat,ifelse(is.na(yearhar.bow)|yearhar.bow==0,NA,((yearhar_se.bow/yearhar.bow)*100)))
  yearcat$yearhar_RSE.spr<- with(yearcat,ifelse(is.na(yearhar.spr)|yearhar.spr==0,NA,((yearhar_se.spr/yearhar.spr)*100)))
  
  yearcat$yearwgt_RSE.all<- with(yearcat,ifelse(is.na(yearwgt.all)|yearwgt.all==0,NA,((yearwgt_se.all/yearwgt.all)*100)))
  yearcat$yearwgt_RSE.bot<- with(yearcat,ifelse(is.na(yearwgt.bot)|yearwgt.bot==0,NA,((yearwgt_se.bot/yearwgt.bot)*100)))
  yearcat$yearwgt_RSE.bnk<- with(yearcat,ifelse(is.na(yearwgt.bnk)|yearwgt.bnk==0,NA,((yearwgt_se.bnk/yearwgt.bnk)*100)))
  yearcat$yearwgt_RSE.ice<- with(yearcat,ifelse(is.na(yearwgt.ice)|yearwgt.ice==0,NA,((yearwgt_se.ice/yearwgt.ice)*100)))
  yearcat$yearwgt_RSE.bow<- with(yearcat,ifelse(is.na(yearwgt.bow)|yearwgt.bow==0,NA,((yearwgt_se.bow/yearwgt.bow)*100)))
  yearcat$yearwgt_RSE.spr<- with(yearcat,ifelse(is.na(yearwgt.spr)|yearwgt.spr==0,NA,((yearwgt_se.spr/yearwgt.spr)*100)))
  
  # CPE                 
  final.angler.effort<-monthly_effort_all[,c("Month","mmon.all")]
  year_CPE<-merge(mdaycat.rev[,c("Month","species","mdayccpe","mdayrcpe","mdayhcpe","mdaywcpe",
        "cws.mdayccpe","cws.mdayrcpe","cws.mdayhcpe","cws.mdaywcpe","vdayccpe","vdayrcpe","vdayhcpe","vdaywcpe",
        "cws.vdayccpe","cws.vdayrcpe","cws.vdayhcpe","cws.vdaywcpe")],final.angler.effort, by="Month")
  year_CPE<-merge(year_CPE,survey.dayz.gt,by="Month")      
  year_CPE$yr_ccpe<- year_CPE$mdayccpe*year_CPE$mmon.all
  year_CPE$yr_rcpe<- year_CPE$mdayrcpe*year_CPE$mmon.all
  year_CPE$yr_hcpe<- year_CPE$mdayhcpe*year_CPE$mmon.all
  year_CPE$yr_wcpe<- year_CPE$mdaywcpe*year_CPE$mmon.all
  year_CPE$cws.yr_ccpe<- year_CPE$cws.mdayccpe*year_CPE$mmon.all
  year_CPE$cws.yr_rcpe<- year_CPE$cws.mdayrcpe*year_CPE$mmon.all
  year_CPE$cws.yr_hcpe<- year_CPE$cws.mdayhcpe*year_CPE$mmon.all
  year_CPE$cws.yr_wcpe<- year_CPE$cws.mdaywcpe*year_CPE$mmon.all
  year_CPE$yr_vccpe<- year_CPE$vdayccpe*year_CPE$mmon.all
  year_CPE$yr_vrcpe<- year_CPE$vdayrcpe*year_CPE$mmon.all
  year_CPE$yr_vhcpe<- year_CPE$vdayhcpe*year_CPE$mmon.all
  year_CPE$yr_vwcpe<- year_CPE$vdaywcpe*year_CPE$mmon.all
  year_CPE$cws.yr_vccpe<- year_CPE$cws.vdayccpe*year_CPE$mmon.all
  year_CPE$cws.yr_vrcpe<- year_CPE$cws.vdayrcpe*year_CPE$mmon.all
  year_CPE$cws.yr_vhcpe<- year_CPE$cws.vdayhcpe*year_CPE$mmon.all
  year_CPE$cws.yr_vwcpe<- year_CPE$cws.vdaywcpe*year_CPE$mmon.all
  year_CPE<-year_CPE[,c( "Month","species","yr_ccpe","yr_rcpe","yr_hcpe","yr_wcpe","cws.yr_ccpe","cws.yr_rcpe","cws.yr_hcpe","cws.yr_wcpe",
  "yr_vccpe","yr_vrcpe","yr_vhcpe","yr_vwcpe","cws.yr_vccpe","cws.yr_vrcpe","cws.yr_vhcpe","cws.yr_vwcpe","DF")]
  
  measure.vars=names(year_CPE)[-c(1,2)]      
  year_CPE.m<-melt(year_CPE,id.var="species",measure.var=measure.vars)
  year_CPE.rev<-cast(year_CPE.m,species~variable,sum)
  year_CPE.rev[,c(2:(ncol(year_CPE.rev)-1))]<- year_CPE.rev[,c(2:(ncol(year_CPE.rev)-1))]/yearly_effort_all$myear.all
  yearDF<-ifelse(year_CPE.rev$DF<=30,year_CPE.rev$DF,30)
  yrtval<-matrix(NA,length(yearDF),1)
  yrtval[which(yearDF>0 & !is.na(yearDF))]<- qt(p=.95, df = yearDF[which(yearDF>0 & !is.na(yearDF))])
  
  #SE calculations
  year_CPE.rev$yr_SEccpe<-sqrt(year_CPE.rev$yr_vccpe)
  year_CPE.rev$yr_SErcpe<-sqrt(year_CPE.rev$yr_vrcpe)
  year_CPE.rev$yr_SEhcpe<-sqrt(year_CPE.rev$yr_vhcpe)
  year_CPE.rev$yr_SEwcpe<-sqrt(year_CPE.rev$yr_vwcpe)
  year_CPE.rev$cws.yr_SEccpe<-sqrt(year_CPE.rev$cws.yr_vccpe)
  year_CPE.rev$cws.yr_SErcpe<-sqrt(year_CPE.rev$cws.yr_vrcpe)
  year_CPE.rev$cws.yr_SEhcpe<-sqrt(year_CPE.rev$cws.yr_vhcpe)
  year_CPE.rev$cws.yr_SEwcpe<-sqrt(year_CPE.rev$cws.yr_vwcpe)
  
  # 90% CI
  year_CPE.rev$yr_CIccpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$yr_CIrcpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$yr_CIhcpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$yr_CIwcpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$cws.yr_CIccpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$cws.yr_CIrcpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$cws.yr_CIhcpe<-year_CPE.rev$yr_SEccpe*yrtval
  year_CPE.rev$cws.yr_CIwcpe<-year_CPE.rev$yr_SEccpe*yrtval
  
  # RSE           
  year_CPE.rev$yr_RSEccpe<-with(year_CPE.rev,ifelse(is.na(yr_ccpe)|yr_ccpe==0,0,((yr_SEccpe/yr_ccpe)*100)))
  year_CPE.rev$yr_RSErcpe<- with(year_CPE.rev,ifelse(is.na(yr_rcpe)|yr_rcpe==0,0,((yr_SErcpe/yr_rcpe)*100)))
  year_CPE.rev$yr_RSEhcpe<- with(year_CPE.rev,ifelse(is.na(yr_hcpe)|yr_hcpe==0,0,((yr_SEhcpe/yr_hcpe)*100)))
  year_CPE.rev$yr_RSEwcpe<- with(year_CPE.rev,ifelse(is.na(yr_wcpe)|yr_wcpe==0,0,((yr_SEwcpe/yr_wcpe)*100)))
  
  year_CPE.rev$cws.yr_RSEccpe<-with(year_CPE.rev,ifelse(is.na(cws.yr_ccpe)|cws.yr_ccpe==0,0,((cws.yr_SEccpe/cws.yr_ccpe)*100)))
  year_CPE.rev$cws.yr_RSErcpe<- with(year_CPE.rev,ifelse(is.na(cws.yr_rcpe)|cws.yr_rcpe==0,0,((cws.yr_SErcpe/cws.yr_rcpe)*100)))
  year_CPE.rev$cws.yr_RSEhcpe<- with(year_CPE.rev,ifelse(is.na(cws.yr_hcpe)|cws.yr_hcpe==0,0,((cws.yr_SEhcpe/cws.yr_hcpe)*100)))
  year_CPE.rev$cws.yr_RSEwcpe<- with(year_CPE.rev,ifelse(is.na(cws.yr_wcpe)|cws.yr_wcpe==0,0,((cws.yr_SEwcpe/cws.yr_wcpe)*100)))
  
  
  harvest.tables<-list(Mean.daily.catch=mdaycat.rev,Monthly.catch=monthcat,Yearly.catch=yearcat,Yearly.CPE=year_CPE.rev)
  }else{harvest.tables<-"no fish caught"}
  out$pressure.tables<-pressure.tables
  out$harvest.tables<-harvest.tables
  
  return(out)
}  