# source("creel_schedule_functions-04-18-14.R")
# 
# shinyServer(function(input, output, session) {
#   
#   # input$date and others are Date objects. When outputting
#   # text, we need to convert to character; otherwise it will
#   # print an integer rather than a date.
#   
# 
#   
#   
#   
#   output$dto  <- renderDataTable({
#     
#     holidayz<-as.character(c(input$specialdays1,input$specialdays2,input$specialdays3,input$specialdays4,input$specialdays5,input$specialdays6,input$specialdays7,input$specialdays8,input$specialdays9))
#     
#     holidayzgrp<-as.numeric(c(input$specialdaysgrp1,input$specialdaysgrp2,input$specialdaysgrp3,input$specialdaysgrp4,input$specialdaysgrp5,input$specialdaysgrp6,input$specialdaysgrp7,input$specialdaysgrp8,input$specialdaysgrp9))
#    
#     
#     params<-list(start.date=input$dateRange[1],
#                    end.date=input$dateRange[2],
#                    day.types=c("weekend","weekday","highuse"),
#                    periods = 2,
#                    period.probs = c(0.5,0.5),
#                    weekdays=input$weekdays,
#                    weekends=input$weekends,
#                    specialdays = input$special,
#                    counts=input$counts,
#                    count.length=60,
#                    lake.sections = 1,
#                    lake.section.probs = NULL,
#                    exclude.days = NULL,
#                    holidays = holidayz,
#                    holiday.grp=holidayzgrp,
#                    sampling.times=data.frame(month=1:12,
#                                              start.period.1=c("08:30","08:00","07:00","07:00","06:30","06:00","06:00","06:30","07:00","08:00","07:30","08:00"),
#                                              start.period.2=c("13:00","13:00","13:00","13:30","13:30","13:30","13:30","13:30","13:30","13:00","12:00","12:30"),    
#                                              end=c("17:30","18:00","19:00","20:00","20:30","21:00","21:00","20:30","20:00","19:00","17:30","17:00")),
#                    seed=as.numeric(input$seed)
#                    
#       )
#     
#     
#        tab<-day.within.month.schedule.2(params=params,holiday.type=input$specialdaytype,add.hour=input$checkGroup)
#        tab
#     
#   })
#   
#   output$downloadData <- downloadHandler(
#     filename = function() { 
#       paste(dto, '.csv', sep='') 
#     },
#     content = function(file) {
#       write.csv(datasetInput(), file)
#     }
#   )
#   
# })


source("creel_schedule_functions-04-18-14.R")

shinyServer(function(input, output, session) {
  
  datasetInput <- reactive({
    holidayz<-as.character(c(input$specialdays1,input$specialdays2,input$specialdays3,input$specialdays4,input$specialdays5,input$specialdays6,input$specialdays7,input$specialdays8,input$specialdays9))
    
    holidayzgrp<-as.numeric(c(input$specialdaysgrp1,input$specialdaysgrp2,input$specialdaysgrp3,input$specialdaysgrp4,input$specialdaysgrp5,input$specialdaysgrp6,input$specialdaysgrp7,input$specialdaysgrp8,input$specialdaysgrp9))
    
    
    params<-list(start.date=input$dateRange[1],
                 end.date=input$dateRange[2],
                 day.types=c("weekend","weekday","highuse"),
                 periods = 2,
                 period.probs = c(0.5,0.5),
                 weekdays=input$weekdays,
                 weekends=input$weekends,
                 specialdays = input$special,
                 counts=input$counts,
                 count.length=60,
                 lake.sections = 1,
                 lake.section.probs = NULL,
                 exclude.days = NULL,
                 holidays = holidayz,
                 holiday.grp=holidayzgrp,
                 sampling.times=data.frame(month=1:12,
                                           start.period.1=c("08:30","08:00","07:00","07:00","06:30","06:00","06:00","06:30","07:00","08:00","07:30","08:00"),
                                           start.period.2=c("13:00","13:00","13:00","13:30","13:30","13:30","13:30","13:30","13:30","13:00","12:00","12:30"),    
                                           end=c("17:30","18:00","19:00","20:00","20:30","21:00","21:00","20:30","20:00","19:00","17:30","17:00")),
                 seed=as.numeric(input$seed)
                 
    )
    
    
    tab<-day.within.month.schedule.2(params=params,holiday.type=input$specialdaytype,add.hour=input$checkGroup)
    tab
  })  
  
  
  
  output$dto  <- renderDataTable({
    datasetInput() 
  })
  
  output$input_type_text <- renderText({
    input$lakename
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$lakename,'schedule-', format(input$dateRange[1],"%Y"), '.csv', sep="")
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
    
  )
  
  
}
)
