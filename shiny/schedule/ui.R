
shinyUI(fluidPage( 

  
  fluidRow(
    column(4,wellPanel(h2("Parameters"),
                       
             textInput("lakename","Lake Name", ""),
             
            numericInput("seed", "Randomization seed:", 51105110),
            
           dateRangeInput('dateRange',
                          label = 'Date range input: yyyy-mm-dd',
                          start = as.Date(paste(format(Sys.Date(),"%Y"),"-04-01",sep="")), end =  as.Date(paste(format(Sys.Date(),"%Y"),"-10-31",sep=""))),
           
           sliderInput("weekdays", "Weekdays per period:", 
                       min=1, max=9, value=5,step=1),
           
           sliderInput("weekends", "Weekends per period:", 
                       min=1, max=4, value=4,step=1),

           
           sliderInput("counts", "Counts per shift:", 
                       min=1, max=4, value=2,step=1),
           
           checkboxGroupInput("checkGroup", 
                              label = h4("Add hour"), 
                              choices = list("TRUE" = TRUE, 
                                             "FALSE" = FALSE),
                              selected = TRUE),
           
           
           fluidRow(h3("Special Dates"),
           column(12,
                     selectInput("specialdaytype", "Choose a special day type:", 
                       choices = c("holiday", "special")),
                  sliderInput("special", "Special days per period:", 
                       min=0, max=3, value=1,step=1)
                  ),
                    
             column(6,
                textInput("specialdays1","Date 1", "2017-05-23")
                ),
             column(6,
                textInput("specialdaysgrp1","Group 1", "1")
                )   
             ),
            fluidRow(
             column(6,
                textInput("specialdays2","Date 2", "2017-05-24")
                ),
             column(6,
                textInput("specialdaysgrp2","Group 2", "1")
                )   
             ),
           
            fluidRow(
             column(6,
                textInput("specialdays3","Date 3", "2017-05-25")
                ),
             column(6,
                textInput("specialdaysgrp3","Group 3", "1")
                )   
             ),
            fluidRow(
             column(6,
                textInput("specialdays4","Date 4", "2017-07-03")
                ),
             column(6,
                textInput("specialdaysgrp4","Group 4", "2")
                )   
             ),
           
            fluidRow(
             column(6,
                textInput("specialdays5","Date 5", "2017-07-04")
                ),
             column(6,
                textInput("specialdaysgrp5","Group 5", "2")
                )   
             ),
            fluidRow(
             column(6,
                textInput("specialdays6","Date 6", "2017-07-05")
                ),
             column(6,
                textInput("specialdaysgrp6","Group 6", "2")
                )   
             ),
           
             fluidRow(
             column(6,
                textInput("specialdays7","Date 7", "2017-09-05")
                ),
             column(6,
                textInput("specialdaysgrp7","Group 7", "3")
                )   
             ),
           
            fluidRow(
             column(6,
                textInput("specialdays8","Date 8", "2017-09-06")
                ),
             column(6,
                textInput("specialdaysgrp8","Group 8", "3")
                )   
             ),
            fluidRow(
             column(6,
                textInput("specialdays9","Date 9", "2017-09-07")
                ),
             column(6,
                textInput("specialdaysgrp9","Group 9", "3")
                )   
             ),
           fluidRow(
             column(6,
                    textInput("specialdays10","Date 10", "None")
             ),
             column(6,
                    textInput("specialdaysgrp10","Group 10", "None")
             )   
           ),
           fluidRow(
             column(6,
                    textInput("specialdays11","Date 11", "None")
             ),
             column(6,
                    textInput("specialdaysgrp1","Group 11", "None")
             )   
           ),
           
           fluidRow(
             column(4,
                    submitButton("Submit")
             ),
             column(8,
                    downloadButton('downloadData', 'Download schedule')
             )   
           )
           
    )
    ),
    column(8,
           h2(verbatimTextOutput("input_type_text")),
           dataTableOutput('dto')
    )
  )
))


