# Scheduler App

There are several controls with this app that the user can modify.  Currently, the app assumes two periods (a morning [sunrise to 1330] and and an afternoon shift[1330 to sunset]) and  counts that take sixty minutes.  
Sunrise and sunset times are based on Kearney, Nebraska. 

If more days are chosen than actually exist during a month, then the app defaults to choosing all that are available.  

## User Controls 
Lake Name: Add this to add the name of the lake to the file download
Randomization seed:  This allows the user to regenerate the same schedule based on the seed
Date range:  The start and end date of the creel
Weekdays per period:  1 to 9.  Given this app is based on two periods that means a 2 to 18 weekdays per month. 
Weekends per period:  1 to 4.  Given this app is based on two periods that means a 2 to 8 weekdays per month. 
Counts per shift:  1 to 4.  The number of instantaneous counts per creel shift. 
Add hour:  Should an hour be added on to the end of the creel shift.

### Special dates
This sets up the use of special dates used in the creel.  'Holiday' will treat the special days as a weekend in the generation and 'Special' treats these as a high-use strata.
If special is chosen, then how many special days should be chosen. If holiday is chosen, then selecting does not change anything. 


Listing of special/holiday dates. 
If you are choosing holiday, then only list the Federal Holiday.  You do not need to worry about adjacent days.
If you are choosing special, then list the special days including weekends that are going to be grouped into your high use strata.  The groups represent how adjacent high-use groups should be linked together.  


After setting your parameters, hit submit to regenerate a schedule.  

Click download schedule, to download a file in '.csv' format
```R
library(shiny)

# Easiest way is to use runGitHub
runGitHub("shiny_example", "rstudio")

# Run a tar or zip file directly
runUrl("https://github.com/rstudio/shiny_example/archive/master.tar.gz")
runUrl("https://github.com/rstudio/shiny_example/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:

```R
# First clone the repository with git. If you have cloned it into
# ~/shiny_example, first go to that directory, then use runApp().
setwd("~/shiny_example")
runApp()
```


To run a Shiny app from a subdirectory in the repo or zip file, you can use the `subdir` argument. This repository happens to contain another copy of the app in `inst/shinyapp/`.

```R
runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")

runUrl("https://github.com/rstudio/shiny_example/archive/master.tar.gz",
  subdir = "inst/shinyapp/")
```
