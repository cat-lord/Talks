
This is a simple [shiny](http://shiny.rstudio.com) app showing repeat locations of crime recorded by Greater Manchester Police during December 2015. 
The open crime data are available from [data.police.uk](data.police.uk)


Run the app locally in your R session with:

```
library(shiny)
runGitHub("cat-lord/talks", subdir = "ICIAC16/repeat_locations/")
```

... but make sure you've installed the shiny, rgdal and leaflet R packages first

```
install.packages(c("shiny", "rgdal", "shiny", ""))
```
