# load the shiny, rgdal and leaflet packages
library(shiny) ; library(rgdal) ; library(leaflet) 

# read the Greater Manchester Police crime data
df <- read.csv("repeat_locations_Dec15.csv", header = T)

# read the district boundary vector layer
boundary <- readOGR("manchester.geojson", "OGRGeoJSON")

ui <- fluidPage(
  # add a title
  titlePanel("Repeat locations"),
                # add a layout
                sidebarLayout(
                  sidebarPanel(width = 3,
                               # add a user input
                               radioButtons(
                                 # inputId is name used to access the value
                                 inputId = "category",
                                 # text displayed above input widget
                                 label = "Select a crime category:",
                                 # list of items to display
                                 choices = levels(df$category),
                                 # initially selected item
                                 selected = "Theft from the person")),
                  # display output(s)
                  mainPanel(
                    # add an output
                    leafletOutput(outputId = "map", width = "100%", height = "530"),
                    br(),
                    # add some HTML
                    tags$div(class = "header", checked = NA,
                             tags$strong("Data sources"),
                             tags$li(tags$a(href="https://data.police.uk", 
                                            "GMP recorded crime (Dec 2015)")),
                             tags$li(tags$a(href="https://data.gov.uk/data/map-based-search",
                                            "Manchester District"))))
                ) 
)

server <- function(input, output) {
  # create a reactive variable
  points <- reactive({subset(df, category == input$category)})
  
  # attach the map to output$map
  output$map <- 
    # build the output with the renderLeaflet() function
    renderLeaflet({
      # create a popup with some HTML
      popup <- paste0("<strong>Category: </strong>", points()$category,
                      "<br><strong>Location: </strong>", points()$location,
                      "<br><strong>Frequency: </strong>", points()$n)
      # create a colour palette
      factpal <- colorFactor("Paired", points()$category)
      # call a leaflet map
      leaflet() %>%
        # add a CartoDB raster layer
        addProviderTiles("CartoDB.Positron") %>% 
        # add the district boundary vector layer
        addPolygons(data = boundary, 
                    fillColor = "white", fillOpacity = 0.7, 
                    color = "grey", weight = 2) %>%
        # add markers graduated by the frequency of crimes
        addCircleMarkers(data = points(), ~long, ~lat, 
                         stroke = TRUE, color = "black", weight = 1, 
                         fillColor = ~factpal(category), fillOpacity = 0.8, 
                         radius = ~n*1.2, popup = popup)})
}

shinyApp(ui, server)