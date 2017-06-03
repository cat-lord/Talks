## Spatial data in R: an introduction to the sf package ##
## R at University of Manchester (R.U.M.) ##
## 05 June 2017 ##

## Installation and loading
# to install from CRAN:
install.packages("sf")  

# or the development version from GitHub:
devtools::install_github("edzer/sfr")

# then load:
library(sf)

# Note that MacOSX and LINUX users need to install a number of geospatial libraries (GEOS, GDAL, and proj.4).

# the tidyverse package also needs to be loaded
library(tidyverse)

## Example spatial data
# A GeoJSON of Greater Manchester’s wards was created from a vector boundary file available 
# from ONS’s Open Geography Portal. The GeoJSON is projected in British National Grid (EPSG:27700)
# and originally derives from the Ordnance Survey.

# Point data are supplied by data.police.uk and represent incidents of anti-social behaviour and crime 
# recorded by the Greater Manchester Police during March 2017.

## Reading spatial data 
# polygons
bdy <- st_read("data/wards.geojson")
bdy

# points
pts <- read_csv("data/2017-03-greater-manchester-street.csv")
pts <- st_as_sf(pts, coords = c("Longitude", "Latitude"))
pts

## Writing spatial data
st_write(bdy, "boundaries.shp")

## sf structure
# spatial metadata (geometry type, dimension, bbox, CRS) and 'geometry' is in a list-column
class(bdy)
bdy[1:2, 1:3]

## Attribute table
head(bdy)
as.tibble(bdy)
bdy_df <- st_set_geometry(bdy, NULL) # remove geometry
head(bdy_df)

## Using dplyr on sf object
bdy <- bdy %>% 
  select(ward = wd16nm, 
         census_code = wd16cd,
         borough = lad16nm) 
glimpse(bdy)

# Count frequency of wards by borough
bdy %>% 
  group_by(borough) %>% 
  count() %>% 
  arrange(desc(n))

## Using sf functions to add geometry column to dplyr chain
bdy <- bdy %>% 
  mutate(area_m2 = st_area(.)) # returns the area of a geometry

bdy %>% 
  select(ward, area_m2) %>% 
  st_set_geometry(., NULL) %>% 
  arrange(desc(area_m2)) %>% 
  slice(1:10)

## Check and assign CRS
st_crs(pts)
pts <- st_set_crs(pts, 4326)
st_crs(pts)

## Reproject CRS
bdy_WGS84 <- st_transform(bdy, 4326)
st_crs(bdy_WGS84)

## Convert to and from sp objects
bdy_sp <- as(bdy, 'Spatial')
class(bdy_sp)

bdy_sf <- st_as_sf(bdy_sp)
class(bdy_sf)

## Points in polygon
pts %>% 
  filter(`Crime type` == "Vehicle crime") %>%  
  st_join(bdy_WGS84, ., left = FALSE) %>% 
  count(ward) %>% 
  arrange(desc(n)) 

crimes <- pts %>% 
  filter(`Crime type` == "Vehicle crime") %>%  
  st_join(bdy_WGS84, ., left = FALSE) %>% 
  count(ward)

## Plotting
# Base plots
plot(crimes)
plot(crimes["n"])

library(RColorBrewer) ; library(classInt)
pal <- brewer.pal(5, "RdPu")
classes <- classIntervals(crimes$n, n=5, style="pretty")$brks
plot(crimes["n"], 
     col = pal[findInterval(crimes$n, classes, all.inside=TRUE)], 
     main = "Vehicle crime in Greater Manchester\nMarch 2017", axes = F)
legend("bottomright", legend = paste("<", round(classes[-1])), fill = pal, cex = 0.7) 

# ggplot2 
devtools::install_github("tidyverse/ggplot2") # NB need development version for geom_sf()
ggplot(crimes) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn('Frequency', colours=RColorBrewer::brewer.pal(5,"RdPu"), 
                       breaks = scales::pretty_breaks(n = 5)) +
  labs(fill = "Frequency",
       title = "Vehicle crime",
       subtitle = "March 2017",
       caption = "Contains OS data © Crown copyright and database right (2017)") +
  theme_void()

# plotly
library(plotly)
p <- ggplot(crimes) +
  geom_sf(aes(fill = n, text = paste0(ward, "\n", "Crimes: ", n))) +
  scale_fill_gradientn('Frequency', colours=RColorBrewer::brewer.pal(5,"RdPu"), 
                       breaks = scales::pretty_breaks(n = 5)) +
  labs(fill = "Frequency",
       title = "Vehicle crime",
       subtitle = "March 2017",
       caption = "Contains OS data © Crown copyright and database right (2017)") +
  theme_void() + 
  coord_fixed(1.3)
ggplotly(p, tooltip = "text")

# leaflet
library(leaflet)
pal <- colorBin("RdPu", domain = crimes$n, bins = 5, pretty = TRUE)
leaflet(crimes) %>% 
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
    attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2017)</a>') %>% 
  addPolygons(fillColor = ~pal(n), fillOpacity = 0.8,
              weight = 2, opacity = 1, color = "grey",
              label = ~as.character(ward)) %>% 
  addLegend(pal = pal, values = ~n, opacity = 0.7, 
            title = 'Vehicle crime (March 2017)', position = "bottomleft") %>%
  addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE)
