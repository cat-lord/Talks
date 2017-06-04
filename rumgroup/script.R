## Spatial data in R: an introduction to the sf package ##
## R at University of Manchester (R.U.M.) ##
## 05 June 2017 ##


## ------------------------------------------------------------------------

## INSTALLATION AND LOADING ##

# to install from CRAN:
install.packages("sf")  

# or the development version from GitHub:
devtools::install_github("edzer/sfr")

# then load:
library(sf)

# Note that MacOSX and LINUX users need to install a number of geospatial libraries (GEOS, GDAL, and proj.4).

# the tidyverse package also needs to be loaded
library(tidyverse)

<<<<<<< HEAD
## ------------------------------------------------------------------------

### EXAMPLE DATA

## Example spatial data (vector and point)
=======
## Example spatial data
>>>>>>> origin/master
# A GeoJSON of Greater Manchester’s wards was created from a vector boundary file available 
# from ONS’s Open Geography Portal. The GeoJSON is projected in British National Grid (EPSG:27700)
# and originally derives from the Ordnance Survey.

# Point data are supplied by data.police.uk and represent incidents of anti-social behaviour and crime 
# recorded by the Greater Manchester Police during March 2017.

## ------------------------------------------------------------------------

## READING AND WRITING SPATIAL DATA ##

## Reading spatial data (polygons)
bdy <- st_read("data/wards.geojson")
bdy

## Reading data with coordinates (points)
pts <- read_csv("data/2017-03-greater-manchester-street.csv")
pts <- st_as_sf(pts, coords = c("Longitude", "Latitude"))
pts

## Writing spatial data
# st_write(bdy, "boundaries.shp")

## ------------------------------------------------------------------------

## SF OBJECTS ##

# attribute table (dataframe) AND geometry (list-column) with coordinates, CRS, bbox
class(bdy)
head(bdy)
as.tibble(bdy)
bdy_df <- st_set_geometry(bdy, NULL) # remove geometry
head(bdy_df)
st_geometry(bdy) # print geometry

## ------------------------------------------------------------------------

## USING DPLYR ##

## Manipulate sf objects with dplyr
# Rename variables
bdy <- bdy %>% 
  select(ward = wd16nm, census_code = wd16cd, borough = lad16nm) # rename variables
glimpse(bdy)

# Count frequency of wards by borough
bdy %>% 
  group_by(borough) %>% 
  count() %>% 
  arrange(desc(n)) %>% # sort in descending order
  st_set_geometry(., NULL) # hide geometry

# Select features
chorlton <- bdy %>% 
  filter(ward == "Chorlton")
plot(st_geometry(bdy))
plot(st_geometry(chorlton), col = "red", add = TRUE)

## Using sf functions to add geometry column to dplyr chain
bdy <- bdy %>% 
  mutate(area = st_area(.)) # returns the area of a feature

bdy %>% 
  select(ward, area) %>% 
  arrange(desc(area)) %>% 
  slice(1:10) %>% 
  st_set_geometry(., NULL)

## ------------------------------------------------------------------------

## PROJECTION ##

## Check and assign CRS
st_crs(pts) 
pts <- st_set_crs(pts, 4326) # assign Lat/Long (epsg:4326)
st_crs(pts)

## Reproject CRS
bdy_WGS84 <- st_transform(bdy, 4326)
st_crs(bdy_WGS84)

## ------------------------------------------------------------------------

## CONVERT TO AND FROM SP OBJECTS

bdy_sp <- as(bdy, 'Spatial')
class(bdy_sp)

bdy_sf <- st_as_sf(bdy_sp)
class(bdy_sf)

## ------------------------------------------------------------------------

## SPATIAL OPERATIONS ##

## Buffer features
buffer <- chorlton %>% 
  st_buffer(dist = 1000)
plot(st_geometry(buffer))
plot(st_geometry(chorlton), col = "red", add = TRUE)

## Buffer and intersect
pts_sub <- bdy %>%
  filter(ward == "Chorlton") %>%
  st_buffer(dist = 1000) %>%
  st_intersection(st_transform(pts, 27700)) # reproject pts to BNG (epsg:27700)

plot(st_geometry(buffer))
plot(st_geometry(chorlton), col = "red", add = TRUE)
plot(st_geometry(pts_sub), col = "black", add = TRUE)

pts_sub %>% 
  group_by(`Crime type`) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  st_set_geometry(., NULL)

## Points in polygon
pts %>% 
  filter(`Crime type` == "Vehicle crime") %>%  
  st_join(bdy_WGS84, ., left = FALSE) %>% 
  count(ward) %>% 
  arrange(desc(n)) %>% 
  st_set_geometry(., NULL)

bdy_pts <- pts %>% 
  filter(`Crime type` == "Vehicle crime") %>%  
  st_join(bdy_WGS84, ., left = FALSE) %>% 
  count(ward)

<<<<<<< HEAD
## ------------------------------------------------------------------------

## PLOTTING ##

## Base plots
plot(bdy_pts) # plots small multiples if dataframe has several attributes
plot(bdy_pts["n"]) # select the appropriate attribute to plot a single map

library(RColorBrewer) ; library(classInt)
pal <- brewer.pal(5, "RdPu")
classes <- classIntervals(bdy_pts$n, n=5, style="pretty")$brks
plot(bdy_pts["n"], 
     col = pal[findInterval(bdy_pts$n, classes, all.inside=TRUE)], 
     main = "Vehicle crime in Greater Manchester\nMarch 2017", axes = F)
legend("bottomright", legend = paste("<", round(classes[-1])), fill = pal, cex = 0.7) 

## ggplot2 
# devtools::install_github("tidyverse/ggplot2") # NB need development version for geom_sf()
ggplot(bdy_pts) +
=======
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
>>>>>>> origin/master
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn('Frequency', colours=RColorBrewer::brewer.pal(5,"RdPu"), 
                       breaks = scales::pretty_breaks(n = 5)) +
  labs(fill = "Frequency",
       title = "Vehicle crime",
       subtitle = "March 2017",
       caption = "Contains OS data © Crown copyright and database right (2017)") +
  theme_void()

## plotly
library(plotly)
p <- ggplot(bdy_pts) +
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

## leaflet
library(leaflet)
<<<<<<< HEAD
pal <- colorBin("RdPu", domain = bdy_pts$n, bins = 5, pretty = TRUE)
leaflet(bdy_pts) %>% 
=======
pal <- colorBin("RdPu", domain = crimes$n, bins = 5, pretty = TRUE)
leaflet(crimes) %>% 
>>>>>>> origin/master
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
    attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2017)</a>') %>% 
  addPolygons(fillColor = ~pal(n), fillOpacity = 0.8,
              weight = 1, opacity = 1, color = "black",
              label = ~as.character(ward)) %>% 
  addLegend(pal = pal, values = ~n, opacity = 0.7, 
            title = 'Vehicle crime (March 2017)', position = "bottomleft") %>%
  addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE)
