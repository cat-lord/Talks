## Using R as a GIS ##

# set the working directory
setwd("")

# download Snow's cholera data collated and digitised by Roger Bivand
download.file("http://geostat-course.org/system/files/data_0.zip", destfile="data_0.zip")
unzip("data_0.zip")

# load the vector layers (polygons, points, lines)
library(rgdal)
boundary <- readOGR(dsn = "data", layer = "bbo", verbose = FALSE) # bounding box
buildings <- readOGR(dsn = "data", layer = "buildings", verbose = FALSE) # building contours on Snow's map
deaths <- readOGR(dsn = "data", layer = "deaths", verbose = FALSE) # cholera deaths at each location
b_pump <- readOGR(dsn = "data", layer = "b_pump", verbose = FALSE) # Broad Street pump
nb_pump <- readOGR(dsn = "data", layer = "nb_pump", verbose = FALSE) # other pump locations in Soho

# plot the vector layers
plot(buildings,  border = "grey")
plot(deaths, col = "red", add = T) 
points(nb_pump, col = "blue") 
points(b_pump, col = "blue") 
title("John Snow's Cholera Map of London \n(1854)")

# inspect the geometry data
summary(deaths)
class(deaths)
proj4string(deaths)
str(boundary@polygons)

# inspect the attribute data
names(deaths)
head(deaths@data, 4)
sum(deaths$Num_Css)

## explore the data
quantile(deaths$Num_Css, .95) 
deaths@data[deaths$Num_Css > 4, ] 
selection <- deaths$Num_Css > 4
plot(buildings, border = "grey")
plot(deaths[selection, ], pch = 21, bg = "red", col = "red", cex = 0.8, add = T)
plot(nb_pump, pch = 21, bg = "blue", col = "blue", cex = 1, add = T)
plot(b_pump, pch = 21, bg = "blue", col = "blue", cex = 1, add = T)
title(main = "Locations with cholera deaths\nin 95th percentile")

## data manipulation
head(b_pump@data)
b_pump@data[1,] <- "Broad Street" # change value in b_pump
pumps <- rbind(nb_pump, b_pump) # merge pump data
View(pumps@data)
plot(buildings, border = "grey")
plot(nb_pump, pch = 21, bg = "blue", col = "blue", cex = 1, add = T)
plot(pumps, pch = 21, bg = "blue", col = "blue", cex = 1, add = T)
pumps$url <- c(rep("https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/480px-No_image_available.svg.png", 11), "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/John_Snow_memorial_and_pub.jpg/900px-John_Snow_memorial_and_pub.jpg")
View(pumps@data)

### create buffers
library(rgeos)
buffer <- gBuffer(pumps, width = 50, byid=TRUE)
plot(buildings, border = "grey")
plot(pumps, pch = 21, bg = "blue", col = "black", cex = 1, add = T)
plot(buffer, add = T)

### point in polygon
plot(deaths, pch = 21, bg = "red", col = "red", cex = 0.8, add = T)
proj4string(deaths) <- proj4string(buffer)
pointsinpolygon <- over(SpatialPolygons(buffer@polygons), SpatialPoints(deaths), returnList = TRUE)
buffer$deaths <- unlist(lapply(pointsinpolygon, length))
View(buffer@data)
plot(buildings, border = "grey")
plot(pumps, pch = 21, bg = "blue", col = "black", cex = 1, add = T)
plot(deaths, pch = 21, bg = "red", col = "red", cex = 0.8, add = T)
plot(buffer[buffer$deaths %in% c(22, 7, 6), ], add = T)

## reproject with new CRS
deaths@proj4string
deaths_WGS84 <- spTransform(deaths, CRS("+proj=longlat +datum=WGS84"))
deaths_WGS84@proj4string 
pumps_WGS84 <- spTransform(pumps, CRS("+proj=longlat +datum=WGS84"))
buildings_WGS84 <- spTransform(buildings, CRS("+proj=longlat +datum=WGS84"))

## Interactive maps with leaflet
library(leaflet)

## a simple map
leaflet() %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles() %>% 
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue") %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red")

## with popups
popup <- paste0("<img style='width: 150px;' src = ", pumps_WGS84$url, " >")
leaflet(data = pumps_WGS84) %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue", radius = 7, popup = popup) %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red", radius = 5)

## with a polygon layer
leaflet() %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = buildings_WGS84, color = "orange", weight = 2) %>% 
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue", radius = 7, popup = popup) %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red", radius = 5)

## with John Snow's map
## https://rpubs.com/walkerke/custom_tiles
leaflet() %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles(urlTemplate = "http://walkerke.github.io/tiles/snow/{z}/{x}/{y}.png",
           attribution = 'Data source: <a href="http://guides.library.yale.edu/gisworkshoparchive">Yale University Library</a>', 
           options = tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE)) %>%
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue", radius = 7, popup = popup) %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red", radius = 5)

## graduated points
leaflet() %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles(urlTemplate = "http://walkerke.github.io/tiles/snow/{z}/{x}/{y}.png",
           attribution = 'Data source: <a href="http://guides.library.yale.edu/gisworkshoparchive">Yale University Library</a>', 
           options = tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE)) %>%
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue", radius = 7, popup = popup) %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red", radius = ~Num_Css, label = ~as.factor(Num_Css))

# kernel density estimation
library(KernSmooth)
coords_df = data.frame(deaths_WGS84@coords)
coords <- cbind(coords_df$coords.x1, coords_df$coords.x2)
kde <- bkde2D(coords, bandwidth = c(bw.nrd(coords[,1]), bw.nrd(coords[,2])))
contour <- contourLines(x = kde$x1, y = kde$x2, z= kde$fhat)

leaflet() %>% 
  setView(-0.1354223, 51.5135085, zoom = 17) %>%
  addTiles(urlTemplate = "http://walkerke.github.io/tiles/snow/{z}/{x}/{y}.png",
           attribution = 'Data source: <a href="http://guides.library.yale.edu/gisworkshoparchive">Yale University Library</a>', 
           options = tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE)) %>%
  addPolygons(contour[[1]]$x, contour[[1]]$y, fillColor = "#f7f7f7", stroke = F, fillOpacity = .2) %>% 
  addPolygons(contour[[3]]$x, contour[[3]]$y, fillColor = "#cccccc", stroke = F, fillOpacity = .2) %>%
  addPolygons(contour[[5]]$x, contour[[5]]$y, fillColor = "#969696", stroke = F, fillOpacity = .2) %>% 
  addPolygons(contour[[7]]$x, contour[[7]]$y, fillColor = "#636363", stroke = F, fillOpacity = .2) %>% 
  addPolygons(contour[[9]]$x, contour[[9]]$y, fillColor = "#252525", stroke = F, fillOpacity = .2) %>% 
  addPolylines(contour[[1]]$x,contour[[1]]$y, color = "#f7f7f7") %>%
  addPolylines(contour[[3]]$x,contour[[3]]$y, color = "#cccccc") %>% 
  addPolylines(contour[[5]]$x,contour[[5]]$y, color = "#969696") %>% 
  addPolylines(contour[[7]]$x,contour[[7]]$y, color = "#636363") %>% 
  addPolylines(contour[[9]]$x,contour[[9]]$y, color = "#252525") %>% 
  addCircles(data = pumps_WGS84, color = "blue", fillColor = "blue", radius = 7, popup = popup) %>% 
  addCircles(data = deaths_WGS84, color = "red", fillColor = "red", radius = 1)

## Static  maps using tmap
library(tmap)

# a simple map
qtm(buildings, fill = NULL) + 
  qtm(pumps, symbols.col = "blue") +
  qtm(deaths, symbols.col = "red",
      symbols.size = "Num_Css", 
      symbols.title.size= "Cholera deaths")

# with tweaks
map <- tm_shape(buildings) + 
  tm_borders(lwd = 1, col = "grey", alpha = 0.5) +
  tm_shape(pumps) + 
  tm_symbols(col = "blue", border.col = "blue") +
  tm_shape(deaths) + 
  tm_bubbles(size = "Num_Css", 
             col = "red",
             alpha = 0.5,
             title.size = "Cholera deaths") +
  tm_layout(title = "Snow's Cholera Map of London (1854)",
            title.size = 0.9,
            title.position = c("center", "top"), 
            legend.position = c("right", "bottom"),
            frame = FALSE, 
            inner.margins=c(0.13, 0.01, 0.05, 0.01), 
            asp = 1) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) 

# export to png image
save_tmap(map, "cholera_map.png", width=2000, height=2000)
