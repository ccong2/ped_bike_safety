
library(sp)
library(maptools)
library(dplyr)
library(leaflet)



knitr::opts_knit$set(echo = TRUE,
                     root.dir = rprojroot::find_rstudio_root_file())

#-----------------Read Incidents Data--------------------
# Pedestrian crashes
data <- read.csv("./data/Pedestrian_Crashes_from_2005_to_2014/Pedestrian_Crashes_from_2005_to_2014.csv", header=T)
data <- subset(data, is.na(data$Y) == FALSE)
xy <- data[,c(1,2)]

# Projects these data for display on the web (EPSG: 4326),
pedcrash.84 <- SpatialPointsDataFrame(coords = xy, data = data,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Bicycle crashes
data <- read.csv("./data/Bike_Crashes_from_2005_to_2014/Bike_Crashes_from_2005_to_2014.csv", header=T)
data <- subset(data, is.na(data$Y) == FALSE)
xy <- data[,c(1,2)]

# Projects these data for display on the web (EPSG: 4326),
bikecrash.84 <- SpatialPointsDataFrame(coords = xy, data = data,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rm(data, xy)

#-----------------Read Streets Shapefile---------------------

streets.sp <- readShapeLines("./data/Street_Centerlines/Street_Centerlines.shp", proj4string=CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333
                                                                                            +k=0.999975 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs+ellps=GRS80 +tosp=0,0,0 ")) 
# Project the street shapefile to WGS84
streets.84<-spTransform(streets.sp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rm(streets.sp)

#-----------------Create Leaflet Map-----------------

# Set color palette to reflect the speed limits on the road
pal <- colorNumeric(palette = "RdBu",   domain = range(streets.84@data$SPEED))


# Add pop-ups to make the layers clickable 
pedestrian.popup <- paste0("<b>Injury: </b>", pedcrash.84@data$Injury, "<br>",
                         "<b>Fatality: </b>", pedcrash.84@data$Fatality, "<br>",
                         "<b>Year:</b>", pedcrash.84@data$Year,"<br>",
                         "<b>Weather:</b>",pedcrash.84@data$Weather,"<br>", 
                         "<b>RoadSurface: </b>", pedcrash.84@data$RoadSurface, "<br>",
                         "<b>TrafficControl: </b>", pedcrash.84@data$TrafficControl, "<br>",
                         "<b>CrashSeverity: </b>", pedcrash.84@data$CrashSeverity)

bicycle.popup <- paste0("<b>Injury: </b>", bikecrash.84@data$Injury, "<br>",
                          "<b>Fatality: </b>", bikecrash.84@data$Fatality, "<br>",
                          "<b>Year:</b>", bikecrash.84@data$Year, "<br>",
                          "<b>Weather:</b>", bikecrash.84@data$Weather, "<br>",
                          "<b>RoadSurface:</b>", bikecrash.84@data$RoadSurface, "<br>",
                          "<b>TrafficControl:</b>", bikecrash.84@data$TrafficControl, "<br>",
                          "<b>CrashSeverity:</b>", bikecrash.84@data$CrashSeverity)

streets.popup <- paste0("<b>StreetName:</b>", streets.84@data$STREETNAME, "<br>",
                        "<b>StreetType:</b>", streets.84@data$STREETPRET, "<br>",
                        "<b>CFCC:</b>",streets.84@data$CFCC,"<br>",
                        "<b>Speed:</b>", streets.84@data$SPEED)

# Plot the map
incidents <-leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-88.228846,40.112858, zoom = 15) %>% 
  addPolylines(data = streets.84,
               stroke=TRUE,
               color =~pal(SPEED),
               weight =2,
               smoothFactor = 0.2, 
               fillOpacity = 0.2, 
               popup=streets.popup, 
               group="Streets"  )  %>%
  addCircles(data = pedcrash.84, fillOpacity = 0.5, 
                   stroke = TRUE, weight = 5, radius = 75,
                   color = "purple",
                   popup= pedestrian.popup,
                   group="Pedestrian Incidents") %>%
  addCircles(data = bikecrash.84, fillOpacity = 0.5, 
                   stroke = TRUE, weight = 5, radius = 75,
                   color = "salmon",
                   popup= bicycle.popup,
                   group="Bicycle Incidents") %>%
  addLegend(position = 'topright',
            pal = pal,
            values = streets.84@data$SPEED,
            opacity = 0.2,
            title = "Speed"  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
    overlayGroups = c("Pedestrian Crashes", "Bicycle Crashes","Streets"),
    options = layersControlOptions(collapsed = TRUE) 
    
  )

# View the map
incidents

# Publish the map to a html file
Sys.setenv(Path='C:/Program Files (x86)/Pandoc') 
htmlwidgets::saveWidget(widget=incidents, file="ped_bike_incidents.html", selfcontained=TRUE, libdir = "js")

#===============END OF SCRIPT=====================