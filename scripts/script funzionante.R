# Load necessary libraries
library(leaflet)
library(sp)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)
library(htmlwidgets)
library(geojsonio)

# Read the CSV with the soldier's data
soldiers <- read.csv("data/csv/judgement_general_dataset.csv")

# Define content for popups in leaflet markers
content_popup <- paste(sep = "<br/>",
                       paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                       paste0("Event type:"),
                       paste0("<b>", soldiers$genre),
                       paste0("</div>"))

# Define icons for leaflet markers
icons <- makeAwesomeIcon(icon= ~icon, library='fa', markerColor = ~color_grade, iconColor = "black")

# HTML content for legend
html_legend <- "<i class='fa fa-user' aria-hidden='true'></i>Military<br/>
                <i class='fa fa-female' aria-hidden='true'></i>Female civilian<br/> 
                <i class='fa fa-male' aria-hidden='true'></i>Male civilian"

# Read shapefile for Italian borders
borders <- readOGR('data/shp/italy_1914/italy_1914.shp')

# Read GeoJSON file for Italian regions
regions <- geojsonio::geojson_read("data/geojson/italy_regions.geojson", what = "sp")

# Define color palette for regions based on the number_integer of people judged
pal <- colorBin("YlOrRd", domain = regions$number_integer)

cities <- geojsonio::geojson_read("data/geojson/italy_cities.geojson", what = "sp")

# Define color palette for regions based on the number_integer of people judged
pal_2 <- colorBin("YlOrRd", domain = regions$number)

OpenStreetMap_HOT <- "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png"
# Basemap URL
# toned_basemap <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
# basemap_osm <- "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
# basemap_stamen <- "http://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png"
# basemap_esri <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
# basemap_cartodb <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
# basemap_cartodb <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"



# Initialize leaflet map
m <- leaflet() %>% 
  # Add basemap
  addTiles(urlTemplate = OpenStreetMap_HOT) %>% 
  # Add mouse coordinates
  leafem::addMouseCoordinates() %>% 
  # Add zoom reset button
  addResetMapButton() %>%
  # Add minimap
  addMiniMap() %>%
  # Set map center and zoom level
  setView(lng = 10.883447, 
          lat = 45.938287, 
          zoom = 2 ) %>%
  
  
  # Add markers for soldiers
  addAwesomeMarkers(data = soldiers, 
                    lng = soldiers$birth_lon, 
                    lat = soldiers$birth_lat,
                    group = "Denunciati",
                    icon = icons,
                    popup = c(content_popup),
                    clusterOptions = markerClusterOptions(),
                    options = markerOptions(autoZIndex = TRUE)) %>%
  
  # Add legend for markers
  addControl(html = html_legend, position = "bottomleft") %>%
  # Add additional legend
  addLegend("bottomleft",
            colors = c("red", 
                       "black",
                       "orange",
                       "white"),
            
            labels=c("Officers", 
                     "None", 
                     "Simple soldiers", 
                     "Civilians"),
            
            title="Legend of colours and symbols",
            
            group = "Denunciati") %>%
  
  # Add Italian regions
  addPolygons(data = regions, 
              fillColor = ~pal(regions$number_integer),
              color = "black",
              opacity = 0.8,
              stroke = TRUE,
              fillOpacity = 0.2,
              weight = 0.6, 
              smoothFactor = 0.5,
              group = "Regioni",
              label = ~paste(NAME_1, ": ", number_integer, " people judged and born here", sep = ""),
              highlightOptions = highlightOptions(weight = 0.6,
                                                  color = "#666",
                                                  fillOpacity = 0.7,
                                                  bringToFront = TRUE)) %>%
  
  # Add legend for regions
  addLegend(position = "bottomleft",
            pal = pal,
            values = regions$number_integer,
            title = "Regioni",
            group = "Regioni",
            opacity = 0.7) %>%
  
  # Add Italian cities
  addPolygons(data = cities, 
              fillColor = ~pal(regions$number),
              color = "black",
              opacity = 0.8,
              stroke = TRUE,
              fillOpacity = 0.2,
              weight = 0.6, 
              smoothFactor = 0.5,
              group = "Città",
              label = ~paste(name, ": ", number, " people judged and born here", sep = ""),
              highlightOptions = highlightOptions(weight = 0.6,
                                                  color = "#666",
                                                  fillOpacity = 0.7,
                                                  bringToFront = TRUE)) %>%
  
  # Add legend for regions
  addLegend(position = "bottomleft",
            pal = pal_2,
            values = regions$number,
            title = "Città",
            group = "Città",
            opacity = 0.7) %>%
  
  # Add Italian borders
  addPolygons(data = borders, 
              color = "black",
              opacity = 0.5,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 0.6, 
              smoothFactor = 0.5,
              group = "Italian borders 1914") %>%
  
  # Add additional legend
  addLegend("topright", 
            colors = c("trasparent", "trasparent"),
            labels=c("Giovanni Pietro Vitali", "giovannipietrovitali@gmail.com"),
            title="Courts of war 13th army corps 1915-1918") %>%
  
   # Add layers control
  addLayersControl(baseGroups = c("Denunciati",
                                  "Empty Layer"),
                   
                   overlayGroups = c("Italian borders 1914",
                                     "Regioni",
                                     "Città",
                                     "Soldati"),
                   
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  # Hide empty groups
  hideGroup(c("Empty Map",
              "Regioni",
              "Città",
              "Soldati"))

# Print the leaflet map
m

