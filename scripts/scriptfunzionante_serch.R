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
                       paste0("Cognome e nome:"),
                       paste0("<b>", soldiers$surname_name, "</b>", "<br>"),
                       paste0("Processato/a il ", "<b>", soldiers$judgement_date_human, "</b>"),
                       paste0("Per i seguenti reati: ", "<b>", soldiers$crime_list_ordered, "</b>"),
                       paste0("Verdetto: ", "<b>", soldiers$verdict, "</b>"),
                       paste0("È stato condannato a: ", "<b>", soldiers$prison_years, "</b>", " anni ", "<b>", soldiers$prison_months, "</b>", " mesi ","<b>", soldiers$prison_days, "</b>", " giorni di prigione"),
                       paste0("Età al giorno del processo: ", "<b>", soldiers$age, "</b>"),
                       paste0("Nato a ", "<b>", soldiers$birth_place, "</b>", " in provincia di ", "<b>", soldiers$birth_city, " giorni di prigione", "</b>"),
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

## Read the geojson datasets for the search tool
## 1. Militaries
militaries <- readr::read_file('data/geojson/geojson_searchbox/militaries.geojson')
militaries_icon <- makeAwesomeIcon(icon='user', 
                                library='ion',
                                markerColor = 'red',
                                iconColor = 'black')

icon_militaries <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/user.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

## 2. Arrests
men <- readr::read_file('data/geojson/geojson_searchbox/men.geojson')
men_icon <- makeAwesomeIcon(icon = "man", library = "glyphicon",
                             markerColor = "white", iconColor = "black", spin = FALSE,
                             extraClasses = NULL, squareMarker = FALSE, iconRotate = 0,
                             fontFamily = "monospace", text = NULL)

icon_men <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/male.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

## Last Letters
women <- readr::read_file('data/geojson/geojson_searchbox/women.geojson')
women_icon <- makeAwesomeIcon(icon='woman', 
                                    library='ion',
                                    markerColor = 'white', 
                                    iconColor = 'black')

icon_women <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/female.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

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
              fillOpacity = 0.1,
              weight = 2, 
              smoothFactor = 0.5,
              group = "Italian borders 1914") %>%
  
  ## Add the geojson for each datasets for the search box
  ## 1. Massacres
  addGeoJSONv2(
    militaries,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = militaries_icon,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "Militaries") %>%
  
  ## 2. Arrests
  addGeoJSONv2(
    men,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = men_icon,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "Men") %>%
  
  ## 3. Last Letters
  addGeoJSONv2(
    women,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = women_icon,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "Women") %>%
  
  ## Add the Search Tool
  addSearchFeatures(
    
    targetGroups =  c('Militaries',
                      'Men', 
                      'Women'),
    
    options = searchFeaturesOptions(
      propertyName='NAME', zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE, position = "topleft" )) %>%
  
  addControl("<P><B>Search</B> for a person's name</P>",
             position='topleft') %>%
  
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
                                     "Città"),
                   
                   options = layersControlOptions(collapsed = TRUE)) %>%
  
  # Hide empty groups
  hideGroup(c("Empty Map",
              "Regioni",
              "Città",
              "Soldati",
              "Militaries",
              "Men",
              "Women"))

# Print the leaflet map
m


