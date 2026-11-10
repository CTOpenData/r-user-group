library(sf)
library(dplyr)
library(leaflet)


#Connecticut municipal boundaries from Geodata Portal
CTtown_OPM <- st_read(
  "https://services3.arcgis.com/3FL1kr7L4LvwA2Kb/arcgis/rest/services/CT_Municipalities_(with_FIPS)/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
) %>%
  st_transform(4326) %>%
  st_make_valid() #make valid is sometimes needed because boundaries may have overlaps (topology issues)



# Create label points for town names
town_labels <- CTtown_OPM %>%
  st_point_on_surface()



# Underground Storage Tank data
ust_url <- "https://data.ct.gov/api/views/utni-rddb/rows.csv?accessType=DOWNLOAD"

USTs <- read.csv(ust_url)

#Point data from Open Data Portal (must have lat and lon)
USTs_sf <- USTs %>%
  filter(
    !is.na(UST.Site.Longitude),#need to remove records that have no lat/lon
    !is.na(UST.Site.Latitude)
  ) %>%
  st_as_sf(
    coords = c("UST.Site.Longitude", "UST.Site.Latitude"),
    crs = 4326,
    remove = FALSE) 
  



# Interactive map
leaflet() %>%
  #addTiles() %>%
  
  addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>% 
  
  addPolygons(
    data = CTtown_OPM,
    weight = 1,
    fill = FALSE,
    color = "blue",
    group = "CT Towns"
  ) %>%
  
  
  
  addLabelOnlyMarkers(
    data = town_labels,
    label = ~Municipality,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "color" = "white",
        "font-size" = "10px",
        "font-weight" = "bold"
      )
    )
  ) %>%
  
  
  addCircleMarkers(
    data = USTs_sf,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.8,
    color = "orange",
    popup = ~paste0(
      "<b>", Site.Name, "</b>",
      "<br>",
      Site.Address
    ),
    group = "UST Sites"
  )
