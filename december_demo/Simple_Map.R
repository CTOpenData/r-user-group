# #Easy map 
library(leaflet)

# Create a simple map using the quakes dataset
leaflet(data = quakes) %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addCircleMarkers(
    radius = 1,
    color = "red",
    fillOpacity = 1,
    clusterOptions = markerClusterOptions() # Cluster for performance
  )


library(leaflet)
library(maps)
library(sf)

#US states dataset
#Convert to sf for polygons
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Sample points dataset
points_data <- data.frame(
  longitude = c(-72.673370, -73.759262, -75.165222),
  latitude = c(41.765804, 42.652580, 39.952583),
  label = c("Hartford", "Albany", "Philadelphia")
)

# Create a map with polygons and points
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = states, color = "blue", weight = 1, fillOpacity = 0.3) %>%
  addCircleMarkers(
    data = points_data,
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = "orange",
    fillOpacity = 1,
    label = ~label
  )

