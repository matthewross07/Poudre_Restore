# R script to generate interactive Leaflet map for Poudre River Sensor Network
library(leaflet)
library(htmlwidgets)
library(sf)
library(dplyr)

# Sensor stations along Cache la Poudre River (Munroe Diversion -> South Platte Confluence)
stations <- data.frame(
  id = 1:10,
  name = c(
    "Munroe Gravity Canal Diversion",
    "Poudre Canyon Mouth (USGS 06752000)",
    "Bellvue / Watson Lake Reach",
    "Laporte Sentinel Site",
    "Fort Collins Downtown (Lincoln Ave - USGS 06752260)",
    "Environmental Learning Center (ELC)",
    "Timnath Reach",
    "Windsor Reach (USGS 06752280)",
    "Greeley O Street Bridge (USGS 06752400)",
    "South Platte Confluence (USGS 06752500)"
  ),
  type = c(
    "Settlement Reach Upper Boundary",
    "Continuous Sensor Station",
    "Continuous Sensor Station",
    "High-Frequency Sensor Station",
    "Continuous Sensor Station",
    "High-Frequency Sensor Station",
    "Continuous Sensor Station",
    "Continuous Sensor Station",
    "Continuous Sensor Station",
    "Settlement Reach Lower Boundary"
  ),
  lat = c(40.6970, 40.6912, 40.6300, 40.6270, 40.5885, 40.5530, 40.5280, 40.4780, 40.4420, 40.4180),
  lng = c(-105.2150, -105.1950, -105.1720, -105.1400, -105.0690, -105.0120, -104.9750, -104.9120, -104.6780, -104.6020),
  params = c(
    "Flow Intake, Diversion Bypass, Temp",
    "Flow (CFS), Temp, Turbidity, SpCond",
    "Temp, DO, Turbidity",
    "Temp, DO, NO3-, Turbidity",
    "Flow, Temp, DO, Turbidity, NO3-",
    "Temp, DO, SpCond, Turbidity",
    "Temp, DO, NO3-",
    "Flow, Temp, DO, Turbidity, NO3-",
    "Flow, Temp, DO, Turbidity, NO3-",
    "Basin Mass Balance Outlet: Flow, Temp, DO, NO3-"
  ),
  status = c(
    "Boundary", "Active Real-Time", "Active Real-Time", "Active Real-Time", 
    "Active Real-Time", "Active Real-Time", "Active Real-Time", "Active Real-Time", 
    "Active Real-Time", "Boundary / Outlet"
  ),
  stringsAsFactors = FALSE
)

# Colors for markers
getColor <- function(type) {
  sapply(type, function(t) {
    if (grepl("Boundary", t)) {
      "#f59e0b" # amber
    } else if (grepl("High-Frequency", t)) {
      "#10b981" # emerald
    } else {
      "#0ea5e9" # sky blue
    }
  })
}

# Create river trajectory polyline
river_coords <- matrix(c(
  -105.2150, 40.6970,
  -105.1950, 40.6912,
  -105.1720, 40.6300,
  -105.1400, 40.6270,
  -105.0690, 40.5885,
  -105.0120, 40.5530,
  -104.9750, 40.5280,
  -104.9120, 40.4780,
  -104.6780, 40.4420,
  -104.6020, 40.4180
), ncol = 2, byrow = TRUE)

m <- leaflet(stations, options = leafletOptions(minZoom = 9, maxZoom = 16)) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://carto.com/">CARTO</a>', 
           group = "Dark Map") %>%
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", 
           attribution = '&copy; Esri', 
           group = "Satellite") %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://carto.com/">CARTO</a>', 
           group = "Light Map") %>%
  addPolylines(
    lng = river_coords[,1], 
    lat = river_coords[,2], 
    color = "#38bdf8", 
    weight = 5, 
    opacity = 0.85,
    dashArray = "1, 0",
    group = "Poudre River Corridor"
  ) %>%
  addCircleMarkers(
    lng = ~lng, 
    lat = ~lat,
    radius = ifelse(grepl("Boundary", stations$type), 10, 8),
    color = getColor(stations$type),
    fillColor = getColor(stations$type),
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 2,
    popup = paste0(
      "<div style='font-family: system-ui, -apple-system, sans-serif; padding: 4px; color: #0f172a;'>",
      "<h4 style='margin:0 0 6px 0; color:#0284c7; font-size: 14px;'>", stations$name, "</h4>",
      "<b>Type:</b> ", stations$type, "<br/>",
      "<b>Parameters:</b> ", stations$params, "<br/>",
      "<b>Status:</b> <span style='background:#e0f2fe; color:#0369a1; padding:2px 6px; border-radius:4px; font-weight:600; font-size:11px;'>", stations$status, "</span>",
      "</div>"
    ),
    label = stations$name
  ) %>%
  addLayersControl(
    baseGroups = c("Dark Map", "Satellite", "Light Map"),
    overlayGroups = c("Poudre River Corridor"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  fitBounds(-105.24, 40.40, -104.58, 40.72)

# Save HTML widget
saveWidget(m, file = "map.html", selfcontained = TRUE)
cat("Map successfully created: map.html\n")
