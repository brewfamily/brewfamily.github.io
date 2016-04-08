#### MAP OF TORONTO

# Libraries
library(leaflet)
library(ggmap)
library(raster)
library(RColorBrewer)
library(htmlwidgets)

# Get canada shapefile
can3 <- getData('GADM', country = 'CAN', level = 3)

# Subset to just include area of toronto
toronto <- can3[can3@data$NAME_3 == 'Toronto',]

# cols
cols0 <- 'darkred'

# Geocode Ben's address
meetup <- geocode(location = '182 Baldwin Street Toronto ON, M5T 1L8',
                  source = 'google')

m <- leaflet(toronto) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
    color = cols0) %>%
  addMarkers(lng = meetup$lon,
             lat = meetup$lat,
             popup = 'Ben and Xing are here')
m

# Save the widget to an html file
saveWidget(m, file="~/Desktop/map_which_you_can_put_in_iframe_if_u_want.html")
