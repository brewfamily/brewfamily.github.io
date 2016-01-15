library(knitr)
dates <- seq(as.Date('2016-07-23'), as.Date('2016-07-31'), by = 1)
dows <- weekdays(dates, abbreviate = TRUE)
dates <- format(dates, format = '%b %d')
locations <- c('BCN / SCQ', #23
               'BCN / SCQ / Palamós', #24
               'Palamós', #25
               'Palamós', #26
               'Palamós / SCQ', #27
               'SCQ', #28
               'SCQ', #29
               'SCQ / Barcelona', #30
               'SCQ / Barcelona') #31
details <- c('Arrival weekend.  Chill in BCN/SCQ or make the journey to Palamós.', #23
             'Arrival weekend.  Chill in BCN/SCQ or make the journey to Palamós.', #24
             'First official family meet-up.  Lunch at 12:00 noon at (restaurant name).', #25
             'Beach, hang out, etc. in Palamós.', #26
             'Morning beach.  Afternoon journey to SCQ. Dinner in SCQ', #27
             'Village, wine tour, mountain bike, hike.',
             'Village, hang out, hike.', #29
             'Departure weekend. Chill in village or go to BCN.', #30
             'Departure weekend. Chill in village or go to BCN.') #31
lodging <- c('BCN / SCQ', #23
             'BCN / Palamós / SCQ', #24
             'Palamós', #25
             'Palamós', #26
             'SCQ', #27
             'SCQ', #28
             'SCQ', #29
             'SCQ / BCN', #30
             'SCQ / BCN') #31
temp <- data.frame(Weekday = dows, 
                   Date = dates,
                   Daytime = locations,
                   Sleep = lodging,
                   Details = details)



kable(temp, format = 'html')


# ####################
# 
# #### MAP OF JUST MAPUTO
# library(leaflet)
# library(ggmap)
# library(dplyr)
# library(raster)
# library(RColorBrewer)
# moz <- getData('GADM', country = 'MOZ', level = 3)
# 
# 
# # Bring in manually data about prevalence
# # http://www.pmi.gov/docs/default-source/default-document-library/malaria-operational-plans/fy14/mozambique_mop_fy14.pdf?sfvrsn=12
# df <- data.frame(NAME_1 = unique(sort(moz$NAME_1)))
# df$val <- c(47.2, 21.8, 36.8, 28.2, 3.2, 43.3,
#             52.1, 30.7, 36.9, 54.8)
# 
# # Bring data into moz
# moz@data <- left_join(x = moz@data, y = df)
# 
# # Define color
# pal <- brewer.pal(n = 9, 'Reds')
# cols_range <- colorRampPalette(pal)(ceiling(max(moz$val)))
# cols <- cols_range[ceiling(moz$val)]
# 
# # # Create a dataframe of locations, descriptions and names
# # places <- data.frame(place = 'Manhica, Maputo, Mozambique',
# #                      description = 'Centro de Investigação em Saúde de Manhiça (research site)',
# #                      stringsAsFactors = FALSE)
# # # Run the following if you want to geocode the locations:
# # temp <- geocode(places$place)
# # places <- cbind(places, temp)
# 
# # Define some stuff for the legened
# legend_seq <- seq(5, 45, 10)
# legend_cols <- cols_range[legend_seq]
# 
# # Create a leaflet widget
# m <- leaflet(moz) %>%
#   # addProviderTiles("Stamen.Toner") %>%
#   # addProviderTiles("Stamen.Toner") %>% 
#   # addProviderTiles("OpenWeatherMap.Temperature") %>%
#   # addProviderTiles("MapQuestOpen.Aerial") %>%
#   addProviderTiles("Stamen.TonerBackground") %>% 
#   setView(lng=32.7971649, lat=-25.3923014 +7, zoom=5) %>%
#   #   addMarkers(lng=places$lon, 
#   #              lat= places$lat, 
#   #              popup=paste0(places$place, ' : ',
#   #                           places$description)) %>%
#   addPolygons(
#     stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.5,
#     # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
#     color = cols) %>%
#   addLegend(position = 'bottomright',
#             colors = legend_cols,
#             labels = legend_seq,
#             opacity = 0.9,
#             title = 'Prevalence'
#   )
# m
# 
# # Save the widget to an html file
# library(htmlwidgets)
# saveWidget(m, file="/home/joebrew/Documents/joebrew.github.io/maps/malaria_prevalence.html")
# 




#### MAP OF PALAMOS
library(leaflet)
library(ggmap)
library(raster)
library(RColorBrewer)
esp4 <- getData('GADM', country = 'ESP', level = 4)
# esp3 <- getData('GADM', country = 'ESP', level = 3)

# Subset to just include area of palamos
palamos <- esp4[esp4@data$NAME_4 == 'Palamós',]

# cols
cols0 <- 'darkred'

# Geocode el galeo restaurant
meetup <- geocode(location = 'El Galeo, Palamos, Spain',
                  source = 'google')

m <- leaflet(palamos) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
    color = cols0) %>%
  addMarkers(lng = meetup$lon,
             lat = meetup$lat,
             popup = 'El Galeo restaurant. Noon, July 25.')
m

# Save the widget to an html file
library(htmlwidgets)
saveWidget(m, file="/home/joebrew/Documents/brewfamily.github.io/maps/palamos.html")


# Subset to just include area of SCQ
scq <- esp4[esp4@data$NAME_4 == 'Santa Coloma de Queralt',]

# Geocode el galeo restaurant
meetup <- geocode(location = '61, Carretera Igualada, Santa Coloma de Queralt, Spain',
                  source = 'google')

# cols
cols0 <- 'darkblue'

m <- leaflet(scq) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
    color = cols0) %>%
  addMarkers(lng = meetup$lon,
             lat = meetup$lat,
             popup = 'Coloma\'s family\'s house')
m

# Save the widget to an html file
saveWidget(m, file="/home/joebrew/Documents/brewfamily.github.io/maps/scq.html")

########################3
# Subset to just include area of BCN
bcn <- esp4[esp4@data$NAME_4 == 'Barcelona',]

meetup <- geocode(location = 'Carrer cienfuegos, 10, Barcelona, Spain',
                  source = 'google')

# cols
cols0 <- 'darkgreen'

m <- leaflet(bcn) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
    color = cols0) %>%
  addMarkers(lng = meetup$lon,
             lat = meetup$lat,
             popup = 'Joe and Coloma\'s apartment')
m

# Save the widget to an html file
saveWidget(m, file="/home/joebrew/Documents/brewfamily.github.io/maps/bcn.html")

