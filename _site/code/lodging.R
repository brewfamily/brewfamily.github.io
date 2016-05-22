# Libraries
library(gsheet)
library(dplyr)
library(xtable)
library(knitr)
library(htmlTable)

# Get link of master spreadsheet
url_nights <- 'https://docs.google.com/spreadsheets/d/1VIYQr6VZzhJrJM0zSuUMhI0sQ8bjPwDscA_F6sxP7cA/edit?usp=sharing'
# Link of the rooms spreadsheet
url_rooms <- 'https://docs.google.com/spreadsheets/d/1eSF79SEKd9i0N6P6Z1rAVs4XSF5eN-sSWmIzItWoDcs/edit?usp=sharing'
# Link of the houses spreadsheet
url_houses <- 'https://docs.google.com/spreadsheets/d/1lZmV4-IxrM1GEb7_qAoYZ58G02UybOz308Kd0gfTSnE/edit?usp=sharing'
# Link of the arrivals/departures spreadsheet
url_arrivals <- 'https://docs.google.com/spreadsheets/d/1v70i20A7-cL6gXjObqkIqEKvzmqoR0hH0BM7FvaaMTI/edit?usp=sharing'

# Read into memory
nights <- gsheet2tbl(url_nights)
rooms <- gsheet2tbl(url_rooms)
houses <- gsheet2tbl(url_houses)
arrivals <- gsheet2tbl(url_arrivals)

# Clean up a dates
nights$date <- as.Date(nights$date, '%m/%d/%Y')
rooms$date <- as.Date(rooms$date, '%m/%d/%Y')
arrivals$arrival_date <- as.Date(arrival_date, '%m/%d/%Y')
arrivals$departure_date <- as.Date(departure_date, '%m/%d/%Y')

# Get day of week
nights$day <- weekdays(nights$date)
rooms$day <- weekdays(rooms$date)

####################

# Get reserved house/rooms by date (just for guialmons)
occupations <-
  nights %>%
  filter(location == 'guialmons') %>%
  group_by(date, house, room) %>%
  tally 

# Join to available nights
availabilities <- 
  left_join(x = rooms,
            y = occupations,
            by = c('date', 'house', 'room')) %>%
  mutate(available = is.na(n)) %>%
  arrange(available, date)

#################################
# Get a list of people
temp <- nights %>%
  filter(!duplicated(name)) %>%
  mutate(id = 1:40) %>%
  dplyr::select(family, name)
# Clean out repeat family names
for (i in nrow(temp):2){
  while(temp$family[i-1] == temp$family[i]){
    temp$family[i] <- ''
  }
}
x <- htmlTable(temp)
str(x)

# Group by family, name, date, and get lodging info
temp <-
  nights %>%
  group_by(family, name) %>%
  summarise(arrival = first(date),
            depart = last(date) + 1,
            nights_in_guialmons = paste0(format(date[which(status == 'lodging arranged by jc' &
                                             location == 'guialmons')], '%m-%d'), collapse = ', '),
            nights_on_own = paste0(format(date[which(status == 'arrange own lodging')], '%m-%d'), collapse = ', '),
            nights_in_arranged_bcn_appt = paste0(format(date[which(status == 'lodging arranged by jc' & location == 'barcelona')], '%m-%d'), collapse = ', '))

# # Clean out repeat family names
# for (i in nrow(temp):2){
#   while(temp$family[i-1] == temp$family[i]){
#     temp$family[i] <- ''
#   }
# }

# Give an id number and reorder


# Clean up column names
names(temp) <- gsub('_', ' ', Hmisc::capitalize(names(temp)))


# Write an html table
kable(temp, row.names = TRUE)

# Join nights and houses
df <- full_join(x = nights,
                y = houses)


####################################

# GET COST DETAILS

# Total number of nights arranged by j/c divided by total cost
sum(houses$cost) / 
  nrow(nights %>% filter(status == 'lodging arranged by jc'))

####################################

# Explore
nights %>% 
  filter(date == '2016-08-05') %>%
  filter(location == 'guialmons') %>%
  data.frame %>%
  View

# Person
nights %>%
  filter(name == 'Tom Brew') %>%
  filter(location == 'guialmons') %>%
  data.frame %>%
  View

# Get total people staying in guialmons
nights %>%
  filter(location == 'guialmons') %>%
  group_by(date) %>%
  tally %>% 
  View

# Get total capacity of guialmons
availabilities %>%
  group_by(date) %>%
  summarise(cap = sum(capacity))

nights %>%
  filter(location == 'barcelona') %>%
  group_by(date) %>%
  tally %>% 
  View

# Total nights in guialmons
x <- nights %>%
  filter(location == 'guialmons')

6120 / nrow(x)

# Create an html table
people <- 
  nights %>%
  group_by(name, location, sub_location) %>%
  tally