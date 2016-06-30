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
  mutate(id = 1:39) %>%
  dplyr::select(family, name)
# Clean out repeat family names
for (i in nrow(temp):2){
  while(temp$family[i-1] == temp$family[i]){
    temp$family[i] <- ''
  }
}
x <- htmlTable(temp)

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

# Get number of nights in guialmons per person
x <- 
  nights %>%
  filter(location == 'guialmons') %>%
  group_by(name) %>%
  summarise(nights_in_guialmons = n())

# BY PERSON  -------------------------
# Total number of nights arranged by j/c divided by total cost
sum(houses$cost) / 
  nrow(nights %>% filter(status == 'lodging arranged by jc'))

# BY ROOM -------------------------
room_per_night <- 
  sum(houses$cost) / 
  nrow(occupations)
room_per_night

# Get number of room-nights occupied by each person
cost_per_person <-
  nights %>%
  filter(location == 'guialmons') %>%
  dplyr::select(date, name, family, house, room) %>%
  # Get the total number of people in that room that night
  left_join(occupations %>%
              rename(total_people_in_room = n)) %>%
  mutate(proportion_of_room = 1 / total_people_in_room) %>%
  mutate(cost = proportion_of_room * room_per_night) %>%
  # Get overall per person
  group_by(name) %>%
  summarise(family = first(family),
            number_of_nights_in_guialmons = n(),
            number_of_room_nights = sum(proportion_of_room),
            average_cost_per_night = mean(cost),
            total_lodging_cost = round(sum(cost))) %>%
  ungroup %>%
  mutate(food_and_water = 20) %>%
  mutate(total_euros = round(total_lodging_cost + food_and_water)) %>%
  mutate(total_dollars = round(total_euros * 1.115)) %>%
  arrange(family)
names(cost_per_person) <- Hmisc::capitalize(gsub('_', ' ', names(cost_per_person)))
kable(cost_per_person)

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