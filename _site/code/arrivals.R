# Libraries
library(gsheet)
library(dplyr)
library(xtable)
library(knitr)
library(htmlTable)
library(chron)

# Link of the arrivals/departures spreadsheet
url_arrivals <- 'https://docs.google.com/spreadsheets/d/1v70i20A7-cL6gXjObqkIqEKvzmqoR0hH0BM7FvaaMTI/edit?usp=sharing'
arrivals <- gsheet2tbl(url_arrivals)

# Clean up a dates
arrivals$arrival_date <- as.Date(arrivals$arrival_date, '%m/%d/%Y')
arrivals$departure_date <- as.Date(arrivals$departure_date, '%m/%d/%Y')

# # Get fractions of time
# fractionify <- 
#   function(x){
#     z <- strsplit(x, ':')
#     hour <- as.numeric(unlist(lapply(z,function(y){y[1]})))
#     minute <- as.numeric(unlist(lapply(z,function(y){y[2]})))
#     the_time <- hour + (minute / 60)
#   }
# arrivals$arrival_time_frac <- fractionify(arrivals$arrival_time)

# Remove those with no time
arrivals <- 
  arrivals %>% filter(!is.na(arrival_time),
                      arrival_time != '')

# Get datetime
arrivals$arrival_date_time <- 
  chron(dates. = dates(format(arrivals$arrival_date, '%m/%d/%y')),
        times. = times(paste0(arrivals$arrival_time, ':00')))

# Group
grouped <- arrivals %>%
  group_by(arrival_date_time) %>%
  summarise(n = n(),
            who = paste0(name, collapse = ', '),
            family = paste0(unique(family), collapse = ', '))

# Define date cutoffs
cutoffs <-
  chron(dates. = dates(format(as.Date(c('2016-07-21',
                                        '2016-07-22',
                                        '2016-07-23',
                                        '2016-07-24')), '%m/%d/%y')),
        times. = times(paste0('00:00', ':00')))

# Plot
plot(x = grouped$arrival_date_time,
     y = grouped$n,
     las = 3,
     xlab = NA,
     ylab = 'Number of people arriving concurrently',
     main = 'Arrivals for PML 2016',
     cex.axis = 0.6,
     pch = 16,
     col = adjustcolor('red', alpha.f = 0.3))
abline(v = cutoffs, col = adjustcolor('darkred', alpha.f = 0.4),
       lwd = 2)
abline(v = grouped$arrival_date_time, col = adjustcolor('black', alpha.f = 0.2), lty = 2)
text(x = grouped$arrival_date_time,
     y = grouped$n,
     label = gsub(' Brew', '', gsub(',', '\n', grouped$who)),
     cex = 0.5)

