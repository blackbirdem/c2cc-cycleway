library(ggplot2)
library(tidyr)
library(dplyr)

#SETUP
# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/'
setwd(directory)

# fill in folder path and file names
# files available at https://data.gov.ie/dataset/dublin-city-centre-cycle-counts
folder <- 'data/cycle_counts/'
fn22 <- 'cycle-counts-2022'
fn23 <- 'cycle-counts-1-jan-31-december-2023'
fn24 <- 'cycle-counts-1-jan-31-december-2024'
fn25 <- 'cycle-counts-1-jan-9-june-2025'

# read the csv
f22 <- read.csv(paste(folder, fn22, '.csv', sep=''))
f23 <- read.csv(paste(folder, fn23, '.csv', sep=''))
f24 <- read.csv(paste(folder, fn24, '.csv', sep=''))
f25 <- read.csv(paste(folder, fn25, '.csv', sep=''))


# FUNCTIONS
# subset only relevant columns
subset_cc <- function(df) {
  if (bool) {
    subset <- df[c("Time","Clontarf...Pebble.Beach.Carpark",
                   "Clontarf...Pebble.Beach.Carpark.Cyclist.IN",
                   "Clontarf...Pebble.Beach.Carpark.Cyclist.OUT")]
  } else {
    subset <- df[c("Time","Clontarf...Pebble.Beach.Carpark",
                   "Clontarf...Pebble.Beach.Carpark.Cyclist.West",
                   "Clontarf...Pebble.Beach.Carpark.Cyclist.East")]
  }
  
  colnames(subset) = c('Datetime', 'Total', 'West', 'East')
  return(subset)
}

# convert characters to Datetime and split into date and time
split_datetime <- function(df) {
  df$Datetime <- as.POSIXct(df$Datetime, format="%d/%m/%Y %H:%M", tz=Sys.timezone())
  
  # extract time
  df$time <- format(as.POSIXct(df$Datetime),format = "%H:%M")
  # extract month
  df$month <- format(as.POSIXct(df$Datetime),format = "%m")
  # extract date
  df$date <- as.Date(df$Datetime)
  
  return(df)
}

# run all formatting functions
format_df <- function(df) {
  df <- subset_cc(df)
  df <- split_datetime(df)
  
  df[is.na(df)] <- 0
  
  return(df)
}

calc_hourly <- function(df) {
  df <- df %>% group_by(month, time) %>% summarise_each(funs(mean))
  
  return(df)
}

calc_monthly <- function(df) {
  year <- format(as.POSIXct(df$Datetime),format = "%Y")
  
  # sum up counts per day
  df <- aggregate(df[, 2], list(df$date), sum)
  # extracting month again
  df$month <- format(as.POSIXct(df$Group.1),format = "%m")
  
  # get mean count per day
  df <- aggregate(df[, 2], list(df$month), mean)
  colnames(df) = c('Month', 'Mean (per day)')

  df$Year <- year[1]
  
  return(df)
}


# EXECUTION 
# select only Clontarf counts
bool <- TRUE
f22 <- format_df(f22)
bool <- FALSE
f23 <- format_df(f23)
f24 <- format_df(f24)
f25 <- format_df(f25)

# sum up counts per month and hour
hours22 <- calc_hourly(f22)
hours23 <- calc_hourly(f23)
hours24 <- calc_hourly(f24)
hours25 <- calc_hourly(f25)

hours <- full_join(hours22, hours23)
hours <- full_join(hours, hours24)
hours <- full_join(hours, hours25)

write.csv(hours24, paste(directory, folder, 'cycle_count_hours24.csv', sep=''))
write.csv(hours, paste(directory, folder, 'cycle_count_hours.csv', sep=''))

# sum up counts per month
month22 <- calc_monthly(f22)
month23 <- calc_monthly(f23)
month24 <- calc_monthly(f24)
month25 <- calc_monthly(f25)

# join different years together
months <- full_join(month22, month23)
months <- full_join(months, month24)

write.csv(months, paste(directory, folder, 'cycle_count_months.csv', sep=''))
write.csv(month25, paste(directory, folder, 'cycle_count_month25.csv', sep=''))


#PLOTS
# get mean of hourly values
h <- hours %>%
  group_by(time) %>%
  summarise(`Mean (per hour)` = mean(Total))

# get mean of monthly values
m <- months %>%
  group_by(Month) %>%
  summarise(`Mean (per day)` = mean(`Mean (per day)`))

# format 2025 for comparison
m25 <- month25
m25[6,] <- c('06', 0, 0)
for (i in list('07', '08', '09', '10', '11', '12')) {
  print(i)
  m25 <- rbind(m25, c(i, 0, 0))
}
m25$`Mean (per day)` <- as.numeric(m25$`Mean (per day)`)

# print plots
ggplot(h, aes(x=time, y=`Mean (per hour)`)) + geom_bar(stat = "identity") + ylim(0,125) +
  labs(title="Cycle counts January 2022 - May 2025", x = "Time") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

ggplot(m, aes(x=Month, y=`Mean (per day)`)) + geom_bar(stat = "identity") + ylim(0,2000) +
  labs(title="Cycle counts 2022-2024")
ggplot(m25, aes(x=Month, y=`Mean (per day)`)) + geom_bar(stat = "identity") + ylim(0,2000) +
  labs(title="Cycle counts 2025")