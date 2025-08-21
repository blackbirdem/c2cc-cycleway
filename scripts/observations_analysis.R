library(dplyr)
library(stringr)
library(purrr)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/observations/'
setwd(directory)

# read the csv of field observations
obs <- read.csv('Field Observations C2CC - data.csv')

# duplicate rows with multiple counts
dup_rows <- subset(obs, !is.na(obs$count))

for (rown in 1:nrow(dup_rows)) {
  row <- dup_rows[rown, ]
  count <- as.numeric(row['count'])
  for (i in 1:count) {
    obs <- rbind(obs, row)
  }
}

# replace moby (inconsistent observation)
obs[obs=='moby'] <- 'bike'
# convert comments to type
obs$type[obs$comment == 'w child'] <- 'bike (with child)'

# get counts per hour
# extract date
obs$datetime <- as.POSIXct(obs$datetime, format="%d/%m/%Y %H:%M", tz=Sys.timezone())
obs$date <- as.Date(obs$datetime)
obs_count <- obs %>% group_by(date, location) %>% summarise(Count=sum(count))

write.csv(obs_count, 'observation_counts.csv')

# get type of cyclist
cyclists <- obs %>% group_by(obs$type) %>% summarise(percentage=n()/nrow(.))
names(cyclists)[names(cyclists) == 'obs$type'] <- 'type'
cyclists <- cyclists[-c(1), ]
cyclists$percent <- paste(round(cyclists$percentage * 100, 1),"%",sep="")

write.csv(cyclists, 'cyclist_type.csv')

# get transgressions
trgr <- unique(obs$transgression)
split_values <- unlist(str_split(obs$transgression, ","))
trgr <- unique(split_values)

transgressions <- tibble(
  name = trgr,
  percentage = map_dbl(trgr, function(v) {
    mean(sapply(split_values, function(row) v %in% row))
  })
)
transgressions$percent <- paste(round(transgressions$percentage * 100, 1),"%",sep="")

write.csv(transgressions, 'transgressions_percentage.csv') 

# only cyclist who arrived at red
reds <- obs[!(is.na(obs$type_red) | obs$type_red==""), ]
rtrgr <- unique(reds$transgression)
split_values <- unlist(str_split(reds$transgression, ","))
rtrgr <- unique(split_values)

red_transgressions <- tibble(
  name = rtrgr,
  percentage = map_dbl(rtrgr, function(v) {
    mean(sapply(split_values, function(row) v %in% row))
  })
)
red_transgressions$percent <- paste(round(red_transgressions$percentage * 100, 1),"%",sep="")

write.csv(red_transgressions, 'transgressions_red_percentage.csv') 