library(dplyr)
library(stringr)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/survey/'
setwd(directory)

# read the csvs of survey data and question names
responses <- read.csv('CC2C_form_0.csv')
headers <- read.csv('headers.csv')

# read survey data as characters
data <- as.data.frame(lapply(responses, as.character))

# drop attribution columns since they are all anonymous
data <- data[, ! names(data) %in% c('Creator', 'Editor'), drop = F]

# join question ref names to responses df
data <- full_join(headers[, colnames(data)], data)
data <- as.data.frame(lapply(data, type.convert, as.is = TRUE))

# replace header row with question references instead of full question
# remove full text questions from df
questions <- names(data)
names(data) <- data[1,]
data[1,] <- questions

# remove introduction text columns
data[3:7] <- list(NULL)
# remove other non-response text columns and geodata columns
away_cols <- c('demographics', 'cycling_general', 'c2cc_page', 'c2cc_img', 'c2cc',
               'additional', 'closing', 'thanks', 'bye', 'NA', 'NA.1', 'NA.2', 'NA.3',
               'leave_cycleway_geodata', 'leave_cycleway_point', 'numStops', 'x', 'y')
data <- data[, ! names(data) %in% away_cols, drop = F]
# remove invalid row
data <- data[-87,]

# join alternative questions that essentially ask the same thing
likert_cols <- c('perception_safety', 'perception_speed', 'perception_stress')
for (sim in likert_cols) {
  data$sim1 <- data[, sim]
  data$sim2 <- data[, paste(sim, '2', sep='')]
  data <- mutate(data, temp = coalesce(sim1, sim2))
  names(data)[names(data) == 'temp'] <- paste(sim, 'all', sep = '_')
  data <- data[, ! names(data) %in% c('sim1', 'sim2'), drop = F]
}

# calculate count of multiple-choice variables
data$purpose_dummy <- str_count(data$purpose, ",")+1
data$safety_gear_dummy <- str_count(data$safety_gear, ",")+1

# get top ranked element from ranking question
f <- function(x) {
  unlist(strsplit(x, ","))[1]
}
data$stress_source_one <- lapply(data$stress_source, f)

# remove string rows
clean <- data[-c(1, 2), ]
# set empty rows to NA
clean[clean == '' ] <- NA

# stringify df and write to csv
string <- apply(clean,2,as.character)
write.csv(string, 'CC2C_form_clean.csv')