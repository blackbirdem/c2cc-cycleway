library(dplyr)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/survey/'
setwd(directory)

# read csv of cleaned data
num <- read.csv('CC2C_form_clean.csv')
# make sure NA values are actua NA values and not strings
num[num == 'na'] <- NA

# convert yes/no responses to binary
# yes = 1, no = 0
yes_no_columns <- c('bike_ownership', 'bikeshare_use', 'bike_to_work',
                    'cycle_frequ_history', 'use_cycleway', 'leave_cycleway')
for (col in yes_no_columns) {
  num[col] <- as.numeric(num[col]=="yes")
}

# convert gender and ethnicity demographics to binary
num$dem_gender[num$dem_gender=="man"]<-1
num$dem_gender[num$dem_gender=="woman"]<-0
num$dem_gender[num$dem_gender=="non-binary"]<-NA


num$dem_ethnicity[is.na(num$dem_ethnicity)]<-0
num$dem_ethnicity[num$dem_ethnicity!="white"]<-0
num$dem_ethnicity[num$dem_ethnicity=="white"]<-1

# convert employment status and route change level to ordinal
num$dem_employment[num$dem_employment=="fullemp"]<-2
num$dem_employment[num$dem_employment=="fulled"]<-2
num$dem_employment[num$dem_employment=="partemp"]<-1
num$dem_employment[num$dem_employment=="parted"]<-1
num$dem_employment[num$dem_employment=="none"]<-0

num$route_change[num$route_change=="change"]<-3
num$route_change[num$route_change=="more"]<-2
num$route_change[num$route_change=="no"]<-1
num$route_change[num$route_change=="yes"]<-0

# fix ordering to be more intuitive
num$maintenance[num$maintenance==2]<-1
num$maintenance[num$maintenance==1]<-2

num$escort[num$escort==2]<-1
num$escort[num$escort==1]<-2

# fix scaling of likert scales
likert <- c(paste('perception', c('safety', 'speed', 'stress'), sep='_'),
            paste('perception_', c('safety', 'speed', 'stress'), '2', sep=''),
            paste('perception', c('safety', 'speed', 'stress'), 'all', sep='_'),
            'car_changes', 'walk_changes')
for (t in likert) {
  num[,t][num[,t] == 5]<- 2
  num[,t][num[,t] == 4]<- 1
  num[,t][num[,t] == 3]<- 0
  num[,t][num[,t] == 1]<- -1
  num[,t][num[,t] == 0]<- -2
}

# get binary values for each occurring multiple choice option
for (p in unique(unlist(strsplit(num$purpose, ",")))) {
  num$temp = lapply(num$purpose,
                    function(x) ifelse(p %in% unlist(strsplit(x, ',')), 1, 0))
  names(num)[names(num) == 'temp'] <- paste('x_purpose', p, sep = '_')
}
for (g in unique(unlist(strsplit(num$safety_gear, ",")))) {
  num$temp = lapply(num$safety_gear,
                    function(x) ifelse(g %in% unlist(strsplit(x, ',')), 1, 0))
  names(num)[names(num) == 'temp'] <- paste('x_safety_gear', g, sep = '_')
}
for (r in unique(unlist(strsplit(num$why_leave_cycleway, ",")))) {
  num$temp = lapply(num$why_leave_cycleway,
                    function(x) ifelse(r %in% unlist(strsplit(x, ',')), 1, 0))
  names(num)[names(num) == 'temp'] <- paste('x_leave_reason', r, sep = '_')
}

# get binary values for each occurring top ranked option
for (source in unique(num$stress_source_one)) {
  if (!is.na(source)) {
    num <- mutate(num, 'x_stress_source_{source}' := ifelse(stress_source_one == source, 1, 0))
  }
}
for (p in unique(num$primary_purpose)) {
  if (!is.na(p)) {
    num <- mutate(num, 'x_primary_purpose_{p}' := ifelse(primary_purpose == p, 1, 0))
  }
}

# remove columns that will be excluded from the statistics (e.g. free text questions)
non_num_cols <- c('object_id', 'global_id', 'oth_ethnicity',
                  'origin_choice', 'home_origin', 'other_origin', 'purpose', 'purpose_other',
                  'primary_purpose', 'primary_dest', 'safety_gear', 'stress_source', 'stress_source_one',
                  'why_leave_cycleway', 'why_leave_cycleway_other', 'extra',
                  'perception_compare', 'describe_changes', 'trial', 'creation_date',
                  'edit_date', 'cycle_frequ_history_reason', 'cycle_frequ_history_other',
                  'leave_cycleway_reason', 'leave_cycleway_reason_other', 'type', 'exp')
num <- num[, ! names(num) %in% non_num_cols, drop = F]

# make sure all entries are numeric
num <- mutate_all(num, function(x) as.integer(as.character(x)))

write.csv(num, 'C2CC_form_numeric.csv')
