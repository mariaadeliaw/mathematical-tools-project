library(tidyverse)  
library(janitor)    # cleaning column names
# library(mice)       # handling missing data

lizards <- read_csv("lizard.csv")

# clean column names
lizards <- janitor::clean_names(lizards)
names(lizards_clean) <- make_clean_names(names(lizards))


# summary of the data
summary(lizards)

# delete the entries that has all NAs
lizards_clean <- lizards[!apply(lizards, 1, function(row) all(is.na(row))), ]

# count missing values in each column
colSums(is.na(lizards_clean))


# visualizing missing data
# library(VIM)
# missing_data_summary <- aggr(
#   lizards_clean, 
#   col = c("skyblue", "red"), 
#   numbers = TRUE, 
#   sortVars = TRUE, 
#   labels = abbreviate(names(lizards_clean), minlength = 10), 
#   cex.axis = 0.7, 
#   gap = 3, 
#   ylab = c("Missing data", "Pattern"), 
#   only.miss = TRUE, 
#   top = 5 
# )

# Based on this data screening, rcm has the highest NA values so decided to not use it
# Otherwise use mice if explanation from the data is needed


