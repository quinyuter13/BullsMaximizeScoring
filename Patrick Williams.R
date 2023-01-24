#####################################

## How can we maximize Patrick William's points?

## All data is from NBA.com/stats

####################################

## Importing necessary libraries and setting the working directory
library(tidyverse)
setwd("/Users/quinyuter/Desktop/Bulls_Stats")

## Importing the necessary csv files
shot_locations_21_22 <- read_csv("Williams_location_21_22.csv")
shot_locations_22_23 <- read_csv("Williams_location_22_23.csv")
shot_types_21_22 <- read_csv("Williams_types_21_22.csv")
shot_types_22_23 <- read_csv("Williams_types_22_23.csv")
