###################################

## How can we maximize Ayo Dosunmu's points?

## Got all data from NBA.com/stats

#####################################

## Importing necessary libraries and setting the working directory
library(tidyverse)
setwd("/Users/quinyuter/Desktop/Bulls_Stats")

## Importing the necessary csv files
shot_locations_21_22 <- read_csv("Dosunmu locations 21-22.csv")
shot_locations_22_23 <- read_csv("Ayo Dosunmu locations 22-23.csv")
shot_types_21_22 <- read_csv("Dosunmu types 21-22.csv")
shot_types_22_23 <- read_csv("Dosunmu types 22-23.csv")

## Creating total DataFrames regardless of year
locations <- bind_cols(shot_locations_21_22, shot_locations_22_23) %>%
  select(`SHOT AREA...1`, `FGM...2`, `FGM...13`, `FGA...3`, `FGA...14`, `3PM...5`, `3PM...16`, 
         `3PA...6`, `3PA...17`) %>%
  mutate(FGM = `FGM...2` +`FGM...13`, FGA = `FGA...3` + `FGA...14`, 
         threePM = `3PM...5` + `3PM...16`, threePA = `3PA...6` + `3PA...17`)
locations_final <- locations %>%
  select(`SHOT AREA...1`, FGM, FGA, threePM, threePA) %>%
  rename(`SHOT AREA` = `SHOT AREA...1`)

types <- bind_cols(shot_types_21_22, shot_types_22_23) %>%
  select(`SHOT TYPE SUMMARY...1`, `FGM...2`, `FGM...13`, `FGA...3`, `FGA...14`, `3PM...5`, `3PM...16`, 
          `3PA...6`, `3PA...17`) %>%
  mutate(FGM = `FGM...2` +`FGM...13`, FGA = `FGA...3` + `FGA...14`, 
          threePM = `3PM...5` + `3PM...16`, threePA = `3PA...6` + `3PA...17`)

types_final <- types %>%
  select(`SHOT TYPE SUMMARY...1`, FGM, FGA, threePM, threePA) %>%
  rename(`SHOT TYPE` = `SHOT TYPE SUMMARY...1`)

## Getting percentages for each type of shot
locations_final <- locations_final %>%
  mutate(percent_FG = FGM/FGA, 
         percent_3P = threePM/threePA)

types_final <- types_final %>%
  mutate(percent_FG = FGM/FGA, 
         percent_3P = threePM/threePA)
####################################################################

## Creating a vector of each of the percentages of each shot
##    location that Dosunmu takes.
## Each index included in the vector is only the percent of each
##    shot area, so [1, 6] is restricted area percentage, [2, 6]
##    is In the Paint, and so on.
## The first three shots in the vector include restricted area, 
##    in the paint, and mid range, which all yield two points
##    upon making the basket, thus being multiplied by 2.
## The latter three shots include left corner three pointer, 
##    right corner three pointer, and above the break three pointer,
##    all of which yield three points upon making the basket,
##    thus being multiplied by 3.
## Not including back court, as Dosunmu only took one shot
##  in that location, so the sample size isn't valid

  
points_percentage_vector_location <- c(locations_final[1, 6] * 2, locations_final[2, 6] * 2, 
                              locations_final[3, 6] * 2, locations_final[4, 7] * 3, 
                              locations_final[5, 7] * 3, locations_final[6, 7] * 3)

## Simulating if Dosumnu would take 1000 attempts at each shot, 
##    putting the results in a DataFrame, and relabeling each column
##    based on the location of said result.
total_area_points <- data.frame(lapply(points_percentage_vector_location,"*", 1000)) %>%
  rename('Restricted Area' = percent_FG, "In the Paint" = percent_FG.1, 
         "Mid Range" = percent_FG.2, "Left Corner 3" = percent_3P, 
         "Right Corner 3" = percent_3P.1, "Above Break 3" = percent_3P.2)

##########################################################################

## Creating a vector of each of the percentages of each shot
##    type that Dosunmu takes.
## Each index included in the vector is only the percent of each
##    shot type, so [1, 6] is Dunk percentage, [2, 6]
##    is Finger Roll, and so on.
## The equations included in the vector is necessary math, as the original
##    DataFrame did not separate two point shots and three point shots for
##    jump shots and hook shots, thus I had to do the subtraction separately
##    to get two different percentages.
## Each index that was multiplied by two are shots taken in the two point range,
##    and each shot multiplied by three are shots taken in the three point range.
## Did not include Alley Oop, as Dosunmu only took three shots in that category,
##    all of which were misses. Also, three is not a large enough sample size.


points_percentage_vector_type <- c(types_final[1, 6]*2, types_final[2, 6]*2, 
                                   types_final[3, 6]*2, types_final[4, 6]*2, 
                                   (78/143)*2, (70/186)*3, types_final[6, 6]*2, 
                                   (12/34)*2, (33/99)*3, types_final[8, 6]*2)

## Simulating if Dosumnu would take 1000 attempts at each shot, 
##    putting the results in a DataFrame, and relabeling each column
##    based on the type of said result.
total_types_points <- data.frame(lapply(points_percentage_vector_type,"*", 1000)) %>%
  rename('Dunk' = percent_FG, 'Finger Roll' = percent_FG.1, 'Tip Shot' = percent_FG.2, 
         'Layup' = percent_FG.3, 'Jump Shot 2pt' = X1090.90909090909, 'Jump Shot 3pt' = X1129.03225806452, 
         'Bank Shot' = percent_FG.4, 'Hook Shot 2pt' = X705.882352941176, 'Hook Shot 3pt' = X1000, 
         'Fadeaway' = percent_FG.5)

#################################################################################
## Conclusion

## Per 1000 attempts, Dosunmu scored the most points in the restricted area,
##    for a total of 1325.581 points. He scored the second most points from
##    the right corner three point line, with a total of 1301.205 points per 1000
##    attempted shots.

## Per 1000 attempts, Dosunmu scored the most points using a Dunk shot, for
##    a total of 1666.67 points. He scored the second points points using a
##    tip shot, for a total of 1500 points. In terms of three point shot types,
##    he scored the most using a fade away, with a total of 1308.824 points per
##    1000 attempted shots.

## So, in order for Dosunmu to score the most amount of points possible, he should
##    dunk or tip shot the ball while in the Restricted Area, or use a fade away
##    shot from the right corner three point area.









