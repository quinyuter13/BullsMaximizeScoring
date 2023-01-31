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
##    location that Williams takes.
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
## Not including back court, as Williams did not take any shots from this
##    location.

points_percentage_vector_location <- c(locations_final[1, 6]*2, locations_final[2, 6]*2, 
                                       locations_final[3, 6]*2, locations_final[4, 6]*3, 
                                       locations_final[5, 6]*3, locations_final[6, 6]*3)

## Simulating if Williams would take 1000 attempts at each shot, 
##    putting the results in a Data Frame, and relabeling each column
##    based on the location of said result.
total_area_points <- data.frame(lapply(points_percentage_vector_location,"*", 1000))%>%
  rename('Restricted Area' = percent_FG, "In the Paint" = percent_FG.1, 
         "Mid Range" = percent_FG.2, "Left Corner 3" = percent_FG.3, 
         "Right Corner 3" = percent_FG.4, "Above Break 3" = percent_FG.5)

##################################################################

## Creating a vector of each of the percentages of each shot
##    type that Williams takes.
## Each index included in the vector is only the percent of each
##    shot type, so [3, 6] is Dunk percentage, [4, 6]
##    is Fadeaway, and so on.
## The equations included in the vector is necessary math, as the original
##    Data Frame did not separate two point shots and three point shots for
##    jump shots, bank shots, and fade away shots, thus I had to do the subtraction separately
##    to get two different percentages.
## Each index that was multiplied by two are shots taken in the two point range,
##    and each shot multiplied by three are shots taken in the three point range.
## Did not include Alley Oops, Bank Shot, or Tip Shot, as there was not a large
##    enough sample size to get an accurate response.

points_percentage_vector_type <- c(types_final[3, 6]*2, types_final[4, 6]*2, 
                                   types_final[5, 6]*2,(53/123)*2,
                                   types_final[7, 7]*3, types_final[8, 6]*2, 
                                   types_final[9, 6]*2)

## Simulating if Williams would take 1000 attempts at each shot, 
##    putting the results in a Data Frame, and relabeling each column
##    based on the type of said result.
total_types_points <- data.frame(lapply(points_percentage_vector_type,"*", 1000))%>%
  rename('Dunk' = percent_FG, 'Fadeaway' = percent_FG.1, 'Finger Roll' = percent_FG.2,
         'Jump Shot 2pt' = X861.788617886179, 'Jump Shot 3pt' = percent_3P, 
         'Layup' = percent_FG.3, 'Tip Shot' = percent_FG.4)

#################################################################################
## Conclusion

## Per 1000 attempts, Williams scored the most points from the left corner
##    3 point line for a total of 1538.462 points. The second most points he scored
##    was from the restricted area, for a total of 1256.198 points.

## Per 1000 attempts, Williams scored the most points taking a Finger Roll shot for a 
##    total of 1714.286 points. In terms of shots that can be taken from
##    three point line, Jump shots yield the highest amount of points for Williams,
##    where he would shoot a total of 1293.103 points.

## So, in order for Patrick Williams to score the most amount of points possible,
##    he should either shoot a Finger Roll from the restricted area,
##    or a Jump shot from either the left corner 3 point line.         

