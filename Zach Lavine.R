#####################################

## How can we maximize Zach Lavine's points?

## All data is from NBA.com/stats

####################################

## Importing necessary libraries and setting the working directory
library(tidyverse)
setwd("/Users/quinyuter/Desktop/Bulls_Stats")

## Importing the necessary csv files
shot_locations_21_22 <- read_csv("Lavine_location_21_22.csv")
shot_locations_22_23 <- read_csv("Lavine_locations_22_23.csv")
shot_types_21_22 <- read_csv("Lavine_type_21_22.csv")
shot_types_22_23 <- read_csv("Lavine_types_22_23.csv")

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
##    location that Lavine takes.
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
## Not including back court, as Lavine did not take any shots from this
##    location.

points_percentage_vector_location <- c(locations_final[1, 6] * 2, locations_final[2, 6] * 2, 
                                       locations_final[3, 6] * 2, locations_final[4, 7] * 3, 
                                       locations_final[5, 7] * 3, locations_final[6, 7] * 3)

## Simulating if Lavine would take 1000 attempts at each shot, 
##    putting the results in a Data Frame, and relabeling each column
##    based on the location of said result.
total_area_points <- data.frame(lapply(points_percentage_vector_location,"*", 1000)) %>%
  rename('Restricted Area' = percent_FG, "In the Paint" = percent_FG.1, 
         "Mid Range" = percent_FG.2, "Left Corner 3" = percent_3P, 
         "Right Corner 3" = percent_3P.1, "Above Break 3" = percent_3P.2)

##################################################################

## Creating a vector of each of the percentages of each shot
##    type that Lavine takes.
## Each index included in the vector is only the percent of each
##    shot type, so [1, 6] is Alley oop percentage, [2, 6]
##    is Dunk, and so on.
## The equations included in the vector is necessary math, as the original
##    Data Frame did not separate two point shots and three point shots for
##    jump shots, bank shots, and fade away shots, thus I had to do the subtraction separately
##    to get two different percentages.
## Each index that was multiplied by two are shots taken in the two point range,
##    and each shot multiplied by three are shots taken in the three point range.
## Did not include 3 point attempt finger roll shot, as Lavine only took two
##    shots at this range and type, and missed both.


points_percentage_vector_type <- c(types_final[1, 6]*2, types_final[2, 6]*2, 
                                   types_final[3, 6]*2, (39/71)*2, 
                                   types_final[5, 6]*2, (120/293)*2, (185/475)*3, 
                                   (76/176)*2, (119/299)*3, (169/322)*2, (2/4)*3, 
                                   types_final[9, 6])

## Simulating if Lavine would take 1000 attempts at each shot, 
##    putting the results in a Data Frame, and relabeling each column
##    based on the type of said result.
total_types_points <- data.frame(lapply(points_percentage_vector_type,"*", 1000)) %>%
  rename('Alley Oop' = percent_FG, 'Dunk' = percent_FG.1, 'Hook Shot' = percent_FG.2, 
         'Finger Roll' = X1098.59154929577, 'Layup' = percent_FG.3,
         'Jump Shot 2pt' = X819.112627986348, 'Jump Shot 3pt' = X1168.42105263158, 
         'Bank Shot 2pt' = X863.636363636364, 'Bank Shot 3pt' = X1193.97993311037, 
         'Fade away 2pt' = X1049.68944099379, 'Fade away 3pt' = X1500, 
         'Tip Shot' = percent_FG.4)


#################################################################################
## Conclusion

## Per 1000 attempts, Lavine scored the most points from the right corner
##    3 point line for a total of 1450 points. The second and third most points
##    that Lavine scores is 1396.552 and 1362.799 from the left corner 3 and the
##    restricted area, respectively.

## Per 1000 attempts, Lavine scored the most points taking an alley oop for a 
##    total of 1857.143 points. In terms of shots that can be taken from
##    three point line, fade aways yield the highest amount of points for Lavine,
##    where he would shoot a total of 1500 points.

## So, in order for Zach Lavine to score the most amount of points possible,
##    he should either shoot an alley oop from the restricted area,
##    or a fade away shot from either the right or left corner 
##    3 point lines.



