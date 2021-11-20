## OKC Thunder Technical Assessment - Jack Puncochar

# Set working directory 
setwd('C:/Users/punco/OneDrive/Desktop/JackProjects')

# Load packages
library(dplyr)

# Read in shots data
shots <- read.csv("shots_data.csv")

# create shot zones (Two Point (2PT), Corner 3 (C3), Non-Corner 3 (NC3))
# hypotenuse c = sqrt(x^2 + y^2), if c > 23.75, then NC3
shots <- shots %>% dplyr::mutate(shotZone = ifelse(abs(x) > 22 & y <= 7.8, "C3", 
                                            ifelse(sqrt(x^2 + y^2) > 23.75, "NC3", "2PT")))

# Percentage of team shots within each zone
# total shots 
totalShots = shots %>% dplyr::group_by(team) %>% dplyr::summarise(totalShots = n()) # 280 for both teams

# Percentage of team shots by shot zone
shots %>% dplyr::group_by(team, shotZone) %>% dplyr::summarise(percentage = n()/280)

##### Calculate eFG% ... = FGM + (.5 * 3PM) / FGA #####

# First roll up the data - get made and attempted shots by shot zone
rollUp = shots %>% dplyr::group_by(team, shotZone) %>% summarise(totalMade = sum(fgmade), shotAttempts = n())

# Get 3PM and 3PA
rollUp = rollUp %>% dplyr::mutate(madeThrees = ifelse(shotZone=="2PT", 0, totalMade))

# eFG%
rollUp %>% dplyr::mutate(eFG = (totalMade + (.5 * madeThrees)) / shotAttempts)

