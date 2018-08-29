
# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")

# Set files dirctory from where to read data (csv files). 
filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
#setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")

# Read the ball by ball details and match summary csv files previously prepared
matDet <- read.csv(file= "wrangled_BallByBallDataIPL.csv", header = TRUE, sep = ",")
matSumm <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, sep = ",")

# Start data exploration 
# First have a smaller file for one team 
matDet_csk <- filter (matDet, TeamNameBat == "Chennai Super Kings" & Season < 2015)
matSumm_csk <- filter (matSumm, TeamNameBat == "Chennai Super Kings" & Season < 2015)

# Explore Variation in Runs Scored, Overs faced, Wickets Lost
# 1st try plain vanilla
ggplot(data = matSumm_csk) +
  geom_histogram (mapping = aes(x = EOIRuns), binwidth = 10)
ggplot(data = matSumm_csk ) +
  geom_histogram(mapping = aes (x = EOIOver), binwidth = 2)
ggplot(data = matSumm_csk) +
  geom_histogram(mapping = aes (x = EOIWkts), binwidth = 1)
#Now explore against specific teams
ggplot(data = matSumm_csk, mapping = aes(x = EOIRuns)) +
  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 10)
ggplot(data = matSumm_csk, mapping = aes(x = EOIOver)) +
  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 2)
ggplot(data = matSumm_csk, mapping = aes(x = EOIWkts)) +
  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 1)
#Finally explore performance at specific venues
ggplot(data = matSumm_csk, mapping = aes(x = EOIRuns)) +
  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 10)
ggplot(data = matSumm_csk, mapping = aes(x = EOIOver)) +
  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 2)
ggplot(data = matSumm_csk, mapping = aes(x = EOIWkts)) +
  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 1)
# These above plots show there is variation in the final total by CSK. Time to explore covariations

#First let's explore covariation between total runs scored (EOIRuns) and opponents
ggplot(matSumm_csk) +
  geom_boxplot(aes(
    x = reorder(TeamNameBowl, EOIRuns, FUN = median),
    y = EOIRuns)) +
  coord_flip()
# Next explore EOIRuns at different venues (again continuous variable vs categorical variable)
ggplot(matSumm_csk) +
  geom_boxplot(aes(
    x = reorder(venueCity, EOIRuns, FUN = median),
    y = EOIRuns)) +
  coord_flip()
# Finally check EOIRuns scored in 1st innings (setting a target) vs 2nd innings (chasing a target)
ggplot(matSumm_csk, aes(x = Innings_No, y = EOIRuns)) +
  geom_point(alpha = 0.2)

# Now explore covariation between 2 continuous variables by binning one of the 2 continuous variables
# Say number of wickets lost in the Powerplay (6 overs) vs total runs scored (EOIRuns)
ggplot(data = matSumm_csk, mapping = aes(x = Over6Wkts, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over6Wkts, 1)), varwidth = TRUE)
# This plot clearly shows the fewer wickets lost in the 1st 6 overs, the higher the final total runs scored
# Let's see the effect of wickets lost at the end of 4th, 8th and 10th overs also on the EOIRuns:
ggplot(data = matSumm_csk, mapping = aes(x = Over4Wkts, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over4Wkts, 1)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over8Wkts, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over8Wkts, 1)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over10Wkts, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over10Wkts, 1)), varwidth = TRUE)
# The above plots confirm that fewer wickets lost, higher the median final runs scored

# Now explore covariation between 2 other continuos variables: Runs scored early on vs Final score
ggplot(data = matSumm_csk, mapping = aes(x = Over4Runs, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over4Runs, 5)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over6Runs, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over6Runs, 5)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over8Runs, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over8Runs, 10)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over10Runs, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over10Runs, 10)), varwidth = TRUE)
ggplot(data = matSumm_csk, mapping = aes(x = Over12Runs, y = EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over12Runs, 10)), varwidth = TRUE)
# These above plots reveal that while the final score is less directly proportional to the runs scored
# at the end of 4th and 6th overs, the dependency is very strong by the end of 8th over and stays that
# way at the end of 10th and 12th overs as well



ggplot (matDet_csk, aes (x = cumRuns, y = cumOver)) +
  geom_point(alpha = 0.2) +
  geom_smooth()

ggplot(data = matSumm, mapping = aes(x = TeamNameBat, y = EOIRunRate)) +
  geom_point(mapping = aes(color = TeamNameBat)) +
  geom_smooth()
  
