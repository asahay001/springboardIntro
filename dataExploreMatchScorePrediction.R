
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

# Abbreviating in my comments throughout: Chennai Super Kings = CSK 
# Try to see how CSK has fared against different opponents in terms of run rate in each match, ball-by-ball:
###### WRITE THE PLOTS TO a pdf file ######################
#########################################
ggplot(data = matDet_csk, mapping = aes( x= cumRuns, y = cumOver)) +
  #geom_point(mapping = aes(color = TeamNameBowl)) +
  geom_smooth(
    mapping = aes(linetype = TeamNameBowl, color = TeamNameBowl),
    method = "lm", se = FALSE)

dev.copy(pdf, "plots.pdf")   # Needed to create a plot 1st above for this command to work; otherwise complains of null device

# First let's get an overview of how CSK fared against each of its opponents isn terms of run rate

ggplot(data = matDet_csk, mapping = aes( x= cumRuns, y = cumOver)) +
  #geom_point(mapping = aes(color = TeamNameBowl)) +
  geom_smooth(
    mapping = aes(linetype = TeamNameBowl, color = TeamNameBowl),
    method = "lm", se = FALSE)
# The above plot shows CSK team has the best run rate against King's X1 Punjab and scores most against them
# and there is a bit of spread against other teams


# Next, explore Variation in Runs Scored, Overs faced, Wickets Lost
# 1st try plain vanilla
ggplot(data = matSumm_csk) +
  geom_histogram (mapping = aes(x = EOIRuns), binwidth = 10)

#ggplot(data = matSumm_csk ) +
#  geom_histogram(mapping = aes (x = EOIOver), binwidth = 2)
#ggplot(data = matSumm_csk) +
#  geom_histogram(mapping = aes (x = EOIWkts), binwidth = 1)

#Now explore against specific teams
ggplot(data = matSumm_csk, mapping = aes(x = EOIRuns)) +
  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 10)

#ggplot(data = matSumm_csk, mapping = aes(x = EOIOver)) +
#  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 2)
#ggplot(data = matSumm_csk, mapping = aes(x = EOIWkts)) +
#  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 1)

#Finally explore performance at specific venues
ggplot(data = matSumm_csk, mapping = aes(x = EOIRuns)) +
  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 10)

#ggplot(data = matSumm_csk, mapping = aes(x = EOIOver)) +
#  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 2)
#ggplot(data = matSumm_csk, mapping = aes(x = EOIWkts)) +
#  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 1)

# These above plots show there is variation in the final total by CSK. Time to explore covariations

#First let's explore covariation between total runs scored (EOIRuns) and opponents
ggplot(matSumm_csk) +
  geom_boxplot(aes(
    x = reorder(TeamNameBowl, EOIRuns, FUN = median),
    y = EOIRuns)) +
  coord_flip()
# The above plot confirms that over the years, CSK has scored consistently higher against King's X1 Punjab
# than other opponents; and amongst the lowest against Rajasthan Royals. But there is a large
# variation in the number of runs against all opponets as shown by the margins of the inter-quartile range

# Next explore EOIRuns at different venues (again continuous variable vs categorical variable)
ggplot(matSumm_csk) +
  geom_boxplot(aes(
    x = reorder(venueCity, EOIRuns, FUN = median),
    y = EOIRuns)) +
  coord_flip()
# The above plot shows that over the years, the pitch at Kochi produces the least number of runs on an average 
# for CSK team and Vishakapatnam produces the highest runs on an average (median).
# But the inter-quartile range is high at almost all venues, so there are other factors involved as
# well in the final score for CSK in each match
# Let's plot the final scores for CSK against opponents at different venues and look at the smooth lm line:
ggplot(data = matSumm_csk, mapping = aes( x= EOIRuns, y = venueCity)) +
  geom_smooth(
    mapping = aes(linetype = TeamNameBowl, color = TeamNameBowl),
    method = "lm", se = FALSE)
# The above plot shows considerable variation at the same venue aginst the same opposition.
# There are some venues, like its home ground of Chennai, where CSK has played multiple opponents
# They have also played multiple opponents at Mumbai

# Finally check EOIRuns scored in 1st innings (setting a target) vs 2nd innings (chasing a target)
ggplot(matSumm_csk, aes(x = Innings_No, y = EOIRuns, color = TeamBattingWon)) +
  geom_point(alpha = 0.2)
# While the above plot shows runs in Innings 1 are a bit higher for CSK than when it chases (Innings 2),
# but that may be because it is surpassing the opponent's scores in the 2nd innings before the allotted 
# 20 overs

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

# Next look at some facet grids: First the impact of wickets lost in Power Play on the final score:
ggplot(data = matSumm_csk) +
  geom_point(mapping = aes(x = EOIRuns, y = TeamNameBowl)) +
  facet_wrap(~Over6Wkts, nrow = 3)
# The above facet plot shows number of wickets lost in the 1st 6 overs (Powerlay overs) significantly
# influence the final score of CSK regardless of the opponent

# Next look at the impact of run rate (runs scored) in the PowerPlay (1st 6 overs) on the final score:
ggplot(data = matSumm_csk) +
  geom_point(mapping = aes(x = EOIRuns, y = TeamNameBowl)) +
  facet_wrap(~ (group = cut_width(Over4Runs, 5)), nrow = 3)
# the above plot seems to suggest that the initial run rate in the 1st 6 overs is less of a factor for the final score

# The above exploration should next be repeated for each opponent of CSK to explore what variables may 
# predict its final score against the particular opponent

#dev.copy(pdf, "plots.pdf")
dev.off()


  
