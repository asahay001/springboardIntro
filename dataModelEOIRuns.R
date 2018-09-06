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

# Start data modeling 
# First have a smaller file for one team: try Rajasthan Royals and CSk as the teams, one by one
#matDet_csk <- filter (matDet, TeamNameBat == "Rajasthan Royals" & Season < 2015)
#matSumm_csk <- filter (matSumm, TeamNameBat == "Rajasthan Royals" & Season < 2015)

matDet_csk <- filter (matDet, TeamNameBat == "Chennai Super Kings" & Season < 2015)
matSumm_csk <- filter (matSumm, TeamNameBat == "Chennai Super Kings" & Season < 2015)

# Abbreviating in my comments throughout: Chennai Super Kings = CSK 
# Try to see how CSK has fared against different opponents in terms of run rate in each match, ball-by-ball

cskEOIModel = lm(EOIRuns ~ venueCity, data = matSumm_csk)
summary (cskEOIModel)
# Above shows poor corelation between EOIRuns and VenueCity

#Obviously there is a strong positive corelation betwen EOIRuns and EOIRunRate as shown below:
  
plot(matSumm_csk$EOIRunRate, matSumm_csk$EOIRuns)
cskEOIModel = lm(EOIRuns ~ EOIRunRate, data = matSumm_csk)
summary (cskEOIModel)

#But let's explore relationships between EOIRuns and runs/wickets at the end of certain overs:
plot(matSumm_csk$Over6Runs, matSumm_csk$EOIRuns )
cskEOIModel = lm(EOIRuns ~ Over6Runs, data = matSumm_csk)
summary (cskEOIModel)

plot(matSumm_csk$Over10Wkts, matSumm_csk$EOIRuns )
cskEOIModel = lm(EOIRuns ~ Over10Wkts, data = matSumm_csk)
summary (cskEOIModel)

# Above shows no corelation in these single independent variables either with the dependent variable EOIRuns


#Now try 2 independent variables in the model: Runs and Wickets at variois points in an innings, esp early on
cskEOIModel = lm (EOIRuns ~ Over12Runs + Over6Wkts, data = matSumm_csk)
summary(cskEOIModel)
# The above just shows that number of wickets lost is not a significant independent variable; only Runs scored


cskEOIModel = lm (EOIRuns ~ Over6Runs + Over10Wkts + TeamNameBowl + venueCity, data = matSumm_csk)
summary(cskEOIModel)
# R2 value is still quite low for these combinations (0.3 to 0.44 for 4 and 6 over runs; wickets lost continue to not be significant

cskEOIModel = lm (EOIRuns ~ Over4Runs + Over6Runs + Over8Runs +TeamNameBowl + venueCity, data = matSumm_csk)
summary(cskEOIModel)
#No significant improvement even after adding over6Runs and over8Runs!! 


# So now try in which oversmilestone runs came
cskEOIModel = lm (EOIRuns ~ Over100Runs, data = matSumm_csk)
summary(cskEOIModel)
#Not till the 75th run, does R2 raches 0.46; at 100th run, it improves marginally to 0.52 Not good enough!

#Next try the milestone wickets falling:
cskEOIModel = lm (EOIRuns ~ Over5Wkts, data = matSumm_csk)
summary(cskEOIModel)
# Even with the 5th wicket down, R2 gets up to only 0.34. Wickets again have even less of a correlation

# Try a combination of these milestone: runs + wickets:
cskEOIModel = lm (EOIRuns ~ Over100Runs + Over4Wkts, data = matSumm_csk)
summary(cskEOIModel)
#Even 50 runs and 5 wickets gives an R2 of only 0.47!! 50 runs and 2 or 3 wickets have the same R2 of 0.40
# 100 runs and 4 wickets takes up to 0.61. Number of wickets lost is not significant!!

cskEOIModel = lm (EOIRuns ~ Over30Runs + Over10Runs, data = matSumm_csk)
summary(cskEOIModel)
#Only 0.46 R2 for the above for RR, and only 0.42 for CSk!

# cor(matSumm_csk)


