## We will try to predict End of Innings (EOI) runs for Innings 1 at the end of 6th, 10th and 15th overs
## for these 20 overs max - a - side match. 
## And then for the team batting 2nd (Innings 2), try to predict their EOI score at the end of their
# 6th, 10th and 15th overs, which for the match are the 26th, 30th and 35th overs respectively
# And in the 2nd part, we will try to predict the match winner at the end of 26th, 30th and 35th overs 
## of each match, store the 3 predictions for each match, and compare it to the actual match result
######### This file tries to create the best fit linear model at the end of 6th, 10th, 15th, 26th, 30th, 35th 
########### overs to predict EOI runs for the 2 innings respectively

# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")

# Set files dirctory from where to read data (csv files). 
filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
#setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")

# Read the match summary csv file previously prepared with scorePred_ver1.R
matSummDet <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, sep = ",")

# Since match data available is for seasons 2008 through 2017, let's use data from 2008 through season 2014 as
# training data (7 seasons), and then test the EOI score prediction model on seasons 2015, 
# then 2016, and finally 2017, one match at a time based on who plays who and where 

maxSeason <- 2014

createMatchDataSlice_fun <- function (dataFrame = matSummDet, minSeason, maxSeason, matchId = NULL) {
  if (is.null(matchId)) {
    x< 2
    matSumm_df <- filter(dataFrame, between (Season,  minSeason, maxSeason))  # Slice of data for best fit model
  } else {
        y <- 2
        matSumm_df <- filter(dataFrame, between (Season,  minSeason, maxSeason) & Match_id == matchId)
    }
  return (matSumm_df)
}

matSummTrng <- createMatchDataSlice_fun(minSeason = 2008, maxSeason = maxSeason)  # Training data: 2008 through 2014 matches

# SO let's start the EOI runs prediction at the end of 6th over (then 10th and finally 15th overs for Innings 1)

Over6modeling_fun <- function() {
  # First, check the correlation among different independent variables:
  matSumm_cor1 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                                 select = c("Inn1EOIRuns", "Over2Runs", "Over2Wkts","Over3Runs", "Over3Wkts", 
                                            "Over4Runs", "Over4Wkts", "Over5Runs", "Over5Wkts",
                                            "Over6Runs", "Over6Wkts", 
                                            "Over2RR", "Over3RR", "Over4RR", "Over5RR", "Over6RR", 
                                            "interactionVenueBatTeam" , "interactionCurrTeams")))
  cor(matSumm_cor1)
  # As expected, correlation with EOI Runs is higher as the number of overs played increases; 
  # Runs scored have a higher correlation with EOI Runs than Wickets lost, and Run Rate has the same correlation as Runs Scored
  
  #matSummTrng[is.na(matSummTrng)] <- 0  # NA values can be safely replaced with 0 in this dataset for further computation
  
  runsInn1EOIAtOver6Model1 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts + Over5Wkts + BatSecond + venueCity, 
                                data = matSummTrng)
  summary (runsInn1EOIAtOver6Model1)
  AIC(runsInn1EOIAtOver6Model1)
  # Running the above shows adjusted R2 of 0.37 and AIC of 4207, 
  # with Over6Wkts, Over 5Runs, Over6Runs as significant coefficients in that order (also venue at Dharamshala)
  
  # let's try more linear models to determine best fit, dropping one independent variable at a time:
  runsInn1EOIAtOver6Model2 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts + BatSecond + venueCity, 
                                data = matSummTrng)
  summary (runsInn1EOIAtOver6Model2)
  AIC(runsInn1EOIAtOver6Model2)
  # Running the 2nd model shows pretty much the same results as the 1st model. Next, drop 1 more variable:
  runsInn1EOIAtOver6Model3 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts + BatSecond, 
                                data = matSummTrng)
  summary (runsInn1EOIAtOver6Model3)
  AIC(runsInn1EOIAtOver6Model3)
  # This had adjusted R2 of 0.36 and a slightly lower AIC of 4188. SIgnificant coeff: Over6Wkts, Over5Runs
  runsInn1EOIAtOver6Model4 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver6Model4)
  AIC(runsInn1EOIAtOver6Model4)
  # This again had adjusted R2 of 0.36 and a slightly lower AIC of 4179. And all coefficeints as significant
  # Try adding Over 5Wkts to see if it improves the model fit:
  runsInn1EOIAtOver6Model5 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts + Over5Wkts, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver6Model5)
  AIC(runsInn1EOIAtOver6Model5)
  # This did not improve the model fit. Add venueCity back to see if it improves the 4th model: 
  runsInn1EOIAtOver6Model6 = lm(Inn1EOIRuns ~ Over6Runs + Over5Runs + Over6Wkts + venueCity, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver6Model6)
  AIC(runsInn1EOIAtOver6Model6)
  # This barely improved the adjusted R2 to 0.38 from 0.37. So stick with the 4th model as the best predictor after 5 overs of a match
  # let's rename the best fit model after 6 overs to have a more generic name:
  runsInn1EOIAtOver6Model <- runsInn1EOIAtOver6Model4
  return (runsInn1EOIAtOver6Model)  # end of Over6modeling_fun
}

Over10modeling_fun <- function() {
  # First, check the correlation among different independent variables:
  matSumm_cor1 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                                 select = c("Inn1EOIRuns", "Over6Runs", "Over6Wkts","Over7Runs", "Over7Wkts", 
                                            "Over8Runs", "Over8Wkts", "Over9Runs", "Over9Wkts",
                                            "Over10Runs", "Over10Wkts", 
                                            "Over6RR", "Over7RR", "Over8RR", "Over9RR", "Over10RR", 
                                            "interactionVenueBatTeam" , "interactionCurrTeams")))
  cor(matSumm_cor1)
  # As expected, correlation with EOI Runs is higher as the number of overs played increases; 
  # Runs scored have a higher correlation with EOI Runs than Wickets lost, and Run Rate has the same correlation as Runs Scored
  
  runsInn1EOIAtOver10Model1 = lm(Inn1EOIRuns ~ Over10Runs + Over9Runs + Over10Wkts + Over9Wkts + BatSecond + 
                                venueCity + toss, data = matSummTrng)
  summary (runsInn1EOIAtOver10Model1)
  AIC(runsInn1EOIAtOver10Model1)
  # Running the above shows adjusted R2 of 0.55 and AIC of 4063, 
  # with Over10Wkts, Over10Runs as significant coefficients in that order
  
  # let's try more linear models to determine best fit, dropping one independent variable at a time:
  runsInn1EOIAtOver10Model2 = lm(Inn1EOIRuns ~ Over10Runs + Over9Runs + Over10Wkts + BatSecond, 
                                data = matSummTrng)
  summary (runsInn1EOIAtOver10Model2)
  AIC(runsInn1EOIAtOver10Model2)
  # Running the 2nd model shows pretty much the same results as the 1st model. Next, drop 1 more variable:
  runsInn1EOIAtOver10Model3 = lm(Inn1EOIRuns ~ Over10Runs + Over9Runs + Over10Wkts, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver10Model3)
  AIC(runsInn1EOIAtOver10Model3)
  # This had adjusted R2 of 0.53 and a slightly lower AIC of 4043. SIgnificant coeff: Over10Wkts, Over10Runs
  runsInn1EOIAtOver10Model4 = lm(Inn1EOIRuns ~ Over10Runs + Over10Wkts, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver10Model4)
  AIC(runsInn1EOIAtOver10Model4)
  # This again had adjusted R2 of 0.53 and a slightly lower AIC of 4041. And all coefficeints as significant
  # Try adding Over 5Wkts to see if it improves the model fit:
  runsInn1EOIAtOver10Model5 = lm(Inn1EOIRuns ~ Over10Runs + Over10Wkts + Over9Wkts, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver10Model5)
  AIC(runsInn1EOIAtOver10Model5)
  # This did not improve the model fit. Add venueCity back to see if it improves the 4th model: 
  runsInn1EOIAtOver10Model6 = lm(Inn1EOIRuns ~ Over10Runs + Over9Runs + Over10Wkts + venueCity, 
                                data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver10Model6)
  AIC(runsInn1EOIAtOver10Model6)
  # This barely improved the adjusted R2 to 0.54 from 0.53. So stick with the 4th model as the best predictor after 5 overs of a match
  # let's rename the best fit model after 6 overs to have a more generic name:
  runsInn1EOIAtOver10Model <- runsInn1EOIAtOver10Model4
  return (runsInn1EOIAtOver10Model)  # end of Over10modeling_fun
}

## Call the functions for linear model for predicting EOI runs at the end of various overs

runsInn1EOIAtOver6Model <- Over6modeling_fun()
runsInn1EOIAtOver10Model <- Over10modeling_fun()


