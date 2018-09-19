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
  # This again had adjusted R2 of 0.36 and a slightly lower AIC of 4179. And all coefficients as significant
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
  # This barely improved the adjusted R2 to 0.38 from 0.37. So stick with the 4th model as the best predictor after 6 overs of a match
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
  # This again had adjusted R2 of 0.53 and a slightly lower AIC of 4041. And all coefficients as significant
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
  # This barely improved the adjusted R2 to 0.54 from 0.53. So stick with the 4th model as the best predictor after 10 overs of a match
  # let's rename the best fit model after 6 overs to have a more generic name:
  runsInn1EOIAtOver10Model <- runsInn1EOIAtOver10Model4
  return (runsInn1EOIAtOver10Model)  # end of Over10modeling_fun
}

Over15modeling_fun <- function() {
  # First, check the correlation among different independent variables:
  matSumm_cor1 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                                 select = c("Inn1EOIRuns", "Over15Runs", "Over15Wkts",
                                            "Over14Runs", "Over14Wkts", 
                                            "Over12Runs", "Over12Wkts", "Over10Runs", "Over10Wkts",
                                            "Over6Runs", "Over6Wkts", 
                                            "interactionVenueBatTeam" , "interactionCurrTeams")))
  cor(matSumm_cor1)
  # As expected, correlation with EOI Runs is higher as the number of overs played increases; 
  # Runs scored have a higher correlation with EOI Runs than Wickets lost, and Run Rate has the same correlation as Runs Scored
  
  runsInn1EOIAtOver15Model1 = lm(Inn1EOIRuns ~ Over15Runs + Over14Runs + Over15Wkts + Over14Wkts + 
                                   BatSecond + 
                                   venueCity + toss, data = matSummTrng)
  summary (runsInn1EOIAtOver15Model1)
  AIC(runsInn1EOIAtOver15Model1)
  # Running the above shows adjusted R2 of 0.80 and AIC of 3683, 
  # with Over15Wkts, Over15Runs and Over 14 Wkts as significant coefficients in that order
  
  # let's try more linear models to determine best fit, dropping one independent variable at a time:
  runsInn1EOIAtOver15Model2 = lm(Inn1EOIRuns ~ Over15Runs + Over15Wkts + Over14Wkts + BatFirst * BatSecond, 
                                 data = matSummTrng)
  summary (runsInn1EOIAtOver15Model2)
  AIC(runsInn1EOIAtOver15Model2)
  # Running the 2nd model shows R2 of .81 and AIC of 3714. Next, drop 1 more variable:
  runsInn1EOIAtOver15Model3 = lm(Inn1EOIRuns ~ Over15Runs + Over15Wkts + Over14Wkts, 
                                 data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver15Model3)
  AIC(runsInn1EOIAtOver15Model3)
  # This had adjusted R2 of 0.79 and a slightly lower AIC of 3675. SIgnificant coeff: Over15Wkts, Over15Runs,
  # Over14 wkts
  runsInn1EOIAtOver15Model4 = lm(Inn1EOIRuns ~ Over15Runs + Over15Wkts, 
                                 data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver15Model4)
  AIC(runsInn1EOIAtOver15Model4)
  # This again had similar adjusted R2 of 0.79 and an AIC of 3677. And all coefficients as significant
  # Try adding teams and 14 ovrRuns to see if it improves the model fit:
  runsInn1EOIAtOver15Model5 = lm(Inn1EOIRuns ~ Over15Runs + Over15Wkts + Over14Runs + 
                                   BatFirst * BatSecond, 
                                 data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver15Model5)
  AIC(runsInn1EOIAtOver15Model5)
  # R2 = 0.81; AIC = 3721. This did not improve the model fit. Add venueCity back to see if it improves the 2nd model: 
  runsInn1EOIAtOver15Model6 = lm(Inn1EOIRuns ~ Over15Runs + Over15Wkts + Over14Wkts + BatFirst * BatSecond +
                                 venueCity, 
                                 data = na.omit(subset(matSummTrng)))
  summary (runsInn1EOIAtOver15Model6)
  AIC(runsInn1EOIAtOver15Model6)
  #  R2 = .81; AIC = 3726. So stick with the 2nd model as the best predictor after 15 overs of a match
  # let's rename the best fit model after 15 overs to have a more generic name:
  runsInn1EOIAtOver15Model <- runsInn1EOIAtOver15Model2
  return (runsInn1EOIAtOver15Model)  # end of Over15modeling_fun
}

## Now create functions for choosing the best fit model for the 2nd innings. This is also when we will
## try to predict the match winner at the end of 6th, 10th and 15th overs of this innings. 

Over26modeling_fun <- function() {
  # First, check the correlation among different independent variables:
  matSumm_cor1 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                                 select = c( "Inn2EOIRuns", "Inn1EOIRuns",
                                    "Over22Runs", "Over22Wkts","Over23Runs", "Over23Wkts", 
                                    "Over24Runs", "Over24Wkts", "Over25Runs", "Over25Wkts",
                                    "Over26Runs", "Over26Wkts", 
                                    "TeamBattingFirstWon", 
                                    "interactionVenueBatTeam" , "interactionCurrTeams")))
  cor(matSumm_cor1)
  # As expected, correlation with EOI Runs is higher as the number of overs played increases; 
  # 1st Innings score (Inn1EOIRuns) is sigificant
  # Runs scored have a higher correlation with EOI Runs than Wickets lost, and Run Rate has the same correlation as Runs Scored
  
  runsInn1EOIAtOver26Model1 = lm(Inn2EOIRuns ~ Over26Runs + Over25Runs + Over26Wkts + Over25Wkts + 
                                  Inn1EOIRuns + BatFirst* BatSecond + venueCity + toss +
                                   TeamBattingFirstWon, 
                                data = matSummTrng)
  summary (runsInn1EOIAtOver26Model1)
  AIC(runsInn1EOIAtOver26Model1)
  # R2 = 0.62, AIC = 3909
  
  # Shows significant coeff as Inn1EOIRuns, TeamBattingFirstWon, Over26Runs, Over26Wkts, 
  # and with 1 asterisk: Over25Wkts
  # Try other models for bist fit. This may work best:
  runsInn1EOIAtOver26Model2 = lm(Inn2EOIRuns ~ Over26Runs + Over26Wkts +  
                                   Inn1EOIRuns + TeamBattingFirstWon, 
                                 data = matSummTrng)
  summary (runsInn1EOIAtOver26Model2)
  AIC(runsInn1EOIAtOver26Model2)
  ## R2 = 0.62; AIC = 3803
} # End of Over26modeling_fun

## Now create a model to predict the winner of the macth
Over26winner_fun <- function() {
  winnerAtOver26Model1 = lm (TeamBattingFirstWon ~ Inn1EOIRuns + Over26Runs + Over26Wkts,
                             data = matSummTrng)
  summary (winnerAtOver26Model1)
  AIC(winnerAtOver26Model1)
  # R2 = 0.34, AIC = 466. All coeff significant with 3 asterisks: Inn1EOIRuns, Over26Runs and Over 26Wkts
  
  return(winnerAtOver26Model1)
} # End of Over26winner_fun



## Call the functions for linear model for predicting EOI runs at the end of various overs

runsInn1EOIAtOver6Model <- Over6modeling_fun()
runsInn1EOIAtOver10Model <- Over10modeling_fun()
runsInn1EOIAtOver15Model <- Over15modeling_fun()
runsInn2EOIAtOver26Model <- Over26modeling_fun()

winnerInn2AtOver26Model <- Over26winner_fun()



