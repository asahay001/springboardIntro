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
innSumm <- read.csv(file= "wrangled_inningsSummaryDataIPL.csv", header = TRUE, sep = ",")

# Since match data available is for seasons 2008 through 2017, let's use data till season 2014 as
# training data (7 seasons), and then test the EOI score prediction model on seasons 2015, 
# then 2016, and finally 2017. 

matDetTrng <- filter(matDet, Season < 2015)
innSummTrng <- filter(innSumm, Season < 2015)

# First, check the correlation among different independent variables:
innSumm_cor1 <- na.omit(subset(innSummTrng, na.rm = TRUE, 
                              select = c("EOIRuns", "Over4Runs", "Over4Wkts", "Over6Runs",
                                         "Runs50InOver", "Wkts3InOver", 
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(innSumm_cor1)
# This shows 50th run scored is more promising than the runs scored at the ednd of powerplay (6 overs)
# So let's try some more similar variables:

innSumm_cor2 <- na.omit(subset(innSummTrng, na.rm = TRUE, 
                              select = c("EOIRuns", "Runs30InOver",  
                                         "Runs50InOver", "Runs75InOver", "Runs100InOver",
                                         "Wkts3InOver", "Wkts4InOver", "Wkts5InOver",
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(innSumm_cor2)
# This resut shows that with 30th 50th, 75th, 100th run scored, the corelation with EOI score gets 
# significantly higher, but ony a bit higher as we move from 3rd wicket down to 4th wicket down to 5th
# Since runs play a more significant role in EOI score, let's create a prediction model with various runs variables:

runsEOIModel1 = lm(EOIRuns ~ Runs30InOver + Runs50InOver + Runs75InOver + 
                  Over4Runs + Over6Runs + Over8Runs + Over10Runs, data = innSummTrng)
summary (runsEOIModel1)
AIC(runsEOIModel1)
runsEOIModel2 = lm(EOIRuns ~ Runs30InOver + Runs75InOver + 
                    Over4Runs + Over10Runs, data = innSummTrng)
summary (runsEOIModel2)
AIC(runsEOIModel2)
# Adjusted R2 for both the above models is 0.40; since 2nd case has fewer variables, this model is 
# better for prediction than the 1st one

# Finally add more variables and take the data from deep into the batting innings to be able to
# predict the EOI score for a new match:
runsEOIModel3 = lm(EOIRuns ~ Runs30InOver + Runs50InOver + Runs75InOver + Runs100InOver +
                     Over4Runs + Over6Runs + Over8Runs + Over10Runs + Over12Runs + Over15Runs,
                     data = innSummTrng)
summary (runsEOIModel3)
AIC(runsEOIModel3)
# This model shows adjusted R2 of 0.641 and an AIC of 7138. But let's drop some independent variables, one at a time,
# and see the effect on adj R2 and AIC. Here is the final model: 
runsEOIModel4 = lm(EOIRuns ~ Runs30InOver + Over4Runs + Over15Runs + Innings_No, data = innSummTrng)
summary (runsEOIModel4)
AIC(runsEOIModel4)
confint(runsEOIModel4)
# This model has the highest Adjusted R2 of 0.71, but its AIC at 7334 is not as low (7133; adj R2 of 0.64) as with
# other independent variables added: Runs 75InOver and Runs100InOver. Actually adding
# Innings_No has helped imprve the model. All the independent variables are
# significant with 2 or 3 asterisks in the Coefficients summary

# Let's try this latest model with test data for season 2015:
thisSeason <- 2015
innSummTest2015 <- filter(innSumm, Season == thisSeason)

predictEOIRuns2015 = predict (runsEOIModel4, newdata = innSummTest2015)

#Check the prediction:
#confint(runsEOIModel4)
#summary(runsEOIModel4)  # R2 is 0.71
#AIC(runsEOIModel4)      # AIC is 7334
pred_EOIRuns <- round(as.numeric(predictEOIRuns2015), digits = 0)  # Predicted based on linear model
act_EOIRuns <-innSummTest2015$EOIRuns  # Actual EOI runs from real match innings data
TotNumOfInnings <- length(act_EOIRuns)
Innings_No <- seq(1,TotNumOfInnings,1)
diff_predActEOIRuns <- round(pred_EOIRuns - act_EOIRuns, digits = 0)
predictedScore2015 <- data.frame(pred_EOIRuns, act_EOIRuns, Innings_No, diff_predActEOIRuns)

# Verify Model prediction against test data: 
ErrorSeasonRuns <- sum(abs(diff_predActEOIRuns), na.rm = TRUE)  # Running this command shows a total error of 1228 runs across 109 innings.
TotalSeasonRuns <- sum (act_EOIRuns)  # This comes out to be 17,597 runs. (1228/17597 = 6.98%)
PercentageErrorAgainstTotRuns <- 100* sum(abs(diff_predActEOIRuns), na.rm = TRUE) / sum (act_EOIRuns) # = 6.98%
# Compute the Root Mean Square Error of prediction against Test data
RMSE <- sqrt((sum((pred_EOIRuns - act_EOIRuns), na.rm = TRUE) ^ 2) / TotNumOfInnings)  # = 54.5
# Check R2 of the prediction: 1 - (SSE/SST); where SST is the baseline model of average EOI for training data
SSE <- sum(diff_predActEOIRuns, na.rm = TRUE) ^ 2
SST <- sum((act_EOIRuns - mean(innSummTrng$EOIRuns, na.rm = TRUE)), na.rm = TRUE) ^ 2
R2_predicted <- 1 - (SSE/SST)  # = 0.70 which is reasonable accuracy
# save data for comparison with other seasons' test data
predictionSummary_df <- data.frame(thisSeason, R2_predicted, RMSE, SSE, SST, ErrorSeasonRuns, 
                                TotalSeasonRuns, PercentageErrorAgainstTotRuns, TotNumOfInnings )

ggplot(data = predictedScore2015, aes (x = Innings_No, y = diff_predActEOIRuns )) +
        geom_point (na.rm = TRUE) +
         geom_smooth (method = "lm", se=FALSE, na.rm = TRUE)

#Now test the model against 2016 season data:
thisSeason <- 2016
innSummTest2016 <- filter(innSumm, Season == thisSeason)

predictEOIRuns2016 = predict (runsEOIModel4, newdata = innSummTest2016)

# check the prediction: 
pred_EOIRuns <- round(as.numeric(predictEOIRuns2016), digits = 0)
act_EOIRuns <-innSummTest2016$EOIRuns
TotNumOfInnings <- length(act_EOIRuns)
Innings_No <- seq(1,length(act_EOIRuns),1)  # There are 117 innings in 2016
diff_predActEOIRuns <- round(pred_EOIRuns - act_EOIRuns, digits = 0)
predictedScore2016 <- data.frame(pred_EOIRuns, act_EOIRuns, Innings_No, diff_predActEOIRuns)
# Verify Model prediction against test data:
ErrorSeasonRuns <- sum(abs(diff_predActEOIRuns), na.rm = TRUE)  # This comes out to be error of 1357 runs total across 117 innings
TotalSeasonRuns <- sum (act_EOIRuns)  # This comes out to be 18,622 runs. 
PercentageErrorAgainstTotRuns <- 100* sum(abs(diff_predActEOIRuns), na.rm = TRUE) / sum (act_EOIRuns) # = 7.29%
# Compute the Root Mean Square Error of prediction against Test data
RMSE = sqrt((sum((pred_EOIRuns - act_EOIRuns), na.rm = TRUE) ^ 2) / TotNumOfInnings)  # = 7.77
# Check R2 of the prediction: 1 - (SSE/SST); where SST is the baseline model of average EOI for training data
SSE <- sum(diff_predActEOIRuns, na.rm = TRUE) ^ 2
SST <- sum((act_EOIRuns - mean(innSummTrng$EOIRuns, na.rm = TRUE)), na.rm = TRUE) ^ 2
R2_predicted <- 1 - (SSE/SST)  # = 0.70 which is reasonable accuracy
predictionSummary_df <- rbind(predictionSummary_df, c(thisSeason, R2_predicted, RMSE, SSE, SST, 
                              ErrorSeasonRuns, TotalSeasonRuns, PercentageErrorAgainstTotRuns, 
                              TotNumOfInnings))

ggplot(data = predictedScore2016, aes (x = Innings_No, y = diff_predActEOIRuns )) +
  geom_point (na.rm = TRUE) +
  geom_smooth (method = "lm", se=FALSE, na.rm = TRUE)  

#Now test the model against 2017 season data:
thisSeason <- 2017
innSummTest2017 <- filter(innSumm, Season == thisSeason)

predictEOIRuns2017 = predict (runsEOIModel4, newdata = innSummTest2017)

# check the prediction: 
pred_EOIRuns <- round(as.numeric(predictEOIRuns2017), digits = 0) 
act_EOIRuns <-innSummTest2017$EOIRuns
TotNumOfInnings <- length(act_EOIRuns)
Innings_No <- seq(1,length(act_EOIRuns),1)
diff_predActEOIRuns <- round(pred_EOIRuns - act_EOIRuns, digits = 0)
predictedScore2017 <- data.frame(pred_EOIRuns, act_EOIRuns, Innings_No, diff_predActEOIRuns)
# Verify Model prediction against test data:
ErrorSeasonRuns <- sum(abs(diff_predActEOIRuns), na.rm = TRUE)  
TotalSeasonRuns <- sum (act_EOIRuns)  
PercentageErrorAgainstTotRuns <- 100* sum(abs(diff_predActEOIRuns), na.rm = TRUE) / sum (act_EOIRuns) # = 7.29%
# Compute the Root Mean Square Error of prediction against Test data
RMSE = sqrt((sum((pred_EOIRuns - act_EOIRuns), na.rm = TRUE) ^ 2) / TotNumOfInnings)
# Check R2 of the prediction: 1 - (SSE/SST); where SST is the baseline model of average EOI for training data
SSE <- sum(diff_predActEOIRuns, na.rm = TRUE) ^ 2
SST <- sum((act_EOIRuns - mean(innSummTrng$EOIRuns, na.rm = TRUE)), na.rm = TRUE) ^ 2
R2_predicted <- 1 - (SSE/SST)  
predictionSummary_df <- rbind(predictionSummary_df, c(thisSeason, R2_predicted, RMSE, SSE, SST, 
                                                      ErrorSeasonRuns, TotalSeasonRuns, PercentageErrorAgainstTotRuns, 
                                                      TotNumOfInnings))

ggplot(data = predictedScore2017, aes (x = Innings_No, y = diff_predActEOIRuns )) +
  geom_point (na.rm = TRUE) +
  geom_smooth (method = "lm", se=FALSE, na.rm = TRUE) 


# Now work with 1 team's batting prformance and create a model and then test it: CSK
# Abbreviating in my comments throughout: Chennai Super Kings = CSK 
# Try to see how CSK has fared against different opponents in terms of run rate in each match, ball-by-ball

matDet_csk <- filter (matDet, TeamNameBat == "Chennai Super Kings" & Season < 2015)
innSumm_csk <- filter (innSumm, TeamNameBat == "Chennai Super Kings" & Season < 2015)

# First, check the correlation among different independent variables:
innSumm_cor <- na.omit(subset(innSumm_csk, na.rm = TRUE, 
                              select = c("EOIRuns", "Over4Runs", "Over4Wkts", "Over6Runs",
                                         "Runs50InOver", "Wkts3InOver", 
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(innSumm_cor)
#This shows that the milestone of scoring 50 runs has a better corelation (0.58) with EOI score than 
# Runs scored at the end of the 6th over (0.54). Similarly, the milestone of 3 wickets lost has a higher 
# corelation with EOI score (0.41) than the number of wickets lost in the 1st 4 overs (-0.29)
