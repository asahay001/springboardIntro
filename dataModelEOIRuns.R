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

# Since match data available is for seasons 2008 through 2017, let's use data till season 2014 as
# training data (7 seasons), and then test the EOI score prediction model on seasons 2015, 
# then 2016, and finally 2017. 

matDetTrng <- filter(matDet, Season < 2015)
matSummTrng <- filter(matSumm, Season < 2015)

# First, check the correlation among different independent variables:
matSumm_cor1 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                              select = c("EOIRuns", "Over4Runs", "Over4Wkts", "Over6Runs",
                                         "Runs50InOver", "Wkts3InOver", 
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(matSumm_cor1)
# This shows 50th run scored is more promising than the runs scored at the ednd of powerplay (6 overs)
# So let's try some more similar variables:

matSumm_cor2 <- na.omit(subset(matSummTrng, na.rm = TRUE, 
                              select = c("EOIRuns", "Runs30InOver",  
                                         "Runs50InOver", "Runs75InOver", "Runs100InOver",
                                         "Wkts3InOver", "Wkts4InOver", "Wkts5InOver",
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(matSumm_cor2)
# This resut shows that with 30th 50th, 75th, 100th run scored, the corelation with EOI score gets 
# significantly higher, but ony a bit higher as we move from 3rd wicket down to 4th wicket down to 5th
# Since runs play a more significant role in EOI score, lt create a prediction model with various runs variables:

runsEOIModel1 = lm(EOIRuns ~ Runs30InOver + Runs50InOver + Runs75InOver + 
                  Over4Runs + Over6Runs + Over8Runs + Over10Runs, data = matSummTrng)
summary (runsEOIModel1)
AIC(runsEOIModel1)
runsEOIModel2 = lm(EOIRuns ~ Runs30InOver + Runs75InOver + 
                    Over4Runs + Over10Runs, data = matSummTrng)
summary (runsEOIModel2)
AIC(runsEOIModel2)
# Adjusted R2 for both the above models is 0.40; since 2nd case has fewer variables, this model is 
# better for prediction than the 1st one

# Finally add more variables and take the data from deep into the batting innings to be able to
# predict the EOI score for a new match:
runsEOIModel3 = lm(EOIRuns ~ Runs30InOver + Runs50InOver + Runs75InOver + Runs100InOver +
                     Over4Runs + Over6Runs + Over8Runs + Over10Runs + Over12Runs + Over15Runs,
                     data = matSummTrng)
summary (runsEOIModel3)
AIC(runsEOIModel3)
# This model shows R2 of 0.641 and an AIC of 7138. But let's drop some independent variables, one at a time,
# and see the effect on R2 and AIC. Here is the final model: 
runsEOIModel4 = lm(EOIRuns ~ Runs30InOver + Over4Runs + Over15Runs + Innings_No, data = matSummTrng)
summary (runsEOIModel4)
AIC(runsEOIModel4)
confint(runsEOIModel4)
# This model has the highest R2 of 0.71, but its AIC at 7334 is not as low (7133; R2 of 0.64) as with
# other independent variables added: Runs 75InOver and Runs100InOver. Actually adding
# Innings_No has helped imprve the model. All the independent variables are
# significant with 2 or 3 asterisks in the Coefficients summary

# Let's try this latest model with test data for season 2015:
matSummTest2015 <- filter(matSumm, Season == 2015)

predictEOIRuns2015 = predict (runsEOIModel4, newdata = matSummTest2015)

#Check the prediction:
#confint(runsEOIModel4)
#summary(runsEOIModel4)  # R2 is 0.71
#AIC(runsEOIModel4)      # AIC is 7334
pred_EOIRuns <- as.numeric(predictEOIRuns2015)
act_EOIRuns <-matSummTest2015$EOIRuns
match_seq <- seq(1,109,1)
diff_predActEOIRuns <- round(pred_EOIRuns - act_EOIRuns, digits = 0)
predictedScore <- data.frame(pred_EOIRuns, act_EOIRuns, match_seq, diff_predActEOIRuns)
ggplot(data = predictedScore, aes (x = match_seq, y = diff_predActEOIRuns )) +
        geom_point (na.rm = TRUE) +
         geom_smooth (method = "lm", se=FALSE, na.rm = TRUE)

# Now work with 1 team's batting prformance and create a model and then test it: CSK
# Abbreviating in my comments throughout: Chennai Super Kings = CSK 
# Try to see how CSK has fared against different opponents in terms of run rate in each match, ball-by-ball

matDet_csk <- filter (matDet, TeamNameBat == "Chennai Super Kings" & Season < 2015)
matSumm_csk <- filter (matSumm, TeamNameBat == "Chennai Super Kings" & Season < 2015)

# First, check the correlation among different independent variables:
matSumm_cor <- na.omit(subset(matSumm_csk, na.rm = TRUE, 
                              select = c("EOIRuns", "Over4Runs", "Over4Wkts", "Over6Runs",
                                         "Runs50InOver", "Wkts3InOver", 
                                         "interactionCurrTeams", "interactionVenueBatTeam")))
cor(matSumm_cor)
#This shows that the milestone of scoring 50 runs has a better corelation (0.58) with EOI score than 
# Runs scored at the end of the 6th over (0.54). Similarly, the milestone of 3 wickets lost has a higher 
# corelation with EOI score (0.41) than the number of wickets lost in the 1st 4 overs (-0.29)
