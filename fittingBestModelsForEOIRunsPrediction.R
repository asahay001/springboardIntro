################### CREATE FUNCTIONS FOR CREATING TRAINING AND TEST DATA ########################
################### AND THEN CREATE PREDICTION MODEL FUNCTIONS AT VARIOUS STAGES OF EACH MATCH ###

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
# install.packages ("leaps")  # leaps function is ot get a good model fit

    # Set files dirctory from where to read data (csv files). 
filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
    #setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")
library("leaps")


    # Function to create data set for prediction model: either the Traiing Data or the Test Data
createMatchDataSlice_fun <- function (dataFrame = matSummDet, minSeason, maxSeason, matchId = NULL) {
  if (is.null(matchId)) {
    matSumm_df <- filter(dataFrame, between (Season,  minSeason, maxSeason))  # Slice of data for best fit model
  } else {
        matSumm_df <- filter(dataFrame, between (Season,  minSeason, maxSeason) & Match_id == matchId)
    }
  return (matSumm_df)
}

    # Let's start the EOI runs prediction at the end of 6th over (then 10th and finally 15th overs for Innings 1)


modelAICbestFitEOI_fn <- function (over, matSummTrngInit) {
  DepVar6 <- "interactionCurrTeams+interactionVenueBatTeam+Over1Runs+Over1Wkts+Over2Runs+Over2Wkts+Over3Runs+Over3Wkts+Over4Runs+Over4Wkts+Over5Runs+Over5Wkts+Over6Runs+Over6Wkts"
  incrementalDepVar10 <- "Over7Runs+Over7Wkts+Over8Runs+Over8Wkts+Over9Runs+Over9Wkts+Over10Runs+Over10Wkts"
  incrementalDepVar15 <- "Over11Runs+Over11Wkts+Over12Runs+Over12Wkts+Over13Runs+Over13Wkts+Over14Runs+Over14Wkts+Over15Runs+Over15Wkts"
  
  incrementalDepVar26 <- "Inn1EOIRuns+Over21Runs+Over21Wkts+Over22Runs+Over22Wkts+Over23Runs+Over23Wkts+Over24Runs+Over24Wkts+Over25Runs+Over25Wkts+Over26Runs+Over26Wkts"
  incrementalDepVar30 <- "Over27Runs+Over27Wkts+Over28Runs+Over28Wkts+Over29Runs+Over29Wkts+Over30Runs+Over30Wkts"
  incrementalDepVar35 <- "Over31Runs+Over31Wkts+Over32Runs+Over32Wkts+Over33Runs+Over33Wkts+Over34Runs+Over34Wkts+Over35Runs+Over35Wkts"
  
  IndVar <- ifelse(over <= 20, "Inn1EOIRuns", "Inn2EOIRuns")
  
  fullDepVar <- case_when(
    over ==  6 ~ DepVar6,
    over == 10 ~ paste (incrementalDepVar10, "+", DepVar6, sep = " "), 
    over == 15 ~ paste (incrementalDepVar15, "+", incrementalDepVar10, 
                        "+", DepVar6, sep = " "),
    over == 26 ~ paste (incrementalDepVar26, "+", incrementalDepVar15, "+", 
                        incrementalDepVar10, "+", DepVar6, sep = " "),
    over == 30 ~ paste (incrementalDepVar30, "+", incrementalDepVar26, "+",
                         incrementalDepVar15, "+", incrementalDepVar10, "+", DepVar6, sep = " "),
    over == 35 ~ paste (incrementalDepVar35, "+", incrementalDepVar30, "+", 
                        incrementalDepVar26, "+", incrementalDepVar15, "+", 
                        incrementalDepVar10, "+", DepVar6, sep = " ")
  )
  formula_base <-paste (IndVar, "~1")
  formula_full <- paste (IndVar, "~", fullDepVar)
  null=lm(formula_base, data=matSummTrngInit)
  full=lm(formula_full, data=matSummTrngInit)
  stepRes <- step(null, scope = list( upper=full, lower=~1 ), direction = "both", trace=FALSE)
  lmFormulaLowestAIC <- as.formula(stepRes$call)
  modelNamelowestAIC <- lm(lmFormulaLowestAIC, data = matSummTrngInit)
        ## stepRes$anova[6]  # stores the AIC values
        ### Using leaps() to dtermine the predictors for the highest adjusted R square:
        #leapsResOv6 = leaps(x= matSummTrngInit[,19:30], y = matSummTrngInit[,"Inn1EOIRuns"],
                            # names = names(matSummTrngInit)[19:30], method = "adjr2")
        #leapsResOv6$which[which.max(leapsResOv6$adjr2),] # shows the relevant predictors with the highest adjusted R square
  return (modelNamelowestAIC)  # will be used to predict EOI Runs
}  # end of create function: modelAICbestFitEOI_fn


    ## Now create a model to predict the WINNER of the match
Over26winner_fun <- function(matSummTrng = matSummTrngInit) {
  winnerAtOver26Model1 = lm (TeamBattingFirstWon ~ Inn1EOIRuns + Over26Runs + Over26Wkts,
                             data = matSummTrng)
  summary (winnerAtOver26Model1)
  AIC(winnerAtOver26Model1)
      # R2 = 0.34, AIC = 466. All coeff significant with 3 asterisks: Inn1EOIRuns, Over26Runs and Over 26Wkts
  return(winnerAtOver26Model1)
}   # End of Over26winner_fun

    ## Now create a model to predict the winner of the match at end of over 30
Over30winner_fun <- function(matSummTrng = matSummTrngInit) {
  winnerAtOver30Model1 = lm (TeamBattingFirstWon ~ Inn1EOIRuns + Over30Runs +  Over29Wkts,
                             data = matSummTrng)
  summary (winnerAtOver30Model1)
  AIC(winnerAtOver30Model1)
  # R2 = 0.39, AIC = 427. All coeff significant with 3 asterisks: Inn1EOIRuns, Over30Runs and Over 29Wkts
  
  return(winnerAtOver30Model1)
}   # End of Over30winner_fun

    ## Now create a model to predict the winner of the match at end of over 35
Over35winner_fun <- function(matSummTrng = matSummTrngInit) {
  winnerAtOver35Model1 = lm (TeamBattingFirstWon ~ Inn1EOIRuns + Over35Runs +  Over34Runs + Over35Wkts,
                             data = matSummTrng)
  summary (winnerAtOver35Model1)
  AIC(winnerAtOver35Model1)
      # R2 = 0.51, AIC = 337. All 4 coeff significant with 3 asterisks
  
  return(winnerAtOver35Model1)
}   # End of Over35winner_fun

##################### All Functions Created Above ##########################################