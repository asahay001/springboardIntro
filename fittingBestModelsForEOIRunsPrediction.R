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

##  remove all variables:
## rm(list=ls())          # remove all objects: functions and variables
## rm(list=lsf.str())     # remove functions but not variables
## rm(list = setdiff(ls(), lsf.str()))  # removes all objects except functions
  ## It uses setdiff to find the subset of objects in the global environment 
    ## (as returned by ls()) that don't have mode function (as returned by lsf.str())

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


modelAICbestFitEOI_fn <- function (over, matSummTrng) {
  
  prevWinsDepVar      <- "BatFirstWinsInLast5Mat"
  incrementalDepVar6  <- "Over3Runs+Over3Wkts+Over5Runs+Over5Wkts+Over6Runs+Over6Wkts"
  incrementalDepVar10 <- "Over9Runs+Over9Wkts+Over10Runs+Over10Wkts"
  incrementalDepVar15 <- "Over13Runs+Over13Wkts+Over14Runs+Over14Wkts+Over15Runs+Over15Wkts"
  
  incrementalDepVar26 <- "Inn1EOIRuns+Over23Runs+Over23Wkts+Over25Runs+Over25Wkts+Over26Runs+Over26Wkts"
  incrementalDepVar30 <- "Over29Runs+Over29Wkts+Over30Runs+Over30Wkts"
  incrementalDepVar35 <- "Over33Runs+Over33Wkts+Over34Runs+Over34Wkts+Over35Runs+Over35Wkts"
  
  IndVar <- ifelse(over <= 20, "Inn1EOIRuns", "Inn2EOIRuns")
  
  fullDepVar <- case_when(
    over ==  6 ~ paste (incrementalDepVar6, "+",  prevWinsDepVar, sep = " "),
    over == 10 ~ paste (incrementalDepVar10, "+", incrementalDepVar6, 
                        "+",  prevWinsDepVar, sep = " "), 
    over == 15 ~ paste (incrementalDepVar15, "+", incrementalDepVar10, 
                        "+", incrementalDepVar6, "+",  prevWinsDepVar, sep = " "),
    over == 26 ~ paste (incrementalDepVar26, "+", incrementalDepVar15, "+", 
                        incrementalDepVar10, "+", incrementalDepVar6, 
                        "+",  prevWinsDepVar, sep = " "),
    over == 30 ~ paste (incrementalDepVar30, "+", incrementalDepVar26, "+",
                         incrementalDepVar15, "+", incrementalDepVar10, "+", 
                        incrementalDepVar6, "+",  prevWinsDepVar, sep = " "),
    over == 35 ~ paste (incrementalDepVar35, "+", incrementalDepVar30, "+", 
                        incrementalDepVar26, "+", incrementalDepVar15, "+", 
                        incrementalDepVar10, "+", incrementalDepVar6, 
                        "+",  prevWinsDepVar, sep = " ")
  )
  formula_base <-paste (IndVar, "~1")
  formula_full <- paste (IndVar, "~", fullDepVar)
  null=lm(formula_base, data = matSummTrng )
  full=lm(formula_full, data = matSummTrng )
  stepRes <- step(null, scope = list( upper=full, lower=~1 ), direction = "both", trace=FALSE)
  
  lmFormulaLowestAIC <- as.formula(stepRes$call)
  modelNamelowestAIC <- lm(lmFormulaLowestAIC, data = matSummTrng)
        ## stepRes$anova[6]  # stores the AIC values
       
  return (modelNamelowestAIC)  # will be used to predict EOI Runs
}  # end of create function: modelAICbestFitEOI_fn


    ## Now create a model to predict the WINNER of the match at 
    ## the start of the match, and then at the end of Overs 20, 26, 30 and 35

predictWinner_fn <- function (over = 0, matSummTrng) {
  
  #prevWinsDepVar <- "BatFirstWonLastMat+BatSecondWonLastMat+BatFirstWinsInLast3Mat+BatSecondWinsInLast3Mat+BatFirstWinsInLast5Mat+BatSecondWinsInLast5Mat"                                              
  #IndVar <-    "TeamBattingFirstWon"   ## check if the team that batted 1st won 
  #commonVar <- "Inn1EOIOvers+Inn1EOIWkts+Inn1EOIRuns+toss+venueCity+BatFirst+BatSecond+interactionCurrTeams+interactionVenueBatTeam"
  #overVar0  <- "toss+BatFirst+BatSecond+venueCity+interactionCurrTeams+interactionVenueBatTeam+Season"
  #overVar20 <- "Over6Runs+Over6Wkts+Over9Runs+Over9Wkts+Over10Runs+Over10Wkts+Over12Runs+Over12Wkts+Over15Runs+Over15Wkts+Over18Runs+Over18Wkts+Over19Runs+Over19Wkts"
  #overVar26 <- "Over26Runs+Over26Wkts+Over25Runs+Over25Wkts+Over24Runs+Over24Wkts+Over23Runs+Over23Wkts"
  #overVar30 <- "Over30Runs+Over30Wkts+Over29Runs+Over29Wkts+Over28Runs+Over28Wkts+Over27Runs+Over27Wkts"
  #overVar35 <- "Over35Runs+Over35Wkts+Over34Runs+Over34Wkts+Over33Runs+Over33Wkts+Over32Runs+Over32Wkts+Over31Runs+Over31Wkts"
  
  prevWinsDepVar <- "BatFirstWonLastMat+BatFirstWinsInLast3Mat+BatFirstWinsInLast5Mat"                                              
  IndVar <-    "TeamBattingFirstWon"   ## check if the team that batted 1st won 
  commonVar <- "Inn1EOIRuns+toss+venueCity+BatFirst+BatSecond"
  overVar0  <- "toss+BatFirst+BatSecond+venueCity"
  overVar20 <- "Over6Runs+Over6Wkts+Over10Runs+Over15Runs+Over15Wkts+Inn1EOIRuns+toss+venueCity+BatFirst+BatSecond"
  overVar26 <- "Over26Runs+Over26Wkts+Over25Runs+Over25Wkts+Over23Runs+BatSecond"
  overVar30 <- "Over30Runs+Over30Wkts+Over28Runs+Over28Wkts+Over27Runs"
  overVar35 <- "Over35Runs+Over35Wkts+Over34Runs+Over32Runs+Over32Wkts+Over31Runs"
  
  over0DepVar  <- paste(prevWinsDepVar, "+", overVar0)  # winner prediction right after toss before a ball is bowled
  #over20DepVar <- paste(overVar20, "+", prevWinsDepVar, "+",  commonVar)
  #over26DepVar <- paste(overVar26, "+", "BatFirstWinsInLast5Mat", "+", "Inn1EOIRuns")
  #over30DepVar <- paste(overVar30, "+", "BatFirstWinsInLast5Mat", "+", "Inn1EOIRuns")
  over35DepVar <- paste(overVar35, "+", "BatFirstWinsInLast5Mat", "+", "Inn1EOIRuns")
  
  DepVar <- case_when(
    over ==  0 ~ over0DepVar, 
    over == 20 ~ overVar20,
    over == 26 ~ overVar26,
    over == 30 ~ overVar30,
    over == 35 ~ over35DepVar
  )
  formula_full <- paste (IndVar, "~", DepVar, sep = " ")
  formula_base <- paste (IndVar, "~1")
  null = lm(formula_base, data = matSummTrng)
  full = lm(formula_full, data=matSummTrng)
  stepRes <- step(null, scope = list( upper=full, lower = ~1 ), direction = "both", trace=FALSE)
  lmFormulaLowestAIC <- as.formula(stepRes$call)
  modelNamelowestAIC <- lm(lmFormulaLowestAIC, data = matSummTrng)
        #print(stepRes$anova[6])
  return(modelNamelowestAIC)
}   # End of create predictWinner_fn

# Determine distinct set of opponents in all matches of a given dataset:
detemineOpponentCombinations_fn <- function(matSummTrng){
  opponents_df <- matSummTrng %>% ungroup() %>%
    select (BatFirst, BatSecond) %>% distinct() %>% arrange (BatFirst) %>%
    mutate(set1 = ifelse((as.character(BatFirst) < as.character(BatSecond)),
                         paste(as.character(BatFirst), as.character(BatSecond)),
                         paste(as.character(BatSecond), as.character(BatFirst)) ),
           set2 = paste(as.character(BatFirst), as.character(BatSecond))) %>%
    arrange(set1)  # Teams can be in both BatFirst and BatSecond in different matches
  opponents_df1 <- opponents_df  # Copy of the data set needed for self-join:
  opponents_df2 <- semi_join(opponents_df, opponents_df1, by= c("set2" = "set1")) 
                    #opponents_df3 <- opponents_df2 %>% distinct(set1)
  opponents_df4 <- anti_join(opponents_df1, opponents_df2, by = c("set1" = "set1"))
  opponents_df  <- rbind(opponents_df2, opponents_df4) %>% arrange(BatFirst, BatSecond) %>%
    select (team1 = BatFirst, team2 = BatSecond)
  
  return (opponents_df)  
}    # end of create function detemineOpponentCombinations_fn

actualWinnerBetween2Teams_fn <- function (occurrence = 1, matSummTrng, team1, team2) {  
                    # determine ACTUAL winner count, either can be Team1 in different matches
  matSummTrng<- matSummTrng %>% filter ((BatFirst == team1 & BatSecond == team2) |
                                BatFirst == team2 & BatSecond == team1 ) %>%
    arrange(desc(Season), desc(Match_id), BatFirst)    #%>% slice(1:occurrence) %>%
  matSummTrng <-  matSummTrng[1:occurrence,]
        # we need the winners from the most recent matches, so we sorted descending
  winCount <-  matSummTrng %>% group_by(winner) %>% tally()  # occurrence is the number of matches we need to report on, starting from most recent and going back
  ifelse(winCount[1,1] == as.character(team1), team1Win <- winCount[1,2], 
         team1Win <- winCount[2,2])
  ifelse(winCount[1,1] == as.character(team2), team2Win <- winCount[1,2], 
         team2Win <- winCount[2,2])
  ifelse (is.na(team1Win$n), team1Win$n <-0, team1Win$n)
  ifelse (is.na(team2Win$n), team2Win$n <-0, team2Win$n)
  return(c(team1Win$n, team2Win$n))
} # end of create function actualWinnerBetween2Teams_fn


updateRecentWinsInDataSet_fn <- function(matSummTrng) {
  matchSets <- detemineOpponentCombinations_fn (matSummTrng) 
  for (row in 1:nrow(matchSets)) {
    tm1 <- matchSets$team1[row]
    tm2 <- matchSets$team2[row]
    winRecLast  <- actualWinnerBetween2Teams_fn (occurrence = 1, matSummTrng, tm1, tm2)
    winRecLast3 <- actualWinnerBetween2Teams_fn (occurrence = 3, matSummTrng, tm1, tm2) 
    winRecLast5 <- actualWinnerBetween2Teams_fn (occurrence = 5, matSummTrng, tm1, tm2) 
                            
    matSummTrng <- matSummTrng %>% 
      mutate (BatFirstWonLastMat      = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                          winRecLast[1], BatFirstWonLastMat),
              BatSecondWonLastMat     = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                          winRecLast[2], BatSecondWonLastMat),
              BatFirstWinsInLast3Mat  = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                          winRecLast3[1], BatFirstWinsInLast3Mat),
              BatSecondWinsInLast3Mat = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                               winRecLast3[2], BatSecondWinsInLast3Mat),
              BatFirstWinsInLast5Mat  = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                              winRecLast5[1], BatFirstWinsInLast5Mat),
              BatSecondWinsInLast5Mat = ifelse(BatFirst == tm1 & BatSecond == tm2, 
                                              winRecLast5[2], BatSecondWinsInLast5Mat)
              ) %>%
      mutate (BatFirstWonLastMat      = ifelse(BatFirst == tm2 & BatSecond == tm1, 
                                        winRecLast[2], BatFirstWonLastMat),
              BatSecondWonLastMat     = ifelse(BatFirst == tm2 & BatSecond == tm1,
                                         winRecLast[1], BatSecondWonLastMat),
              BatFirstWinsInLast3Mat  = ifelse(BatFirst == tm2 & BatSecond == tm1, 
                                               winRecLast3[2], BatFirstWinsInLast3Mat),
              BatSecondWinsInLast3Mat = ifelse(BatFirst == tm2 & BatSecond == tm1, 
                                               winRecLast3[1], BatSecondWinsInLast3Mat),
              BatFirstWinsInLast5Mat  = ifelse(BatFirst == tm2 & BatSecond == tm1, 
                                               winRecLast5[2], BatFirstWinsInLast5Mat),
              BatSecondWinsInLast5Mat = ifelse(BatFirst == tm2 & BatSecond == tm1, 
                                               winRecLast5[1], BatSecondWinsInLast5Mat)
    )
                #paste (winRecLast[1], tm1)
                #print (winRecLast[2], tm2)
  }    # end of for loop with matchSets
  return(matSummTrng)
}   # end of create function updateRecentWinsInDataSet_fn



##################### All Functions Created Above ##########################################

