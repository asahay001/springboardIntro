####### ALL FUNCTIONS ARE CREATED IN FILE fittingBestModelForEOIRunsPrediction.R  ##############
#
#
####   THIS FILE IS THE DRIVER FILE FOR PREDICTING AND ANALYZING PREDICTIONS  ##########
#
#
#
## Here we will predict End of Innings (EOI) runs for Innings 1 at the end of 6th, 10th and 15th overs
## for these 20 overs max - a - side match. 
## And then for the team batting 2nd (Innings 2), try to predict their EOI score at the end of their
# 6th, 10th and 15th overs, which for the match are the 26th, 30th and 35th overs respectively
# And in the 2nd part, we will try to predict the match winner at the end of 26th, 30th and 35th overs 
## of each match, store the 3 predictions for each match, and compare it to the actual match result

# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")
# install.packages("caret")   
# install.packages('caret', dependencies = TRUE) # contains caret::confusionMatrix (tidy)

    # Set files dirctory from where to read data (csv files). 
filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
    #setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")
library("caret")

runType <- "FinalTest"  # Final Test is ONLY for 2018 Eliminator matches

#runType <- "Regular"    # For all regular training and test iterations


    # Read the match summary csv file previously prepared with scorePred_ver1.R
    # or for 2018 file with dataWrangling_2018IPLDataForTesting
matSummDet <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, sep = ",")

FirstTime <- TRUE  ## Variable showing if the processing has just started: will create 
                    # the result data frame. For subsequent processing, will append to this data frame

    # Since match data available is for seasons 2008 through 2017, let's use data from 2008 through season 2014 as
    # training data (7 seasons), and then test the EOI score prediction model on seasons 2015, 
    # then 2016, and finally 2017, one match at a time based on who plays who and where 

minMaxSeason <- summarise (matSummDet,  minSeasonInit = min(Season), maxSeasonData = max(Season))

      # The initial Training Data will be built on 70% of the seasons in the entire data set:
minSeasonInit <- as.numeric(minMaxSeason[1])  # converting from data frame to numeric
maxSeasonData <- as.numeric(minMaxSeason[2])
numberOfSeasons <- maxSeasonData - minSeasonInit + 1
SeventyPercentSeason <- 0.7 * numberOfSeasons
maxSeasonInit <- minSeasonInit + SeventyPercentSeason - 1
    

      # Testing: 
TestSeasonSlice <- maxSeasonInit + 1
MaxTestSeasonData <- maxSeasonData
if (runType == "FinalTest" ) { ## Read Test data
  matSummTestSeason <- read.csv(file= "wrangled_matchSummaryDataIPL2018only.csv", header = TRUE, sep = ",")
  TestSeasonSlice <- max(matSummTestSeason$Season)
  MaxTestSeasonData <- TestSeasonSlice  # 2018; just the one year
  maxSeasonInit <- maxSeasonData        # Use all of the 2008-2017 data for training the model
  FirstTime <- FALSE
  }
      # Training Data:
      ## First create the Training Data (adding successively each season on which Testing has been completed)
matSummTrngInit <- createMatchDataSlice_fun(minSeason = minSeasonInit, maxSeason = maxSeasonInit)

while (TestSeasonSlice <= MaxTestSeasonData) {  
              # Start testing model on matches in each season, one season at a time
  if (runType != "FinalTest") {
      matSummTestSeason <- createMatchDataSlice_fun(minSeason = TestSeasonSlice, 
                                              maxSeason = TestSeasonSlice)  # Test data for the model 
  }  # else matSummTestSeason has already been read as 2018 data
 
         # So let's start the EOI runs prediction at the end of 6th over (then 10th and finally 15th overs for Innings 1)

        # First call functions to get the best fit models on Training Data at the end of various overs, 
        # and also winner predictors (Training Data is revised as testing is done on each successive season):
  
  runsInn1EOIAtOver6Model  <- modelAICbestFitEOI_fn (over= 6, matSummTrngInit) # end of 6th over model to predict EOI Runs
  runsInn1EOIAtOver10Model <- modelAICbestFitEOI_fn (over=10, matSummTrngInit) # end of 10th over model to predict EOI Runs
  runsInn1EOIAtOver15Model <- modelAICbestFitEOI_fn (over=15, matSummTrngInit) # end of 15th over model to predict EOI Runs
  runsInn2EOIAtOver26Model <- modelAICbestFitEOI_fn (over=26, matSummTrngInit)  # end of 26th over model to predict EOI Runs
  runsInn2EOIAtOver30Model <- modelAICbestFitEOI_fn (over=30, matSummTrngInit)
  runsInn2EOIAtOver35Model <- modelAICbestFitEOI_fn (over=35, matSummTrngInit)

  winnerInn2AtOver20Model <- predictWinner_fn (over = 20, matSummTrng = matSummTrngInit)
  winnerInn2AtOver26Model <- predictWinner_fn (over = 26, matSummTrng = matSummTrngInit) # predicting winner at the end of 26th over
  winnerInn2AtOver30Model <- predictWinner_fn (over = 30, matSummTrng = matSummTrngInit)
  winnerInn2AtOver35Model <- predictWinner_fn (over = 35, matSummTrng = matSummTrngInit)

  for (row in 1:nrow(matSummTestSeason)) {  # Process 1 match at a time after initializing variables from prev row
            # initialize variables used to populate the dataframe with results of predictions
    matchID <- matSummTestSeason$Match_id[row]  # 1 row only per match
    matSummTestMatch <- createMatchDataSlice_fun (dataFrame = matSummTestSeason, minSeason = TestSeasonSlice, 
                                        maxSeason = TestSeasonSlice, matchId = matSummTestSeason$Match_id[row])
    if (is.na(matSummTestMatch$BatFirst)) {
      next
    } 
    Over6EOIRunsPred = predict (runsInn1EOIAtOver6Model, newdata = matSummTestMatch)
    Over6RunsErr <- Over6EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
    Over6PerErr <- (Over6RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    if (matSummTestMatch$Inn1EOIOvers >= 10) { #Only if the 1st Innings lasts at least 10 overs
      Over10EOIRunsPred = predict (runsInn1EOIAtOver10Model, newdata = matSummTestMatch)
      Over10RunsErr <- Over10EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
      Over10PerErr <- (Over10RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    }   # end of 10th over processing
    if (matSummTestMatch$Inn1EOIOvers >= 15) { #Only if the 1st Innings lasts at least 15 overs
      Over15EOIRunsPred = predict (runsInn1EOIAtOver15Model, newdata = matSummTestMatch)
                      #print (row)
                      #print (matSummTestMatch$Inn1EOIRuns)
                      #print (Over10EOIRunsPred[1])
      Over15RunsErr <- Over15EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
      Over15PerErr <- (Over15RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
                      #print (Over10PerErr[1])
    }  # end of 15th over processing
    
                  # Now take a shot at predciting winner at the end of 1st innings; 
                  # I am getting a low adjusted R2 of 0.21 !!
    Over20WinnerPredBatFirstTeamModel = predict (winnerInn2AtOver20Model, newdata = matSummTestMatch)
    if (Over20WinnerPredBatFirstTeamModel >0.5 ) {  # Prediction that Team Batting First will win
      correctmatchWinnerPredictedModelOver20 <- ifelse (matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                        TRUE, FALSE )
      predO20TeamBatFirstWinMod <- TRUE
    } else { # Model is predicting Team 2 (batting second) will beat Team 1 (batting 1st in Overs 1-20)
      correctmatchWinnerPredictedModelOver20 <- ifelse (matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                        TRUE, FALSE )
      predO20TeamBatFirstWinMod <- FALSE
    }
                  #1st innings processing done. Moving on to 2nd innings with Over 21 onwards
    Over26EOIRunsPred = predict (runsInn2EOIAtOver26Model, newdata = matSummTestMatch)
    Over26RunsErr <- Over26EOIRunsPred[1] - matSummTestMatch$Inn2EOIRuns
    Over26PerErr <- (Over26RunsErr/matSummTestMatch$Inn2EOIRuns) * 100 # Percentage error w.r.t. actual EOI
                # And now get the winner prediction based on winner prediction model:
    Over26WinnerPredBatFirstTeamModel = predict (winnerInn2AtOver26Model, newdata = matSummTestMatch)
    if (Over26WinnerPredBatFirstTeamModel >0.5 ) {  # Prediction for Team Bat First will win
      correctmatchWinnerPredictedModelOver26 <- ifelse (matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                      TRUE, FALSE )
      predO26TeamBatFirstWinMod <- TRUE
    } else { # Model is predicting Team 2 (batting second) will beat Team 1 (batting 1st in Overs 1-20)
      correctmatchWinnerPredictedModelOver26 <- ifelse (matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                      TRUE, FALSE )
      predO26TeamBatFirstWinMod <- FALSE
    }
                # Now verify winner prediction based on EOIRuns prediction:
    if (Over26EOIRunsPred[1] > matSummTestMatch$Inn1EOIRuns) {  #Predicting 2nd team will score more than Team 1
      correctmatchWinnerPredictedEOIRunsOver26 <- ifelse(matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                       TRUE, FALSE)
      predO26TeamBatFirstWinEOI <- FALSE
    } else {  #Predicting Team 1 will score more than Team 2
      correctmatchWinnerPredictedEOIRunsOver26 <- ifelse(matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                       TRUE, FALSE)
      predO26TeamBatFirstWinEOI <- TRUE
    }
    
    Over30EOIRunsPred = predict (runsInn2EOIAtOver30Model, newdata = matSummTestMatch)
    Over30RunsErr <- Over30EOIRunsPred[1] - matSummTestMatch$Inn2EOIRuns
    Over30PerErr <- (Over30RunsErr/matSummTestMatch$Inn2EOIRuns) * 100 # Percentage error w.r.t. actual EOI
                # And now get the winner prediction based on winner prediction model:
    Over30WinnerPredBatFirstTeamModel = predict (winnerInn2AtOver30Model, newdata = matSummTestMatch)
    if (Over30WinnerPredBatFirstTeamModel >0.5 ) {  # Prediction for Team Bat First will win
      correctmatchWinnerPredictedModelOver30 <- ifelse (matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                      TRUE, FALSE )
      predO30TeamBatFirstWinMod <- TRUE
    } else { # Model is predicting Team 2 (batting second) will beat Team 1 (batting 1st in Overs 1-20)
      correctmatchWinnerPredictedModelOver30 <- ifelse (matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                      TRUE, FALSE )
      predO30TeamBatFirstWinMod <- FALSE
    }
              # Now verify winner prediction based on EOIRuns prediction:
    if (Over30EOIRunsPred[1] > matSummTestMatch$Inn1EOIRuns) {  #Predicting 2nd team will score more than Team 1
      correctmatchWinnerPredictedEOIRunsOver30 <- ifelse(matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                       TRUE, FALSE)
      predO30TeamBatFirstWinEOI <- FALSE
    } else {  #Predicting Team 1 will score more than Team 2
      correctmatchWinnerPredictedEOIRunsOver30 <- ifelse(matSummTestMatch$BatFirst == 
                                                           matSummTestMatch$winner, TRUE, FALSE)
      predO30TeamBatFirstWinEOI <- TRUE
    }
  
    if (matSummTestMatch$Inn2EOIOvers >= 15) { #Only if the 2nd Innings lasts at least 15 overs (equiv to 35 overs of match)
      Over35EOIRunsPred = predict (runsInn2EOIAtOver35Model, newdata = matSummTestMatch)
      Over35RunsErr <- Over35EOIRunsPred[1] - matSummTestMatch$Inn2EOIRuns
      Over35PerErr <- (Over35RunsErr/matSummTestMatch$Inn2EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    # And now get the winner prediction based on winner prediction model:
      Over35WinnerPredBatFirstTeamModel = predict (winnerInn2AtOver35Model, newdata = matSummTestMatch)
      if (Over35WinnerPredBatFirstTeamModel >0.5 ) {  # Prediction for Team Bat First will win
        correctmatchWinnerPredictedModelOver35 <- ifelse (matSummTestMatch$BatFirst == 
                                                            matSummTestMatch$winner, TRUE, FALSE )
        predO35TeamBatFirstWinMod <- TRUE
      } else { # Model is predicting Team 2 (batting second) will beat Team 1 (batting 1st in Overs 1-20)
        correctmatchWinnerPredictedModelOver35 <- ifelse (matSummTestMatch$BatSecond == 
                                                            matSummTestMatch$winner, TRUE, FALSE )
        predO35TeamBatFirstWinMod <- FALSE
      }
                    # Now verify winner prediction based on EOIRuns prediction:
      if (Over35EOIRunsPred[1] > matSummTestMatch$Inn1EOIRuns) {  #Predicting 2nd team will score more than Team 1
        correctmatchWinnerPredictedEOIRunsOver35 <- ifelse(matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                         TRUE, FALSE)
        predO35TeamBatFirstWinEOI <- FALSE
      } else {  #Predicting Team 1 will score more than Team 2
        correctmatchWinnerPredictedEOIRunsOver35 <- ifelse(matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                         TRUE, FALSE)
        predO35TeamBatFirstWinEOI <- TRUE
      } #Predicting Team 1 will score more than Team 2
    } #Only if the 2nd Innings lasts at least 15 overs
  
          # Now store prediction results in a data frame for summary analysis
    if (FirstTime) {  # create the data frame only 1 time of the processing
      FirstTime <- FALSE
      predRes_df <- data.frame (season = matSummTestMatch$Season, matchID, 
                            inn1EOIRuns = matSummTestMatch$Inn1EOIRuns, 
                            predO6EOIRuns = Over6EOIRunsPred[1], 
                            errO6Run = round(Over6RunsErr,2), errO6Per = round(Over6PerErr,2), 
                            predO10EOIRuns = Over10EOIRunsPred[1], 
                            errO10Run = round(Over10RunsErr,2), errO10Per = round(Over10PerErr,2),
                            predO15EOIRuns = Over15EOIRunsPred[1], 
                            errO15Run = round(Over15RunsErr,2), errO15Per = round(Over15PerErr,2),
                            predO20TeamBatFirstWinMod, 
                            actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                            correctPredWinModO20 = correctmatchWinnerPredictedModelOver20,
                            inn2EOIRuns = matSummTestMatch$Inn2EOIRuns,
                            predO26EOIRuns = Over26EOIRunsPred[1], 
                            errO26Run = round(Over26RunsErr,2), errO26Per = round(Over26PerErr,2), 
                            predO26TeamBatFirstWinMod, 
                            actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                            correctPredWinModO26 = correctmatchWinnerPredictedModelOver26,
                            predO26TeamBatFirstWinEOI, 
                            correctPredWinEOIO26 = correctmatchWinnerPredictedEOIRunsOver26,
                            predO30EOIRuns = Over30EOIRunsPred[1], 
                            errO30Run = round(Over30RunsErr,2), errO30Per = round(Over30PerErr,2),
                            predO30TeamBatFirstWinMod,
                            correctPredWinModO30 = correctmatchWinnerPredictedModelOver30,
                            predO30TeamBatFirstWinEOI,
                            correctPredWinEOIO30 = correctmatchWinnerPredictedEOIRunsOver30,
                            predO30EOIRuns = Over30EOIRunsPred[1], 
                            errO35Run = round(Over35RunsErr,2), errO35Per = round(Over35PerErr,2), 
                            predO35TeamBatFirstWinMod,
                            correctPredWinModO35 = correctmatchWinnerPredictedModelOver35,
                            predO35TeamBatFirstWinEOI,
                            correctPredWinEOIO35 = correctmatchWinnerPredictedEOIRunsOver35
                            ) 
  } else { # add row to the existing data frame
      newrow_df <- data.frame (season = matSummTestMatch$Season, matchID, 
                             inn1EOIRuns = matSummTestMatch$Inn1EOIRuns, 
                             predO6EOIRuns = Over6EOIRunsPred[1], 
                             errO6Run = round(Over6RunsErr,2), errO6Per = round(Over6PerErr,2), 
                             predO10EOIRuns = Over10EOIRunsPred[1], 
                             errO10Run = round(Over10RunsErr,2), errO10Per = round(Over10PerErr,2),
                             predO15EOIRuns = Over15EOIRunsPred[1], 
                             errO15Run = round(Over15RunsErr,2), errO15Per = round(Over15PerErr,2),
                             predO20TeamBatFirstWinMod, 
                             actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                             correctPredWinModO20 = correctmatchWinnerPredictedModelOver20,
                             inn2EOIRuns = matSummTestMatch$Inn2EOIRuns,
                             predO26EOIRuns = Over26EOIRunsPred[1], 
                             errO26Run = round(Over26RunsErr,2), errO26Per = round(Over26PerErr,2), 
                             predO26TeamBatFirstWinMod, 
                             actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                             correctPredWinModO26 = correctmatchWinnerPredictedModelOver26,
                             predO26TeamBatFirstWinEOI, 
                             correctPredWinEOIO26 = correctmatchWinnerPredictedEOIRunsOver26,
                             predO30EOIRuns = Over30EOIRunsPred[1], 
                             errO30Run = round(Over30RunsErr,2), errO30Per = round(Over30PerErr,2),
                             predO30TeamBatFirstWinMod,
                             correctPredWinModO30 = correctmatchWinnerPredictedModelOver30,
                             predO30TeamBatFirstWinEOI,
                             correctPredWinEOIO30 = correctmatchWinnerPredictedEOIRunsOver30,
                             predO30EOIRuns = Over30EOIRunsPred[1], 
                             errO35Run = round(Over35RunsErr,2), errO35Per = round(Over35PerErr,2), 
                             predO35TeamBatFirstWinMod,
                             correctPredWinModO35 = correctmatchWinnerPredictedModelOver35,
                             predO35TeamBatFirstWinEOI,
                             correctPredWinEOIO35 = correctmatchWinnerPredictedEOIRunsOver35
                              ) 
      predRes_df <- rbind(predRes_df, newrow_df)
    }
  
  } # finished procesing all matches of a season
  
          ## Append this season's data to the Training Data (adding successively each season on which Testing has been completed)
  matSummTrngInit <- createMatchDataSlice_fun(minSeason = minSeasonInit, maxSeason = TestSeasonSlice)
              print (TestSeasonSlice)
  TestSeasonSlice <- TestSeasonSlice + 1  # Start procesing next season's test data
  
} # end of Start processing matches in each season, one season at a time

##################### ALL PREDICTIONS?CLASSIFICATIONS COMPLETE ##########################

# Now summarize/analyze the predictions

#if (runType != "FinalTest" ) {
  predResSumm_df <- predRes_df %>%
    mutate (SEO6 = (errO6Run) ^ 2, SEO10 = ( errO10Run) ^ 2,
            SEO15 = (errO15Run) ^ 2, SEO26 = (errO26Run) ^ 2, 
            SEO30 = (errO30Run) ^ 2, SEO35 = (errO35Run) ^2 ) %>%
    group_by(season) %>% 
    mutate ( RMSE_O6 =  (sum(SEO6)/n()) ^ 0.5, 
             RMSE_O10 = (sum(SEO10)/n()) ^ 0.5, RMSE_O15 = (sum(SEO15)/n()) ^ 0.5,
             RMSE_O26 = (sum(SEO26)/n()) ^ 0.5, 
             RMSE_O30 = (sum(SEO30)/n()) ^ 0.5, RMSE_O35 = (sum(SEO35)/n()) ^ 0.5, 
                      # Now compute R square: R2 = 1 - (SSE/SST)
             R2_O6 =  1 - (sum(SEO6)/sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),
             R2_O10 = 1 - (sum(SEO10)/sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),
             R2_O15 = 1 - (sum(SEO15)/sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),
             R2_O26 = 1 - (sum(SEO26)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),
             R2_O30 = 1 - (sum(SEO30)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),
             R2_O35 = 1 - (sum(SEO35)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),
                    # Now compute how many times our model correctly predicted match winner per season
             matWinModPer_O20 = sum(correctPredWinModO20) *100/n(),
             matWinModPer_O26 = sum(correctPredWinModO26) *100/n(),
             matWinEOIPer_O26 = sum(correctPredWinEOIO26) *100/n(), 
             matWinModPer_O30 = sum(correctPredWinModO30) *100/n(),
             matWinEOIPer_O30 = sum(correctPredWinEOIO30) *100/n(),
             matWinModPer_O35 = sum(correctPredWinModO35) *100/n(),
             matWinEOIPer_O35 = sum(correctPredWinEOIO35) *100/n()
             ) %>%
    select (season, RMSE_O6, RMSE_O10,RMSE_O15, RMSE_O26, RMSE_O30,RMSE_O35,
            R2_O6, R2_O10, R2_O15, R2_O26, R2_O30, R2_O35, matWinModPer_O20,
            matWinModPer_O26, matWinEOIPer_O26, matWinModPer_O30, matWinEOIPer_O30,
            matWinModPer_O35, matWinEOIPer_O35) %>% 
    distinct()
#  } # if not (runType == "FinalTest" )

        # Create a confusion matrix for match winner predictions after various overs of 2nd innings
if (runType == "FinalTest") {
  predRes_df1 <- predRes_df %>% group_by (season, matchID) %>%
    filter (matchID == max(matchID)) %>% 
    filter(season == MaxTestSeasonData)
} else {
  predRes_df1 <- predRes_df %>% group_by (season, matchID) %>%
    filter (matchID == max(matchID)) 
}
confMatrix_O20 <- confusionMatrix(as.factor(predRes_df1$predO20TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O26 <- confusionMatrix(as.factor(predRes_df1$predO26TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O30 <- confusionMatrix(as.factor(predRes_df1$predO30TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O35 <- confusionMatrix(as.factor(predRes_df1$predO35TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))

            print (confMatrix_O20, printStats = FALSE)            
            print (confMatrix_O26, printStats = FALSE)
            print (confMatrix_O30, printStats = FALSE)
            print (confMatrix_O35, printStats = FALSE)

# write out the predictions for EOI Runs and winners to a csv file: 

write.csv(predRes_df, "MatchByMatchResultPredictionsIPL.csv")
write.csv(predResSumm_df, "SummarizedResultPredictionsIPL.csv")
  







