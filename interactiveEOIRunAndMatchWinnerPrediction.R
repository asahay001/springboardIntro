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
    setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")
library("caret")

runType <- "FinalTest"  # Final Test is ONLY for 2018 Eliminator matches

#runType <- "Regular"    # For all regular training and test iterations


    # Read the match summary csv file previously prepared with scorePred_ver1.R
    # or for 2018 file with dataWrangling_2018IPLDataForTesting
matSummDet <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, 
                       sep = "," #, stringsAsFactors = FALSE
                       )

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
  matSummTestSeason <- read.csv(file= "wrangled_matchSummaryDataIPL2018only.csv", 
                                header = TRUE, sep = "," #, stringsAsFactors = FALSE
                                )
  TestSeasonSlice <- max(matSummTestSeason$Season)
  MaxTestSeasonData <- TestSeasonSlice  # 2018; just the one year
  maxSeasonInit <- maxSeasonData        # Use all of the 2008-2017 data for training the model
  FirstTime <- FALSE
  }
      # Training Data:
      ## First create the Training Data (adding successively each season on which Testing has been completed)
matSummTrngInit <- createMatchDataSlice_fn(minSeason = minSeasonInit, maxSeason = maxSeasonInit)

while (TestSeasonSlice <= MaxTestSeasonData) {  
              # Start testing model on matches in each season, one season at a time
  if (runType != "FinalTest") {
      matSummTestSeason <- createMatchDataSlice_fn(minSeason = TestSeasonSlice, 
                                              maxSeason = TestSeasonSlice)  # Test data for the model: successively 2015, 2016 and 2017 season data 
  }      # else matSummTestSeason has already been read as 2018 data
 
            ## First update with recent winners between 2 teams in recent matches
            ## This is done once at the start of processing for EACH iteration:
            ## so if the Training Data is up till 2014, we consider matches only up till 2014,
            ## when the Training Data is extended through 2015, we consider matches till 2105,
            ## similarly through 2016 next, and finally all the data, i.e., through 2017:
  matSummTrngInit = updateRecentWinsInDataSet_fn (matSummTrngInit)
  
  # Next create stats for matches played by each team and win percentage of the matches played
  # ONLY through the Seasons being considered (not all of the data set unless its the last iteration)
  matSummTrngInit = updateWinStats_fn (matSummTrngInit)
  
  # Now call functions to get the best fit models on Training Data at the end of various overs, 
  # and also winner predictors (Training Data is revised as testing is done on each successive season):
  runsInn1EOIAtOver6Model  <- modelAICbestFitEOI_fn (over= 6, matSummTrngInit) # end of 6th over model to predict EOI Runs
  runsInn1EOIAtOver10Model <- modelAICbestFitEOI_fn (over=10, matSummTrngInit) # end of 10th over model to predict EOI Runs
  runsInn1EOIAtOver15Model <- modelAICbestFitEOI_fn (over=15, matSummTrngInit) # end of 15th over model to predict EOI Runs
  runsInn2EOIAtOver26Model <- modelAICbestFitEOI_fn (over=26, matSummTrngInit)  # end of 26th over model to predict EOI Runs
  runsInn2EOIAtOver30Model <- modelAICbestFitEOI_fn (over=30, matSummTrngInit)
  runsInn2EOIAtOver35Model <- modelAICbestFitEOI_fn (over=35, matSummTrngInit)

  winnerAtOver0Model      <- predictWinner_fn (over =  0, matSummTrng = matSummTrngInit)
        ## print (winnerAtOver0Model)
  winnerInn2AtOver20Model <- predictWinner_fn (over = 20, matSummTrng = matSummTrngInit)
  winnerInn2AtOver26Model <- predictWinner_fn (over = 26, matSummTrng = matSummTrngInit) # predicting winner at the end of 26th over
  winnerInn2AtOver30Model <- predictWinner_fn (over = 30, matSummTrng = matSummTrngInit)
  winnerInn2AtOver35Model <- predictWinner_fn (over = 35, matSummTrng = matSummTrngInit)
     ## print("after initial prediction calls")
      
  # Now account for new teams that may come in a new season (new levels):
  winnerInn2AtOver20Model$xlevels[["BatFirst"]] <- 
    union(winnerInn2AtOver20Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  winnerInn2AtOver26Model$xlevels[["BatFirst"]] <- 
    union(winnerInn2AtOver26Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  winnerInn2AtOver30Model$xlevels[["BatFirst"]] <- 
    union(winnerInn2AtOver30Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  winnerInn2AtOver35Model$xlevels[["BatSecond"]] <- 
    union(winnerInn2AtOver35Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  
  winnerInn2AtOver20Model$xlevels[["BatSecond"]] <- 
    union(winnerInn2AtOver20Model$xlevels[["BatSecond"]], levels(matSummTestSeason$BatSecond))
  winnerInn2AtOver26Model$xlevels[["BatSecond"]] <- 
    union(winnerInn2AtOver26Model$xlevels[["BatSecond"]], levels(matSummTestSeason$BatSecond))
  winnerInn2AtOver30Model$xlevels[["BatSecond"]] <- 
    union(winnerInn2AtOver30Model$xlevels[["BatSecond"]], levels(matSummTestSeason$BatSecond))
  winnerInn2AtOver35Model$xlevels[["BatSecond"]] <- 
    union(winnerInn2AtOver35Model$xlevels[["BatSecond"]], levels(matSummTestSeason$BatSecond))
  
  winnerAtOver0Model$xlevels[["BatFirst"]] <- 
    union(winnerAtOver0Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn1EOIAtOver6Model$xlevels[["BatFirst"]] <- 
    union(runsInn1EOIAtOver6Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn1EOIAtOver10Model$xlevels[["BatFirst"]] <- 
    union(runsInn1EOIAtOver10Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn1EOIAtOver15Model$xlevels[["BatFirst"]] <- 
    union(runsInn1EOIAtOver15Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn2EOIAtOver26Model$xlevels[["BatFirst"]] <- 
    union(runsInn2EOIAtOver26Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn2EOIAtOver30Model$xlevels[["BatFirst"]] <- 
    union(runsInn2EOIAtOver30Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
  runsInn2EOIAtOver35Model$xlevels[["BatFirst"]] <- 
    union(runsInn2EOIAtOver35Model$xlevels[["BatFirst"]], levels(matSummTestSeason$BatFirst))
         ## print ("after all leveling")
  
  for (row in 1:nrow(matSummTestSeason)) {  # Process 1 match at a time after initializing variables from prev row
            # initialize variables used to populate the dataframe with results of predictions
    matchID <- matSummTestSeason$Match_id[row]  # 1 row only per match
    matSummTestMatch <- createMatchDataSlice_fn (dataFrame = matSummTestSeason, minSeason = TestSeasonSlice, 
                                        maxSeason = TestSeasonSlice, matchId = matSummTestSeason$Match_id[row])
    if (is.na(matSummTestMatch$BatFirst)) {
      next
    } 
              # So let's start the End Of Innings (EOI) runs prediction at the end of 6th over 
              # (then 10th and finally 15th overs for Innings 1)
   ## print ("Before 6")
    Over6EOIRunsPred = predict (runsInn1EOIAtOver6Model, newdata = matSummTestMatch)
   ## print ("After 6")
    Over6RunsErr <- Over6EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
    Over6PerErr <- (Over6RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    if (matSummTestMatch$Inn1EOIOvers >= 10) { #Only if the 1st Innings lasts at least 10 overs
            ## print ("Before 10")
      Over10EOIRunsPred = predict (runsInn1EOIAtOver10Model, newdata = matSummTestMatch)
           ## print ("After 10")
      Over10RunsErr <- Over10EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
      Over10PerErr <- (Over10RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    }   # end of 10th over processing
    if (matSummTestMatch$Inn1EOIOvers >= 15) { #Only if the 1st Innings lasts at least 15 overs
           ## print ("Before 15")
      Over15EOIRunsPred = predict (runsInn1EOIAtOver15Model, newdata = matSummTestMatch)
                      #print (row)
                      #print (matSummTestMatch$Inn1EOIRuns)
                      #print (Over10EOIRunsPred[1])
      Over15RunsErr <- Over15EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
      Over15PerErr <- (Over15RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
                      #print (Over10PerErr[1])
    }  # end of 15th over processing
    
                  # Try to predict match winner at the start of the match, right after the
                  # toss, before even a ball is bowled. This is the baseline winner prediction.
                  # Anything above 50% accurcy will make the data science project worth the effort
    
             ## print ("Before Win 0")
    Over0WinnerPredBatFirstTeamModel = predict (winnerAtOver0Model, newdata = matSummTestMatch)
              ##print (Over0WinnerPredBatFirstTeamModel)
        
    if (Over0WinnerPredBatFirstTeamModel >0.5 ) {  # Prediction that Team Batting First will win
      correctmatchWinnerPredictedModelOver0 <- ifelse (matSummTestMatch$BatFirst == matSummTestMatch$winner,
                                                        TRUE, FALSE )
      predO0TeamBatFirstWinMod <- TRUE
    } else { # Model is predicting Team 2 (batting second) will beat Team 1 (batting 1st in Overs 1-20)
      correctmatchWinnerPredictedModelOver0 <- ifelse (matSummTestMatch$BatSecond == matSummTestMatch$winner,
                                                        TRUE, FALSE )
      predO0TeamBatFirstWinMod <- FALSE
    } 
                  # Now take a shot at predciting winner at the end of 1st innings; 
                  # Hopefully accuracy will be better than at the start of the match !!
        
             ## print ("Before Win 20")
    Over20WinnerPredBatFirstTeamModel = predict (winnerInn2AtOver20Model, newdata = matSummTestMatch)
                  # print ("20 done")
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
                  # print ("26 start")
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
      predRes_df <- data.frame (rowID = 1,
                            season = matSummTestMatch$Season, matchID,
                            actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                            predO0TeamBatFirstWinMod, predO20TeamBatFirstWinMod,
                            predO26TeamBatFirstWinMod, predO30TeamBatFirstWinMod,
                            predO35TeamBatFirstWinMod, predO26TeamBatFirstWinEOI,
                            predO30TeamBatFirstWinEOI, predO35TeamBatFirstWinEOI,
                            inn1EOIRuns    = matSummTestMatch$Inn1EOIRuns,
                            predO6EOIRuns  = round(Over6EOIRunsPred[1]),
                            predO10EOIRuns = round(Over10EOIRunsPred[1]),
                            predO15EOIRuns = round(Over15EOIRunsPred[1]),
                            inn2EOIRuns    = matSummTestMatch$Inn2EOIRuns,
                            predO26EOIRuns = round(Over26EOIRunsPred[1]),
                            predO30EOIRuns = round(Over30EOIRunsPred[1]),
                            predO35EOIRuns = round(Over35EOIRunsPred[1]),
                            errO35Run = round(Over35RunsErr,2), errO35Per = round(Over35PerErr,2),
                            errO30Run = round(Over30RunsErr,2), errO30Per = round(Over30PerErr,2),
                            errO26Run = round(Over26RunsErr,2), errO26Per = round(Over26PerErr,2),
                            errO15Run = round(Over15RunsErr,2), errO15Per = round(Over15PerErr,2),
                            errO10Run = round(Over10RunsErr,2), errO10Per = round(Over10PerErr,2),
                            errO6Run  = round(Over6RunsErr,2), errO6Per = round(Over6PerErr,2),
                            correctPredWinEOIO35 = correctmatchWinnerPredictedEOIRunsOver35,
                            correctPredWinEOIO30 = correctmatchWinnerPredictedEOIRunsOver30,
                            correctPredWinEOIO26 = correctmatchWinnerPredictedEOIRunsOver26,
                            correctPredWinModO35 = correctmatchWinnerPredictedModelOver35,
                            correctPredWinModO30 = correctmatchWinnerPredictedModelOver30,
                            correctPredWinModO26 = correctmatchWinnerPredictedModelOver26,
                            correctPredWinModO20 = correctmatchWinnerPredictedModelOver20,
                            correctPredWinModO0  = correctmatchWinnerPredictedModelOver0
                            ) 
  } else { # add row to the existing data frame
      newrow_df <- data.frame (rowID = nrow(predRes_df) + 1,
                               season = matSummTestMatch$Season, matchID,
                               actualTeamBatFirstWin = matSummTestMatch$TeamBattingFirstWon,
                               predO0TeamBatFirstWinMod, predO20TeamBatFirstWinMod,
                               predO26TeamBatFirstWinMod, predO30TeamBatFirstWinMod,
                               predO35TeamBatFirstWinMod, predO26TeamBatFirstWinEOI,
                               predO30TeamBatFirstWinEOI, predO35TeamBatFirstWinEOI,
                               inn1EOIRuns    = matSummTestMatch$Inn1EOIRuns,
                               predO6EOIRuns  = round(Over6EOIRunsPred[1]),
                               predO10EOIRuns = round(Over10EOIRunsPred[1]),
                               predO15EOIRuns = round(Over15EOIRunsPred[1]),
                               inn2EOIRuns    = matSummTestMatch$Inn2EOIRuns,
                               predO26EOIRuns = round(Over26EOIRunsPred[1]),
                               predO30EOIRuns = round(Over30EOIRunsPred[1]),
                               predO35EOIRuns = round(Over35EOIRunsPred[1]),
                               errO35Run = round(Over35RunsErr), errO35Per = round(Over35PerErr,1),
                               errO30Run = round(Over30RunsErr), errO30Per = round(Over30PerErr,1),
                               errO26Run = round(Over26RunsErr), errO26Per = round(Over26PerErr,1),
                               errO15Run = round(Over15RunsErr), errO15Per = round(Over15PerErr,1),
                               errO10Run = round(Over10RunsErr), errO10Per = round(Over10PerErr,1),
                               errO6Run  = round(Over6RunsErr), errO6Per = round(Over6PerErr,1),
                               correctPredWinEOIO35 = correctmatchWinnerPredictedEOIRunsOver35,
                               correctPredWinEOIO30 = correctmatchWinnerPredictedEOIRunsOver30,
                               correctPredWinEOIO26 = correctmatchWinnerPredictedEOIRunsOver26,
                               correctPredWinModO35 = correctmatchWinnerPredictedModelOver35,
                               correctPredWinModO30 = correctmatchWinnerPredictedModelOver30,
                               correctPredWinModO26 = correctmatchWinnerPredictedModelOver26,
                               correctPredWinModO20 = correctmatchWinnerPredictedModelOver20,
                               correctPredWinModO0  = correctmatchWinnerPredictedModelOver0
                              ) 
      predRes_df <- rbind(predRes_df, newrow_df)
    }
  
  } # finished procesing all matches of a season
  
          ## Append this season's data to the Training Data (adding successively each season on which Testing has been completed)
  matSummTrngInit <- createMatchDataSlice_fn(minSeason = minSeasonInit, maxSeason = TestSeasonSlice)
              # print (TestSeasonSlice)
  TestSeasonSlice <- TestSeasonSlice + 1  # Start procesing next season's test data
  
} # end of Start processing matches in each season, one season at a time

##################### ALL PREDICTIONS/CLASSIFICATIONS COMPLETE ##########################

# Now summarize/analyze the predictions

#if (runType != "FinalTest" ) {
  predResSumm_df <- predRes_df %>%
    mutate (SEO6 = (errO6Run) ^ 2, SEO10 = ( errO10Run) ^ 2,
            SEO15 = (errO15Run) ^ 2, SEO26 = (errO26Run) ^ 2, 
            SEO30 = (errO30Run) ^ 2, SEO35 = (errO35Run) ^2 ) %>%
    group_by(season) %>% 
    mutate ( RMSE_O6 =  round((sum(SEO6)/n()) ^ 0.5,2), 
             RMSE_O10 = round((sum(SEO10)/n()) ^ 0.5,2), RMSE_O15 = round((sum(SEO15)/n()) ^ 0.5,2),
             RMSE_O26 = round((sum(SEO26)/n()) ^ 0.5,2), 
             RMSE_O30 = round((sum(SEO30)/n()) ^ 0.5,2), RMSE_O35 = round((sum(SEO35)/n()) ^ 0.5,2), 
                      # Now compute R square: R2 = 1 - (SSE/SST)
             adjRsqr_O6  = round(1 - (sum(SEO6) /sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),2),
             adjRsqr_O10 = round(1 - (sum(SEO10)/sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),2),
             adjRsqr_O15 = round(1 - (sum(SEO15)/sum((inn1EOIRuns - mean(inn1EOIRuns))^2)),2),
             adjRsqr_O26 = round(1 - (sum(SEO26)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),2),
             adjRsqr_O30 = round(1 - (sum(SEO30)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),2),
             adjRsqr_O35 = round(1 - (sum(SEO35)/sum((inn2EOIRuns - mean(inn2EOIRuns))^2)),2),
                    # Now compute how many times our model correctly predicted match winner per season
             matWinModPer_O0  = round(sum(correctPredWinModO0)  *100/n()),
             matWinModPer_O20 = round(sum(correctPredWinModO20) *100/n()),
             matWinModPer_O26 = round(sum(correctPredWinModO26) *100/n()),
             matWinModPer_O30 = round(sum(correctPredWinModO30) *100/n()),
             matWinModPer_O35 = round(sum(correctPredWinModO35) *100/n()),
             matWinEOIPer_O26 = round(sum(correctPredWinEOIO26) *100/n()), 
             matWinEOIPer_O30 = round(sum(correctPredWinEOIO30) *100/n()),
             matWinEOIPer_O35 = round(sum(correctPredWinEOIO35) *100/n())
             ) %>%
    select (season, PredictionAccuracyOver0 = matWinModPer_O0, 
            PredictionAccuracyOver20 = matWinModPer_O20,
            PredictionAccuracyOver26 = matWinModPer_O26, 
            PredictionAccuracyOver30 = matWinModPer_O30, 
            PredictionAccuracyOver35 = matWinModPer_O35, 
            matWinEOIPer_O26, matWinEOIPer_O30, matWinEOIPer_O35,
            RMSE_O6, RMSE_O10,RMSE_O15, RMSE_O26, RMSE_O30,
            RMSE_O35, adjRsqr_O6, adjRsqr_O10, adjRsqr_O15, 
            adjRsqr_O26, adjRsqr_O30, adjRsqr_O35 
            ) %>% 
    distinct()
#  } # if not (runType == "FinalTest" )

        # Create a confusion matrix for match winner predictions after various overs of 2nd innings
predRes_df1 <- predRes_df %>% group_by (season, matchID) %>% filter (rowID == max(rowID)) 
                                            ## That filtered out multiple runs of FinalTest data
if (runType == "FinalTest") {
  predRes_df1 <- predRes_df1 %>% filter(season == MaxTestSeasonData) ## Just for 2018 season
}

confMatrix_O0  <- confusionMatrix(as.factor(predRes_df1$predO0TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O20 <- confusionMatrix(as.factor(predRes_df1$predO20TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O26 <- confusionMatrix(as.factor(predRes_df1$predO26TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O30 <- confusionMatrix(as.factor(predRes_df1$predO30TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
confMatrix_O35 <- confusionMatrix(as.factor(predRes_df1$predO35TeamBatFirstWinMod), 
                                  as.factor(predRes_df1$actualTeamBatFirstWin))
                  # Since confusionMatrix output is a list, we can access the results:
accuracy0Over  <- round(confMatrix_O0$overall[1]  *100, 0)
accuracy20Over <- round(confMatrix_O20$overall[1] *100, 0)
accuracy26Over <- round(confMatrix_O26$overall[1] *100, 0)
accuracy30Over <- round(confMatrix_O30$overall[1] *100, 0)
accuracy35Over <- round(confMatrix_O35$overall[1] *100, 0)

paste ("Winner Prediction Right After Toss: Accuracy is", accuracy0Over, "%")            
            print (confMatrix_O0, printStats = FALSE) 
paste ("Winner Prediction after 20 Overs: Accuracy is", accuracy20Over, "%")            
            print (confMatrix_O20, printStats = FALSE) 
paste ("Winner Prediction after 26 Overs: Accuracy is", accuracy26Over, "%")
            print (confMatrix_O26, printStats = FALSE)
paste ("Winner Prediction after 30 Overs: Accuracy is", accuracy30Over, "%")
            print (confMatrix_O30, printStats = FALSE)
paste ("Winner Prediction after 35 Overs: Accuracy is", accuracy35Over, "%")
            print (confMatrix_O35, printStats = FALSE)

            # write out the predictions for EOI Runs and winners to a csv file: 
write.csv(predRes_df, "MatchByMatchResultPredictionsIPL.csv")
if (runType == "FinalTest") {
  write.csv(predResSumm_df, "SummarizedResultPredictionsTestData2018IPL.csv")
} else {
  write.csv(predResSumm_df, "SummarizedResultPredictionsIPL.csv")
}
  







