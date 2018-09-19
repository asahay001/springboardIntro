## We will try to predict End of Innings (EOI) runs for Innings 1 at the end of 6th, 10th and 15th overs
## for these 20 overs max - a - side match. 
## And then for the team batting 2nd (Innings 2), try to predict their EOI score at the end of their
# 6th, 10th and 15th overs, which for the match are the 26th, 30th and 35th overs respectively
# And in the 2nd part, we will try to predict the match winner at the end of 26th, 30th and 35th overs 
## of each match, store the 3 predictions for each match, and compare it to the actual match result

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

TestSeasonSlice <- 2015

matSummTestSeason <- createMatchDataSlice_fun(minSeason = TestSeasonSlice, maxSeason = TestSeasonSlice)  # Test the model 

# So let's start the EOI runs prediction at the end of 6th over (then 10th and finally 15th overs for Innings 1)

# First call functins to get the best fit models at the end of various overs:
runsInn1EOIAtOver6Model <- Over6modeling_fun()   # end of 6th over model to predict EOI Runs
runsInn1EOIAtOver10Model <- Over10modeling_fun() # end of 10th over model to predict EOI Runs
runsInn1EOIAtOver15Model <- Over15modeling_fun() # end of 15th over model to predict EOI Runs

for (row in 1:nrow(matSummTestSeason)) { # Process 1 match at a time after initializing variables from prev row
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
  }  # end of 10th over processing
  if (matSummTestMatch$Inn1EOIOvers >= 15) { #Only if the 1st Innings lasts at least 15 overs
    Over15EOIRunsPred = predict (runsInn1EOIAtOver15Model, newdata = matSummTestMatch)
    #print (row)
    #print (matSummTestMatch$Inn1EOIRuns)
    #print (Over10EOIRunsPred[1])
    Over15RunsErr <- Over15EOIRunsPred[1] - matSummTestMatch$Inn1EOIRuns
    Over15PerErr <- (Over15RunsErr/matSummTestMatch$Inn1EOIRuns) * 100 # Percentage error w.r.t. actual EOI
    print (Over10PerErr[1])
  }  # end of 15th over processing; 1st innings processing done. Moving on to 2nd innings with Over 21 onwards
  Over26EOIRunsPred = predict (runsInn1EOIAtOver26Model, newdata = matSummTestMatch)
  Over26RunsErr <- Over26EOIRunsPred[1] - matSummTestMatch$Inn2EOIRuns
  Over26PerErr <- (Over26RunsErr/matSummTestMatch$Inn2EOIRuns) * 100 # Percentage error w.r.t. actual EOI
  # And now get the winner prediction based on EOI scores for the 2 innings
  
  
} # finished procesing all matches of a season










