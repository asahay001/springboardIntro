# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")


library ("tidyverse")
library("dplyr")



    # Set files dirctory from where to read data (csv files). 

filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
setwd(filesDir)

    # Read Team.csv which has Team ID and Team Names. There are 13 teams that have played from 2008-2017 in IPL T20 tournament
    #teamss$Team_Name will have the names of the teams, while teams$Team_Id will have the IDs for cross-referencing across files
teams <- read.csv(file= 'Team.csv', header = TRUE, sep = ",")

    # Read Match.csv which has summary of each match played: opponents, venue, runs scored by each team, match result
innSumm <- read.csv(file= 'Match.csv', header = TRUE, sep = ",")
innSumm <- innSumm[order(innSumm$match_id),]

    # Finally read the details of each match from Ball_By_Ball.csv
matDetFile <- "Ball_By_Ball.csv"
matDet <- read.csv(file = matDetFile, header = TRUE, sep = ",")


    # select only the needed columns, then sort, then find cumulative runs scored after each ball, and then further reduce columns no longer needed
matDet <- matDet %>% select (Team_Batting, Season, Match_id = MatcH_id, Innings_No, Over_id, Ball_id, 
                             Runs_Scored, Extra_runs, Bowler_Wicket, Run_out, Team_Bowling)  %>%
  arrange (Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id) %>%
  group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  mutate (cumOver = (Over_id - 1) + 
            round(ifelse (Ball_id > 6, 0.0, (Ball_id %% 6) / 6), 2) +   # If an over had extra balls, count them as 6 balls
            ifelse(Ball_id >= 6, Ball_id %/% 6, 0 )) %>%
  mutate (cumRuns = cumsum(Runs_Scored + Extra_runs), 
          runRate = round (cumRuns / cumOver, 2), 
          cumWkts = cumsum(Bowler_Wicket + Run_out)) %>%
  select (-c(Runs_Scored, Extra_runs, Bowler_Wicket, Run_out)) %>% # Discard columns not needed anymore since cumRuns and cumWkts are built
  filter (Innings_No < 3) %>%   # Only interested in full innings 1 and 2; not super over innings to break tie
  left_join(innSumm, by = c("Match_id" = "match_id")) %>%
    select(Team_Batting:cumWkts, venueGround = Venue_Name, venueCity = City_Name, 
           toss = Toss_Winner, winner = match_winner)

    # Some of the Team_Batting and Team_bowling in the original csv file (and hence in matDet) are character strings
    # let's convert them to the TeamId joining with Teams 
matDet <- matDet %>% left_join(teams, by = c("Team_Batting"= "Team_Name")) %>%
  select(Team_Batting:winner, TeamBatID = Team_Id) %>%
  left_join(teams, by = c("Team_Bowling"= "Team_Name")) %>%
  select(Team_Batting:TeamBatID, TeamBowlID = Team_Id)
matDet$Team_Batting <- as.numeric(ifelse(is.na(matDet$TeamBatID), matDet$Team_Batting, matDet$TeamBatID))
matDet$Team_Bowling <- as.numeric(ifelse(is.na(matDet$TeamBowlID), matDet$Team_Bowling, matDet$TeamBowlID))

    # Now add the batting and bowling team names with the lookup table team; Trouble with data "as is" is that 
    # seasons 2008-2016 has numbers in the batting and bolwing team ids, but for season 2017, the actual team names are stored
    # So do the lookup in 2 steps: first for the nueric ids (they actually are factor data types)

matDet <- matDet %>% left_join(teams, by = c("Team_Batting"= "Team_Id")) %>%
  select (TeamNameBat = Team_Name, Team_Batting:winner) %>%
  left_join(teams, by = c("Team_Bowling"= "Team_Id")) %>%
  select (TeamNameBat:winner, TeamNameBowl = Team_Name, Team_Bowling) %>%
  mutate (TeamBattingWon = as.character(TeamNameBat) == as.character(winner)) %>%
  arrange(TeamNameBat, Season, Match_id, Innings_No, Over_id, Ball_id)

    # This completes the ball by ball data file preparation. Let's write it out for data exploration 
    # writing to current working directory which is defined in the variable filesDir

write.csv (matDet, "wrangled_ballByBallDataIPL.csv")

      ## Now build dataset with each match as a data row so that we can predict EOI scores in each match at
      ## for the team batting 1st at the end of 5th, 10th and 15th overs (with increasing accuracy)
      ## and predict the macth winner starting in the 2nd team's batting innings at the end of
      ## the 5th, 10th and 15th overs (so for the match 20+5, 20 + 10, and 20 + 15 overs, assuming
      ## the team batting 1st bats all of its 20 overs)
      ## So we will get runs and wickets data in each row for each over: 1 through 15 which will
      ## be used in creating the prediction model for EOI score and match winner
      
          ##  mutate (!!(sym(paste("Over", as.character(cumOver), "Runs", sep =""))) := cumRuns)

matDetOverStats <- matDet %>% ungroup %>% select (Season:cumWkts) %>%
  group_by(Season, Match_id, Innings_No, Over_id) %>%
  filter (Over_id %in% seq(1, to = 20, by =1 ) & Ball_id == max(Ball_id, na.rm = TRUE)) %>%
  ungroup() %>%  # ungrouping is necessary because Over_id as part of the group_by cannot be updated
  mutate (Over_id = ifelse (Innings_No == 2, Over_id + 20, Over_id)) %>% # 2nd Innings starts with the 21st over of the match
  mutate (cumOverRuns = paste ("Over", as.character(Over_id), "Runs", sep = ""),
          cumOverWkts = paste ("Over", as.character(Over_id), "Wkts", sep = "")
          )
              ##    !!(varOverRuns) := ifelse (cumOver == 2 & Innings_No ==1, cumRuns, 0))
          
matDetOverStats <- spread(matDetOverStats, cumOverRuns, cumRuns)
matDetOverStats <- spread(matDetOverStats, cumOverWkts, cumWkts)
        #matDetOverStats[is.na(matDetOverStats)] <- 0  # NA values can be safely replaced with 0 in this dataset for further computation

matDetOverStats <- matDetOverStats %>% select ( -cumOver, -Ball_id, -Team_Bowling) %>%
  group_by(Season, Match_id) %>%
  mutate (Over1Runs = ifelse(max(Over1Runs,na.rm=TRUE) >=0, max(Over1Runs,na.rm=TRUE), -1), 
          Over1Wkts = max(Over1Wkts,na.rm=TRUE), 
          Over2Runs = ifelse(max(Over2Runs,na.rm=TRUE) >=0, max(Over2Runs,na.rm=TRUE), -1), 
          Over2Wkts = max(Over2Wkts,na.rm=TRUE),
          Over3Runs = ifelse(max(Over3Runs,na.rm=TRUE) >=0, max(Over3Runs,na.rm=TRUE), -1),    
          Over3Wkts = max(Over3Wkts,na.rm=TRUE), 
          Over4Runs = ifelse(max(Over4Runs,na.rm=TRUE) >=0, max(Over4Runs,na.rm=TRUE), -1),    
          Over4Wkts = max(Over4Wkts,na.rm=TRUE), 
          Over5Runs = ifelse(max(Over5Runs,na.rm=TRUE) >=0, max(Over5Runs,na.rm=TRUE), -1),    
          Over5Wkts = max(Over5Wkts,na.rm=TRUE), 
          Over6Runs = ifelse(max(Over6Runs,na.rm=TRUE) >=0, max(Over6Runs,na.rm=TRUE), -1),    
          Over6Wkts = max(Over6Wkts,na.rm=TRUE), 
          Over7Runs = ifelse(max(Over7Runs,na.rm=TRUE) >=0, max(Over7Runs,na.rm=TRUE), -1),
          Over7Wkts = max(Over7Wkts,na.rm=TRUE), 
          Over8Runs = ifelse(max(Over8Runs,na.rm=TRUE) >=0, max(Over8Runs,na.rm=TRUE), -1),    
          Over8Wkts = max(Over8Wkts,na.rm=TRUE), 
          Over9Runs = ifelse(max(Over9Runs,na.rm=TRUE) >=0, max(Over9Runs,na.rm=TRUE), -1),
          Over9Wkts = max(Over9Wkts,na.rm=TRUE), 
          Over10Runs = ifelse(max(Over10Runs,na.rm=TRUE) >=0, max(Over10Runs,na.rm=TRUE), -1), 
          Over10Wkts = max(Over10Wkts,na.rm=TRUE), 
          Over11Runs = ifelse(max(Over11Runs,na.rm=TRUE) >0, max(Over12Runs,na.rm=TRUE), -1),
          Over11Wkts = max(Over11Wkts,na.rm=TRUE), 
          Over12Runs = ifelse(max(Over12Runs,na.rm=TRUE) >=0, max(Over12Runs,na.rm=TRUE), -1),
          Over12Wkts = ifelse(max(Over12Wkts,na.rm=TRUE) >=0, max(Over12Wkts,na.rm=TRUE), -1), 
          Over13Runs = ifelse(max(Over13Runs,na.rm=TRUE) >=0, max(Over13Runs,na.rm=TRUE), -1), 
          Over13Wkts = ifelse(max(Over13Wkts,na.rm=TRUE) >=0, max(Over13Wkts,na.rm=TRUE), -1), 
          Over14Runs = ifelse(max(Over14Runs,na.rm=TRUE) >=0, max(Over14Runs,na.rm=TRUE), -1), 
          Over14Wkts = ifelse(max(Over14Wkts,na.rm=TRUE) >=0, max(Over14Wkts,na.rm=TRUE), -1), 
          Over15Runs = ifelse(max(Over15Runs,na.rm=TRUE) >=0, max(Over15Runs,na.rm=TRUE), -1), 
          Over15Wkts = ifelse(max(Over15Wkts,na.rm=TRUE) >=0, max(Over15Wkts,na.rm=TRUE), -1), 
          Over16Runs = ifelse(max(Over16Runs,na.rm=TRUE) >=0, max(Over16Runs,na.rm=TRUE), -1), 
          Over16Wkts = ifelse(max(Over16Wkts,na.rm=TRUE) >=0, max(Over16Wkts,na.rm=TRUE), -1), 
          Over17Runs = ifelse(max(Over17Runs,na.rm=TRUE) >=0, max(Over17Runs,na.rm=TRUE), -1), 
          Over17Wkts = ifelse(max(Over17Wkts,na.rm=TRUE) >=0, max(Over17Wkts,na.rm=TRUE), -1), 
          Over18Runs = ifelse(max(Over18Runs,na.rm=TRUE) >=0, max(Over18Runs,na.rm=TRUE), -1), 
          Over18Wkts = ifelse(max(Over18Wkts,na.rm=TRUE) >=0, max(Over18Wkts,na.rm=TRUE), -1), 
          Over19Runs = ifelse(max(Over19Runs,na.rm=TRUE) >=0, max(Over19Runs,na.rm=TRUE), -1), 
          Over19Wkts = ifelse(max(Over19Wkts,na.rm=TRUE) >=0, max(Over19Wkts,na.rm=TRUE), -1), 
          Over20Runs = ifelse(max(Over20Runs,na.rm=TRUE) >=0, max(Over20Runs,na.rm=TRUE), -1), 
          Over20Wkts = ifelse(max(Over20Wkts,na.rm=TRUE) >=0, max(Over20Wkts,na.rm=TRUE), -1), 
          Over21Runs = ifelse(max(Over21Runs,na.rm=TRUE) >=0, max(Over21Runs,na.rm=TRUE), -1),
          Over21Wkts = ifelse(max(Over21Wkts,na.rm=TRUE) >=0, max(Over21Wkts,na.rm=TRUE), -1), 
          Over22Runs = ifelse(max(Over22Runs,na.rm=TRUE) >=0, max(Over22Runs,na.rm=TRUE), -1),
          Over22Wkts = ifelse(max(Over22Wkts,na.rm=TRUE) >=0, max(Over22Wkts,na.rm=TRUE), -1), 
          Over23Runs = ifelse(max(Over23Runs,na.rm=TRUE) >=0, max(Over23Runs,na.rm=TRUE), -1),
          Over23Wkts = ifelse(max(Over23Wkts,na.rm=TRUE) >=0, max(Over23Wkts,na.rm=TRUE), -1), 
          Over24Runs = ifelse(max(Over24Runs,na.rm=TRUE) >=0, max(Over24Runs,na.rm=TRUE), -1),
          Over24Wkts = ifelse(max(Over24Wkts,na.rm=TRUE) >=0, max(Over24Wkts,na.rm=TRUE), -1), 
          Over25Runs = ifelse(max(Over25Runs,na.rm=TRUE) >=0, max(Over25Runs,na.rm=TRUE), -1),
          Over25Wkts = ifelse(max(Over25Wkts,na.rm=TRUE) >=0, max(Over25Wkts,na.rm=TRUE), -1), 
          Over26Runs = ifelse(max(Over26Runs,na.rm=TRUE) >=0, max(Over26Runs,na.rm=TRUE), -1),
          Over26Wkts = ifelse(max(Over26Wkts,na.rm=TRUE) >=0, max(Over26Wkts,na.rm=TRUE), -1), 
          Over27Runs = ifelse(max(Over27Runs,na.rm=TRUE) >=0, max(Over27Runs,na.rm=TRUE), -1),
          Over27Wkts = ifelse(max(Over27Wkts,na.rm=TRUE) >=0, max(Over27Wkts,na.rm=TRUE), -1), 
          Over28Runs = ifelse(max(Over28Runs,na.rm=TRUE) >=0, max(Over28Runs,na.rm=TRUE), -1),
          Over28Wkts = ifelse(max(Over28Wkts,na.rm=TRUE) >=0, max(Over28Wkts,na.rm=TRUE), -1), 
          Over29Runs = ifelse(max(Over29Runs,na.rm=TRUE) >=0, max(Over29Runs,na.rm=TRUE), -1),
          Over29Wkts = ifelse(max(Over29Wkts,na.rm=TRUE) >=0, max(Over29Wkts,na.rm=TRUE), -1), 
          Over30Runs = ifelse(max(Over30Runs,na.rm=TRUE) >=0, max(Over30Runs,na.rm=TRUE), -1),
          Over30Wkts = ifelse(max(Over30Wkts,na.rm=TRUE) >=0, max(Over30Wkts,na.rm=TRUE), -1), 
          Over31Runs = ifelse(max(Over31Runs,na.rm=TRUE) >=0, max(Over31Runs,na.rm=TRUE), -1),
          Over31Wkts = ifelse(max(Over31Wkts,na.rm=TRUE) >=0, max(Over31Wkts,na.rm=TRUE), -1), 
          Over32Runs = ifelse(max(Over32Runs,na.rm=TRUE) >=0, max(Over32Runs,na.rm=TRUE), -1),
          Over32Wkts = ifelse(max(Over32Wkts,na.rm=TRUE) >=0, max(Over32Wkts,na.rm=TRUE), -1), 
          Over33Runs = ifelse(max(Over33Runs,na.rm=TRUE) >=0, max(Over33Runs,na.rm=TRUE), -1),
          Over33Wkts = ifelse(max(Over33Wkts,na.rm=TRUE) >=0, max(Over33Wkts,na.rm=TRUE), -1), 
          Over34Runs = ifelse(max(Over34Runs,na.rm=TRUE) >=0, max(Over34Runs,na.rm=TRUE), -1),
          Over34Wkts = ifelse(max(Over34Wkts,na.rm=TRUE) >=0, max(Over34Wkts,na.rm=TRUE), -1), 
          Over35Runs = ifelse(max(Over35Runs,na.rm=TRUE) >=0, max(Over35Runs,na.rm=TRUE), -1),
          Over35Wkts = ifelse(max(Over35Wkts,na.rm=TRUE) >=0, max(Over35Wkts,na.rm=TRUE), -1), 
          Over36Runs = ifelse(max(Over36Runs,na.rm=TRUE) >=0, max(Over36Runs,na.rm=TRUE), -1),
          Over36Wkts = ifelse(max(Over36Wkts,na.rm=TRUE) >=0, max(Over36Wkts,na.rm=TRUE), -1), 
          Over37Runs = ifelse(max(Over37Runs,na.rm=TRUE) >=0, max(Over37Runs,na.rm=TRUE), -1),
          Over37Wkts = ifelse(max(Over37Wkts,na.rm=TRUE) >=0, max(Over37Wkts,na.rm=TRUE), -1),
          Over38Runs = ifelse(max(Over38Runs,na.rm=TRUE) >=0, max(Over38Runs,na.rm=TRUE), -1), 
          Over38Wkts = ifelse(max(Over38Wkts,na.rm=TRUE) >=0, max(Over38Wkts,na.rm=TRUE), -1), 
          Over39Runs = ifelse(max(Over39Runs,na.rm=TRUE) >=0, max(Over39Runs,na.rm=TRUE), -1), 
          Over39Wkts = ifelse(max(Over39Wkts,na.rm=TRUE) >=0, max(Over39Wkts,na.rm=TRUE), -1), 
          Over40Runs = ifelse(max(Over40Runs,na.rm=TRUE) >=0, max(Over40Runs,na.rm=TRUE), -1), 
          Over40Wkts = ifelse(max(Over40Wkts,na.rm=TRUE) >=0, max(Over40Wkts,na.rm=TRUE), -1) 
          ) %>%  ## filter to keep only 1 row per match innings, since all the important row info is now in columns
  group_by(Season, Match_id, Innings_No) %>%
  filter (Over_id == min(Over_id)) %>%
  left_join(innSumm, by = c("Season", "Match_id", "Innings_No")) %>% 
  mutate (EOIInnOvers = paste ("Inn", as.character(Innings_No), "EOIOvers", sep = ""),
          EOIInnRuns = paste ("Inn", as.character(Innings_No), "EOIRuns", sep = ""),
          EOIInnWkts = paste ("Inn", as.character(Innings_No), "EOIWkts", sep = "")
          ) %>%
  select (Season, Match_id, Innings_No, Over_id, BatFirst = TeamNameBat, BatSecond = TeamNameBowl, toss, winner,
          TeamBattingFirstWon = TeamBattingWon, venueGround, venueCity, 
          EOIInnOvers:EOIInnWkts, EOIOver:EOIWkts, 
          interactionCurrTeams, interactionVenueBatTeam,
          Over1Runs, Over1Wkts,  Over2Runs, Over2Wkts,  Over3Runs, Over3Wkts, 
          Over4Runs = Over4Runs.x, Over4Wkts = Over4Wkts.x,  
          Over5Runs, Over5Wkts, 
          Over6Runs = Over6Runs.x, Over6Wkts = Over6Wkts.x, 
          Over7Runs, Over7Wkts, 
          Over8Runs = Over8Runs.x, Over8Wkts = Over8Wkts.x, 
          Over9Runs, Over9Wkts, 
          Over10Runs = Over10Runs.x, Over10Wkts = Over10Wkts.x, 
          Over11Runs, Over11Wkts, 
          Over12Runs = Over12Runs.x, Over12Wkts = Over12Wkts.x, 
          Over13Runs, Over13Wkts,  Over14Runs, Over14Wkts, 
          Over15Runs = Over15Runs.x, Over15Wkts = Over15Wkts.x, 
          Over16Runs, Over16Wkts, 
          Over17Runs = Over17Runs.x, Over17Wkts = Over17Wkts.x, 
          Over18Runs, Over18Wkts,  Over19Runs, Over19Wkts, 
          Over20Runs = Over20Runs.x, Over20Wkts = Over20Wkts.x, 
          Over21Runs, Over21Wkts,  Over22Runs, Over22Wkts,  Over23Runs, Over23Wkts, 
          Over24Runs, Over24Wkts,  Over25Runs, Over25Wkts,  Over26Runs, Over26Wkts, 
          Over27Runs, Over27Wkts,  Over28Runs, Over28Wkts,  Over29Runs, Over29Wkts, 
          Over30Runs, Over30Wkts,  Over31Runs, Over31Wkts,  Over32Runs, Over32Wkts, 
          Over33Runs, Over33Wkts,  Over34Runs, Over34Wkts,  Over35Runs, Over35Wkts, 
          Over36Runs, Over36Wkts,  Over37Runs, Over37Wkts, 
          Over38Runs, Over38Wkts, 
          Over39Runs, Over39Wkts, 
          Over40Runs, Over40Wkts 
          )

matDetOverStats <- spread(matDetOverStats, EOIInnOvers, EOIOver)
matDetOverStats <- spread(matDetOverStats, EOIInnRuns, EOIRuns)
matDetOverStats <- spread(matDetOverStats, EOIInnWkts, EOIWkts)

matDetOverStats <- matDetOverStats %>% 
  group_by(Season, Match_id) %>%
  mutate (Inn1EOIOvers = max(Inn1EOIOvers,na.rm=TRUE), Inn2EOIOvers = max(Inn2EOIOvers, na.rm = TRUE),
          Inn1EOIRuns = max(Inn1EOIRuns,na.rm=TRUE), Inn2EOIRuns = max(Inn2EOIRuns, na.rm = TRUE), 
          Inn1EOIWkts = max(Inn1EOIWkts,na.rm=TRUE), Inn2EOIWkts = max(Inn2EOIWkts, na.rm = TRUE)
          ) %>%
  filter(Innings_No == 1) %>%   ## Keep only 1 row per match; each row is now self-contained for a match
  filter (Inn1EOIOvers >12 & Inn2EOIOvers > 12) %>%  ## Do not want shortened matches where teams do not play 12 overs each
  select(Season, Match_id, BatFirst:venueCity, interactionCurrTeams, interactionVenueBatTeam, 
         Inn1EOIOvers, Inn1EOIRuns, Inn1EOIWkts, Inn2EOIOvers, Inn2EOIRuns, Inn2EOIWkts, 
         Over1Runs:Over40Wkts) %>%
  arrange(Season, Match_id)

#Finally write the new match summary data frame to a csv file so that we can do exploration and then create predictive model
#writing to current working directory which is defined in the variable filesDir
write.csv (matDetOverStats, "wrangled_matchSummaryDataIPL.csv")

  





