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
    select(Team_Batting:cumWkts, venueGround = Venue_Name, venueCity = City_Name, winner = match_winner)

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

# Now cull out only the rows pertaining to end of 6th (end of PowerPlay),10th, 15th and 20th overs to get the match situations (runs, wickets) at that stage
# Since an innings may end before the scheduled 20 overs (all out before that or crossed opponenet score before that), determine when End of Innings happens
# Also remember that not every over will end after 6 balls: extras can make an over go longer. Hence determine which was truly the last ball of an over
# Added summaries for 4th, 8th, 12th and 17th overs 

matDetEOI <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == max(Over_id)) %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = -99) %>% # -99 signifies end of an innings
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
  
matDet6Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 6)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 6) %>%
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker) # -At the end of over 6
matDet10Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 10)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 10) %>% # -At the end of over 10
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
matDet15Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 15)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 15) %>% # -At the end of over 15
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
matDet20Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 20)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 20) %>%  # -At the end of over 20
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
# Add more such columns for a match
matDet4Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 4)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 4) %>% # -At the end of over 4
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
matDet8Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 8)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 8)  %>% # -At the end of over 8
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
matDet12Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 12)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 12)  %>% # -At the end of over 12
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)
matDet17Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 17)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 17) %>% # -At the end of over 17  
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, cumRuns, cumWkts, inningsMarker)

#### Now determine when some significant milestones happen in an innings: in which over is the 30th run scored,
#### in which the 50th and then the 75th run scored, and finally the 100th run
#### And in which over we have 3 wickets down, then 4 wickets out, and then 5 wickets

matDetHighlights <- matDet %>% 
  select (Team_Batting, Season, Match_id, Innings_No,cumOver:cumWkts) %>%
  filter (cumRuns > 29 | cumWkts > 2) 
                        
matDet30Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 29) %>%
  mutate (Runs30InOver = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Runs30InOver) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Runs30InOver) %>%
  distinct(Runs30InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet50Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 49) %>%
  mutate (Runs50InOver = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Runs50InOver) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Runs50InOver) %>%
  distinct(Runs50InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet75Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 74) %>%
  mutate (Runs75InOver = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Runs75InOver) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Runs75InOver) %>%
  distinct(Runs75InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet100Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 99) %>%
  mutate (Runs100InOver = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Runs100InOver) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Runs100InOver) %>%
  distinct(Runs100InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....

matDet3Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 2) %>%
  mutate (Wkts3InOver = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Wkts3InOver) %>%
  distinct(Wkts3InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet4Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 3) %>%
  mutate (Wkts4InOver = min(cumOver)) %>%
  group_by(Team_Batting, Season, Match_id, Innings_No, Wkts4InOver) %>%
  distinct(Wkts4InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet5Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 4) %>% 
  mutate (Wkts5InOver = min(cumOver)) %>%
  group_by(Team_Batting, Season, Match_id, Innings_No, Wkts5InOver) %>%
  distinct(Wkts5InOver) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....


# After creating these summaries at end of specific overs/milestones to be analyzed, keep only 1 row per match innings
# So keep only Over = 1 and Ball = 1 for each match innings, and add those over stats as columns 

innSumm <- matDet %>% filter (Over_id == 1 & Ball_id == 1) %>%
  left_join(matDetEOI, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:TeamBattingWon,  
          EOIOver = Over_id.y, EOIBall = Ball_id.y, EOIRuns = cumRuns.y, EOIWkts = cumWkts.y) %>% 
  ## if an innings lasts less than 12 overs, I dont want it to be included in the analysis: innings too short
  ## unless 9 or more wickets are lost in that innings (that is a full innings, just that hey could not bat even 12 overs)
  filter (EOIOver >= 12 )  %>%  ## | EOIWkts > 8
  mutate (EOIRunRate = round(EOIRuns / EOIOver, 2)) %>%
  left_join(matDet4Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:EOIRunRate, 
          Over4 = Over_id, Over4Ball = Ball_id, Over4Runs = cumRuns, Over4Wkts = cumWkts) %>%
  mutate (Over4RunRate = round(Over4Runs / 4, 2)) %>%
  left_join(matDet6Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over4RunRate, 
          Over6 = Over_id, Over6Ball = Ball_id, Over6Runs = cumRuns, Over6Wkts = cumWkts) %>%
  mutate (Over6RunRate = round(Over6Runs / 6, 2)) %>%
  left_join(matDet8Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over6RunRate, 
          Over8 = Over_id, Over8Ball = Ball_id, Over8Runs = cumRuns, Over8Wkts = cumWkts) %>%
  mutate (Over8RunRate = round(Over8Runs / 8, 2)) %>%
  left_join(matDet10Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over8RunRate, 
          Over10 = Over_id, Over10Ball = Ball_id, Over10Runs = cumRuns, Over10Wkts = cumWkts) %>%
  mutate (Over10RunRate = round(Over10Runs / 10, 2)) %>%
  left_join(matDet12Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over10RunRate, 
          Over12 = Over_id, Over12Ball = Ball_id, Over12Runs = cumRuns, Over12Wkts = cumWkts) %>%
  mutate (Over12RunRate = round(Over12Runs / 12, 2)) %>%
  left_join(matDet15Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over12RunRate, 
          Over15 = Over_id, Over15Ball = Ball_id, Over15Runs = cumRuns, Over15Wkts = cumWkts) %>%
  mutate (Over15RunRate = round(Over15Runs / 15, 2)) %>%
  left_join(matDet17Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over15RunRate, 
          Over17 = Over_id, Over17Ball = Ball_id, Over17Runs = cumRuns, Over17Wkts = cumWkts) %>%
  mutate (Over17RunRate = round(Over17Runs / 17, 2)) %>%
  left_join(matDet20Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over17RunRate, 
          Over20 = Over_id, Over20Ball = Ball_id, Over20Runs = cumRuns, Over20Wkts = cumWkts) %>%
  mutate (Over20RunRate = round(Over20Runs / 20, 2)) %>%
  select(TeamNameBat:Innings_No, Team_Bowling, venueGround:Over20RunRate) %>%
  arrange(TeamNameBat, Season, Match_id, Innings_No)

## Compute a couple of mean values for interaction columns later on:
mean_Team_Batting <- mean(innSumm$Team_Batting)
mean_Team_Bowling <- mean(innSumm$Team_Bowling)
mean_venueCity <- mean(as.numeric(innSumm$venueCity))

## Add the milestone run columns:
innSumm <- innSumm %>% left_join(matDet30Runs, 
                                 by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over20Wkts, Runs30InOver) %>%
  left_join(matDet50Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Runs30InOver, Runs50InOver) %>%
  left_join(matDet75Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Runs50InOver, Runs75InOver) %>%
  left_join(matDet100Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Runs75InOver, Runs100InOver) %>%
  left_join(matDet3Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Runs100InOver, Wkts3InOver) %>%
  left_join(matDet4Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Wkts3InOver, Wkts4InOver)  %>%
  left_join(matDet5Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Wkts4InOver, Wkts5InOver) %>%
  ## Now add a couple of interaction columns for the prediction modeling later on:
  # a) How does one team score against its opponent (interaction betweene the opponenets in a match)
  # b) How does scoring for a team get impacted at different venues: interaction between batting team and venue
  mutate (interactionCurrTeams = (Team_Batting - mean_Team_Batting) * (Team_Bowling - mean_Team_Bowling),
          interactionVenueBatTeam = ((Team_Batting - mean_Team_Batting) * 
                                       (as.numeric(venueCity) - mean_venueCity)))
  
#Finally write the new summary data frame to a csv file so that we can do exploration and then create predictive model
#writing to current working directory which is defined in the variable filesDir
write.csv (innSumm, "wrangled_inningsSummaryDataIPL.csv")

## Now build dataset with each match as a data row so that we can predict EOI scores in each match at
## for the team batting 1st at the end of 5th, 10th and 15th overs (with increasing accuracy)
## and predict the macth winner starting in the 2nd team's batting innings at the end of
## the 5th, 10th and 15th overs (so for the match 20+5, 20 + 10, and 20 + 15 overs, assuming
## the team batting 1st bats all of its 20 overs)
## So we will get runs and wickets data in each row for each over: 1 through 15 which will
## be used in creating the prediction model for EOI score and match winner

##  mutate (!!(sym(paste("Over", as.character(cumOver), "Runs", sep =""))) := cumRuns)

matDetOverStats <- matDet %>% select (Season:cumWkts) %>%
  group_by(Season, Match_id, Innings_No, Over_id) %>%
  filter (Over_id %in% seq(1, to = 20, by =1 ) & Ball_id == max(Ball_id, na.rm = TRUE)) %>%
  ungroup() %>%  # ungrouping is necessary because Over_id as part of the group_by cannot be updated
  mutate (Over_id = ifelse (Innings_No == 2, Over_id + 20, Over_id)) %>% # 2nd Innings starts with the 21st over of the match
  mutate (cumOverRuns = paste ("Over", as.character(Over_id), "Runs", sep = ""),
          cumOverWkts = paste ("Over", as.character(Over_id), "Wkts", sep = ""),
          cumOverRR = paste ("Over", as.character(Over_id), "RR", sep = ""))
          ##    !!(varOverRuns) := ifelse (cumOver == 2 & Innings_No ==1, cumRuns, 0))
          
matDetOverStats <- spread(matDetOverStats, cumOverRuns, cumRuns)
matDetOverStats <- spread(matDetOverStats, cumOverWkts, cumWkts)
matDetOverStats <- spread(matDetOverStats, cumOverRR, runRate)
#matDetOverStats[is.na(matDetOverStats)] <- 0  # NA values can be safely replaced with 0 in this dataset for further computation

matDetOverStats <- matDetOverStats %>% select ( -cumOver, -Ball_id, -Team_Bowling) %>%
  group_by(Season, Match_id) %>%
  mutate (Over1Runs = max(Over1Runs,na.rm=TRUE), Over1Wkts = max(Over1Wkts,na.rm=TRUE), Over1RR = max(Over1RR,na.rm=TRUE),
          Over2Runs = max(Over2Runs,na.rm=TRUE), Over2Wkts = max(Over2Wkts,na.rm=TRUE), Over2RR = max(Over2RR,na.rm=TRUE),
          Over3Runs = max(Over3Runs,na.rm=TRUE), Over3Wkts = max(Over3Wkts,na.rm=TRUE), Over3RR = max(Over3RR,na.rm=TRUE),
          Over4Runs = max(Over4Runs,na.rm=TRUE), Over4Wkts = max(Over4Wkts,na.rm=TRUE), Over4RR = max(Over4RR,na.rm=TRUE),
          Over5Runs = max(Over5Runs,na.rm=TRUE), Over5Wkts = max(Over5Wkts,na.rm=TRUE), Over5RR = max(Over5RR,na.rm=TRUE),
          Over6Runs = max(Over6Runs,na.rm=TRUE), Over6Wkts = max(Over6Wkts,na.rm=TRUE), Over6RR = max(Over6RR,na.rm=TRUE),
          Over7Runs = max(Over7Runs,na.rm=TRUE), Over7Wkts = max(Over7Wkts,na.rm=TRUE), Over7RR = max(Over7RR,na.rm=TRUE),
          Over8Runs = max(Over8Runs,na.rm=TRUE), Over8Wkts = max(Over8Wkts,na.rm=TRUE), Over8RR = max(Over8RR,na.rm=TRUE),
          Over9Runs = max(Over9Runs,na.rm=TRUE), Over9Wkts = max(Over9Wkts,na.rm=TRUE), Over9RR = max(Over9RR,na.rm=TRUE),
          Over10Runs = max(Over10Runs,na.rm=TRUE), Over10Wkts = max(Over10Wkts,na.rm=TRUE), Over10RR = max(Over10RR,na.rm=TRUE),
          Over11Runs = max(Over11Runs,na.rm=TRUE), Over11Wkts = max(Over11Wkts,na.rm=TRUE), Over11RR = max(Over11RR,na.rm=TRUE),
          Over12Runs = max(Over12Runs,na.rm=TRUE), Over12Wkts = max(Over12Wkts,na.rm=TRUE), Over12RR = max(Over12RR,na.rm=TRUE),
          Over13Runs = max(Over13Runs,na.rm=TRUE), Over13Wkts = max(Over13Wkts,na.rm=TRUE), Over13RR = max(Over13RR,na.rm=TRUE),
          Over14Runs = max(Over14Runs,na.rm=TRUE), Over14Wkts = max(Over14Wkts,na.rm=TRUE), Over14RR = max(Over14RR,na.rm=TRUE),
          Over15Runs = max(Over15Runs,na.rm=TRUE), Over15Wkts = max(Over15Wkts,na.rm=TRUE), Over15RR = max(Over15RR,na.rm=TRUE),
          Over16Runs = max(Over16Runs,na.rm=TRUE), Over16Wkts = max(Over16Wkts,na.rm=TRUE), Over16RR = max(Over16RR,na.rm=TRUE),
          Over17Runs = max(Over17Runs,na.rm=TRUE), Over17Wkts = max(Over17Wkts,na.rm=TRUE), Over17RR = max(Over17RR,na.rm=TRUE),
          Over18Runs = max(Over18Runs,na.rm=TRUE), Over18Wkts = max(Over18Wkts,na.rm=TRUE), Over18RR = max(Over18RR,na.rm=TRUE),
          Over19Runs = max(Over19Runs,na.rm=TRUE), Over19Wkts = max(Over19Wkts,na.rm=TRUE), Over19RR = max(Over19RR,na.rm=TRUE),
          Over20Runs = max(Over20Runs,na.rm=TRUE), Over20Wkts = max(Over20Wkts,na.rm=TRUE), Over20RR = max(Over20RR,na.rm=TRUE),
          Over21Runs = max(Over21Runs,na.rm=TRUE), Over21Wkts = max(Over21Wkts,na.rm=TRUE), Over21RR = max(Over21RR,na.rm=TRUE),
          Over22Runs = max(Over22Runs,na.rm=TRUE), Over22Wkts = max(Over22Wkts,na.rm=TRUE), Over22RR = max(Over22RR,na.rm=TRUE),
          Over23Runs = max(Over23Runs,na.rm=TRUE), Over23Wkts = max(Over23Wkts,na.rm=TRUE), Over23RR = max(Over23RR,na.rm=TRUE),
          Over24Runs = max(Over24Runs,na.rm=TRUE), Over24Wkts = max(Over24Wkts,na.rm=TRUE), Over24RR = max(Over24RR,na.rm=TRUE),
          Over25Runs = max(Over25Runs,na.rm=TRUE), Over25Wkts = max(Over25Wkts,na.rm=TRUE), Over25RR = max(Over25RR,na.rm=TRUE),
          Over26Runs = max(Over26Runs,na.rm=TRUE), Over26Wkts = max(Over26Wkts,na.rm=TRUE), Over26RR = max(Over26RR,na.rm=TRUE),
          Over27Runs = max(Over27Runs,na.rm=TRUE), Over27Wkts = max(Over27Wkts,na.rm=TRUE), Over27RR = max(Over27RR,na.rm=TRUE),
          Over28Runs = max(Over28Runs,na.rm=TRUE), Over28Wkts = max(Over28Wkts,na.rm=TRUE), Over28RR = max(Over28RR,na.rm=TRUE),
          Over29Runs = max(Over29Runs,na.rm=TRUE), Over29Wkts = max(Over29Wkts,na.rm=TRUE), Over29RR = max(Over29RR,na.rm=TRUE),
          Over30Runs = max(Over30Runs,na.rm=TRUE), Over30Wkts = max(Over30Wkts,na.rm=TRUE), Over30RR = max(Over30RR,na.rm=TRUE),
          Over31Runs = max(Over31Runs,na.rm=TRUE), Over31Wkts = max(Over31Wkts,na.rm=TRUE), Over31RR = max(Over31RR,na.rm=TRUE),
          Over32Runs = max(Over32Runs,na.rm=TRUE), Over32Wkts = max(Over32Wkts,na.rm=TRUE), Over32RR = max(Over32RR,na.rm=TRUE),
          Over33Runs = max(Over33Runs,na.rm=TRUE), Over33Wkts = max(Over33Wkts,na.rm=TRUE), Over33RR = max(Over33RR,na.rm=TRUE),
          Over34Runs = max(Over34Runs,na.rm=TRUE), Over34Wkts = max(Over34Wkts,na.rm=TRUE), Over34RR = max(Over34RR,na.rm=TRUE),
          Over35Runs = max(Over35Runs,na.rm=TRUE), Over35Wkts = max(Over35Wkts,na.rm=TRUE), Over35RR = max(Over35RR,na.rm=TRUE),
          Over36Runs = max(Over36Runs,na.rm=TRUE), Over36Wkts = max(Over36Wkts,na.rm=TRUE), Over36RR = max(Over36RR,na.rm=TRUE),
          Over37Runs = max(Over37Runs,na.rm=TRUE), Over37Wkts = max(Over37Wkts,na.rm=TRUE), Over37RR = max(Over37RR,na.rm=TRUE),
          Over38Runs = max(Over38Runs,na.rm=TRUE), Over38Wkts = max(Over38Wkts,na.rm=TRUE), Over38RR = max(Over38RR,na.rm=TRUE),
          Over39Runs = max(Over39Runs,na.rm=TRUE), Over39Wkts = max(Over39Wkts,na.rm=TRUE), Over39RR = max(Over39RR,na.rm=TRUE),
          Over40Runs = max(Over40Runs,na.rm=TRUE), Over40Wkts = max(Over40Wkts,na.rm=TRUE), Over40RR = max(Over40RR,na.rm=TRUE)
          ) %>%  ## filter to keep only 1 row per match innings, since all the important row info is now in columns
  group_by(Season, Match_id, Innings_No) %>%
  filter (Over_id == min(Over_id)) %>%
  left_join(innSumm, by = c("Season", "Match_id", "Innings_No")) %>% 
  mutate (EOIInnOvers = paste ("Inn", as.character(Innings_No), "EOIOvers", sep = ""),
          EOIInnRuns = paste ("Inn", as.character(Innings_No), "EOIRuns", sep = ""),
          EOIInnWkts = paste ("Inn", as.character(Innings_No), "EOIWkts", sep = ""),
          EOIInnRR = paste ("Inn", as.character(Innings_No), "EOIRR", sep = "") ) %>%
  select (Season, Match_id, Innings_No, Over_id, BatFirst = TeamNameBat, BatSecond = TeamNameBowl, winner,
          TeamBattingFirstWon = TeamBattingWon, venueGround, venueCity, 
          EOIInnOvers:EOIInnRR, EOIOver:EOIRunRate, 
          interactionCurrTeams, interactionVenueBatTeam,
          Over1Runs, Over1Wkts, Over1RR, Over2Runs, Over2Wkts, Over2RR, Over3Runs, Over3Wkts, Over3RR,
          Over4Runs = Over4Runs.x, Over4Wkts = Over4Wkts.x, Over4RR, 
          Over5Runs, Over5Wkts, Over5RR,
          Over6Runs = Over6Runs.x, Over6Wkts = Over6Wkts.x, Over6RR,
          Over7Runs, Over7Wkts, Over7RR,
          Over8Runs = Over8Runs.x, Over8Wkts = Over8Wkts.x, Over8RR,
          Over9Runs, Over9Wkts, Over9RR,
          Over10Runs = Over10Runs.x, Over10Wkts = Over10Wkts.x, Over10RR,
          Over11Runs, Over11Wkts, Over11RR,
          Over12Runs = Over12Runs.x, Over12Wkts = Over12Wkts.x, Over12RR,
          Over13Runs, Over13Wkts, Over13RR, Over14Runs, Over14Wkts, Over14RR,
          Over15Runs = Over15Runs.x, Over15Wkts = Over15Wkts.x, Over15RR,
          Over16Runs, Over16Wkts, Over16RR,
          Over17Runs = Over17Runs.x, Over17Wkts = Over17Wkts.x, Over17RR,
          Over18Runs, Over18Wkts, Over18RR, Over19Runs, Over19Wkts, Over19RR,
          Over20Runs = Over20Runs.x, Over20Wkts = Over20Wkts.x, Over20RR,
          Over21Runs, Over21Wkts, Over21RR, Over22Runs, Over22Wkts, Over22RR, Over23Runs, Over23Wkts, Over23RR,
          Over24Runs, Over24Wkts, Over24RR, Over25Runs, Over25Wkts, Over25RR, Over26Runs, Over26Wkts, Over26RR,
          Over27Runs, Over27Wkts, Over27RR, Over28Runs, Over28Wkts, Over28RR, Over29Runs, Over29Wkts, Over29RR,
          Over30Runs, Over30Wkts, Over30RR, Over31Runs, Over31Wkts, Over31RR, Over32Runs, Over32Wkts, Over32RR,
          Over33Runs, Over33Wkts, Over33RR, Over34Runs, Over34Wkts, Over34RR, Over35Runs, Over35Wkts, Over35RR,
          Over36Runs, Over36Wkts, Over36RR, Over37Runs, Over37Wkts, Over37RR, Over38Runs, Over38Wkts, Over38RR,
          Over39Runs, Over39Wkts, Over39RR, Over40Runs, Over40Wkts, Over40RR )

matDetOverStats <- spread(matDetOverStats, EOIInnOvers, EOIOver)
matDetOverStats <- spread(matDetOverStats, EOIInnRuns, EOIRuns)
matDetOverStats <- spread(matDetOverStats, EOIInnWkts, EOIWkts)
matDetOverStats <- spread(matDetOverStats, EOIInnRR, EOIRunRate)

matDetOverStats <- matDetOverStats %>% 
  group_by(Season, Match_id) %>%
  mutate (Inn1EOIOvers = max(Inn1EOIOvers,na.rm=TRUE), Inn2EOIOvers = max(Inn2EOIOvers, na.rm = TRUE),
          Inn1EOIRuns = max(Inn1EOIRuns,na.rm=TRUE), Inn2EOIRuns = max(Inn2EOIRuns, na.rm = TRUE), 
          Inn1EOIWkts = max(Inn1EOIWkts,na.rm=TRUE), Inn2EOIWkts = max(Inn2EOIWkts, na.rm = TRUE),
          Inn1EOIRR = max(Inn1EOIRR,na.rm=TRUE), Inn2EOIRR = max(Inn2EOIRR, na.rm = TRUE) ) %>%
  filter(Innings_No == 1) %>%   ## Keep only 1 row per match; each row is now self-contained for a match
  select(Season, Match_id, BatFirst:venueCity, interactionCurrTeams, interactionVenueBatTeam, 
         Inn1EOIOvers:Inn2EOIRR, Over1Runs:Over40RR) %>%
  arrange(Season, Match_id)

#Finally write the new match summary data frame to a csv file so that we can do exploration and then create predictive model
#writing to current working directory which is defined in the variable filesDir
write.csv (matDetOverStats, "wrangled_matchSummaryDataIPL.csv")

  





