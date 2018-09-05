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
matSumm <- read.csv(file= 'Match.csv', header = TRUE, sep = ",")
matSumm <- matSumm[order(matSumm$match_id),]

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
  left_join(matSumm, by = c("Match_id" = "match_id")) %>%
    select(Team_Batting:cumWkts, venueGround = Venue_Name, venueCity = City_Name, winner = match_winner)

# Now add the batting and bowling team names with the lookup table team; Trouble with data "as is" is that 
# seasons 2008-2016 has numbers in the batting and bolwing team ids, but for season 2017, the actual team names are stored
# So do the lookup in 2 steps: first for the nueric ids (they actually are factor data types)

matDetNum <-matDet %>% mutate (newBatTeamId = as.character(Team_Batting), 
                               newBowlTeamId = as.character(Team_Bowling) ) %>%
  filter (newBatTeamId < "99") %>% mutate (Team_Id = as.numeric(newBatTeamId)) %>%
  left_join (teams, by = "Team_Id") %>% 
  select (TeamNameBat = Team_Name, Team_Batting:Ball_id, cumOver:winner, 
          Team_Bowling, newBowlTeamId) %>%
  mutate ( Team_Id = as.numeric(newBowlTeamId)) %>% left_join (teams, by = "Team_Id") %>%
  select(TeamNameBat:winner, TeamNameBowl = Team_Name, Team_Bowling)

matDetChar <- matDet %>% mutate (newTeamId = as.character (Team_Batting)) %>% filter (newTeamId > "99")
# IDs are already stored as names for Batting and Bowling teams for this set if rows in the original data file
matDetChar <- matDetChar %>% mutate (TeamNameBat = Team_Batting, TeamNameBowl = Team_Bowling) %>%
  select(TeamNameBat, Team_Batting:Ball_id, cumOver:winner, TeamNameBowl, Team_Bowling)
# Now combine the 2 data.frames back into 1, and then save in each row if TeamNameBat won the match:
matDet <- rbind(matDetNum, matDetChar) %>%
  mutate (TeamBattingWon = as.character(TeamNameBat == winner)) %>%
  arrange(TeamNameBat, Season, Match_id, Innings_No, Over_id, Ball_id)

# This completes the ball by ball data file preparation. Let's write it out for data exploration 
# writing to current working directory which is defined in the variable filesDir

#################################write.csv (matDet, "wrangled_ballByBallDataIPL.csv")

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
  mutate (Over30Runs = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Over30Runs) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Over30Runs) %>%
  distinct(Over30Runs) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet50Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 49) %>%
  mutate (Over50Runs = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Over50Runs) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Over50Runs) %>%
  distinct(Over50Runs) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet75Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 74) %>%
  mutate (Over75Runs = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Over75Runs) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Over75Runs) %>%
  distinct(Over75Runs) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet100Runs <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumRuns > 99) %>%
  mutate (Over100Runs = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Over100Runs) %>%
  filter(cumOver == min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, cumOver, Over100Runs) %>%
  distinct(Over100Runs) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....

matDet3Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 2) %>%
  mutate (Over3Wkts = min(cumOver)) %>% 
  group_by(Team_Batting, Season, Match_id, Innings_No, Over3Wkts) %>%
  distinct(Over3Wkts) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet4Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 3) %>%
  mutate (Over4Wkts = min(cumOver)) %>%
  group_by(Team_Batting, Season, Match_id, Innings_No, Over4Wkts) %>%
  distinct(Over4Wkts) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....
matDet5Wkts <- matDetHighlights %>% group_by(Team_Batting, Season, Match_id, Innings_No) %>%
  filter (cumWkts > 4) %>% 
  mutate (Over5Wkts = min(cumOver)) %>%
  group_by(Team_Batting, Season, Match_id, Innings_No, Over5Wkts) %>%
  distinct(Over5Wkts) # to eliminate extra balls in any over whch get lumped in as over 2.0 or 3.0 or 4.0....


# After creating these summaries at end of specific overs/milestones to be analyzed, keep only 1 row per match innings
# So keep only Over = 1 and Ball = 1 for each match innings, and add those over stats as columns 

matSumm <- matDet %>% filter (Over_id == 1 & Ball_id == 1) %>%
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
  select(TeamNameBat:Innings_No, venueGround:Over20RunRate) %>%
  arrange(TeamNameBat, Season, Match_id, Innings_No)

## Add the milestone run columns:
matSumm <- matSumm %>% left_join(matDet30Runs, 
                                 by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over20Wkts, Over30Runs) %>%
  left_join(matDet50Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over30Runs, Over50Runs) %>%
  left_join(matDet75Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over50Runs, Over75Runs) %>%
  left_join(matDet100Runs, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over75Runs, Over100Runs) %>%
  left_join(matDet3Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over100Runs, Over3Wkts) %>%
  left_join(matDet4Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over3Wkts, Over4Wkts = Over4Wkts.y)  %>%
  left_join(matDet5Wkts, 
            by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (TeamNameBat:Over4Wkts, Over5Wkts) 
  
#Finally write the new summary data frame to a csv file so that we can do exploration and then create predictive model
#writing to current working directory which is defined in the variable filesDir
write.csv (matSumm, "wrangled_matchSummaryDataIPL.csv")


# build new data frame for analysis from sorted data files for 2008
#matDet2008 <- subset(matDet, subset = Season == 2008)
