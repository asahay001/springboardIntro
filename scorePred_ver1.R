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
  mutate (cumRuns = cumsum(Runs_Scored + Extra_runs), cumWkts = cumsum(Bowler_Wicket + Run_out)) %>%
  select (-c(Runs_Scored, Extra_runs, Bowler_Wicket, Run_out))  # Discard columns not needed anymore since cumRuns and cumWkts are built

# Now cull out only the rows pertaining to end of 6th (end of PowerPlay),10th, 15th and 20th overs to get the match situations (runs, wickets) at that stage
# Since an innings may end before the scheduled 20 overs (all out before that or crossed opponenet score before that), determine when End of Innings happens
# Also remember that not every over will end after 6 balls: extras can make an over go longer. Hence determine which was truly the last ball of an over

matDetEOI <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == max(Over_id)) %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = -99)  # -99 signifies end of an innings
  
matDet6Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 6)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 6)  # -At the end of over 6
matDet10Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 10)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 10)  # -At the end of over 10
matDet15Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 15)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 15)  # -At the end of over 15
matDet20Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 20)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 20)  # -At the end of over 20

matDet <- matDet %>% left_join(matDetEOI, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (Team_Batting, Season, Match_id, Innings_No, Over_id = Over_id.x, Ball_id = Ball_id.x, 
          Team_Bowling = Team_Bowling.x, cumRuns = cumRuns.x, cumWkts = cumWkts.x, 
          EOIOver = Over_id.y, EOIBall = Ball_id.y, EOIRuns = cumRuns.y, EOIWkts = cumWkts.y) %>% 
  left_join(matDet6Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (Team_Batting, Season, Match_id, Innings_No, Over_id = Over_id.x, Ball_id = Ball_id.x, 
          Team_Bowling = Team_Bowling.x, cumRuns = cumRuns.x, cumWkts = cumWkts.x, 
          EOIOver, EOIBall, EOIRuns, EOIWkts, 
          Over6 = Over_id.y, Over6Ball = Ball_id.y, Over6Runs = cumRuns.y, Over6Wkts = cumWkts.y) %>%
  left_join(matDet10Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (Team_Batting, Season, Match_id, Innings_No, Over_id = Over_id.x, Ball_id = Ball_id.x, 
          Team_Bowling = Team_Bowling.x, cumRuns = cumRuns.x, cumWkts = cumWkts.x, 
          EOIOver, EOIBall, EOIRuns, EOIWkts, Over6, Over6Ball, Over6Runs, Over6Wkts, 
          Over10 = Over_id.y, Over10Ball = Ball_id.y, Over10Runs = cumRuns.y, Over10Wkts = cumWkts.y) %>%
  left_join(matDet15Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (Team_Batting, Season, Match_id, Innings_No, Over_id = Over_id.x, Ball_id = Ball_id.x, 
          Team_Bowling = Team_Bowling.x, cumRuns = cumRuns.x, cumWkts = cumWkts.x, 
          EOIOver, EOIBall, EOIRuns, EOIWkts, Over6, Over6Ball, Over6Runs, Over6Wkts, 
          Over10, Over10Ball, Over10Runs, Over10Wkts,
          Over15 = Over_id.y, Over15Ball = Ball_id.y, Over15Runs = cumRuns.y, Over15Wkts = cumWkts.y) %>%
  left_join(matDet20Over, by = c("Team_Batting", "Season", "Match_id", "Innings_No")) %>%
  select (Team_Batting, Season, Match_id, Innings_No, Over_id = Over_id.x, Ball_id = Ball_id.x, 
          Team_Bowling = Team_Bowling.x, cumRuns = cumRuns.x, cumWkts = cumWkts.x, 
          EOIOver, EOIBall, EOIRuns, EOIWkts, Over6, Over6Ball, Over6Runs, Over6Wkts, 
          Over10, Over10Ball, Over10Runs, Over10Wkts, Over15, Over15Ball, Over15Runs, Over15Wkts,
          Over20 = Over_id.y, Over20Ball = Ball_id.y, Over20Runs = cumRuns.y, Over20Wkts = cumWkts.y) %>%
  left_join(matSumm, by = c("Match_id" = "match_id")) %>%
  select(Team_Batting:Over20Wkts, venueGround = Venue_Name, venueCity = City_Name, winner = match_winner)

# Now add the batting and bowling team names with the lookup table team; Trouble with data "as is" is that 
# seasons 2008-2016 has numbers in the batting and bolwing team ids, but for season 2017, the actual team names are stored
# So do the lookup in 2 steps: first for the nueric ids (they actually are factor data types)

matDetNum <-matDet %>% mutate (newBatTeamId = as.character(Team_Batting), newBowlTeamId = as.character(Team_Bowling) ) %>%
  filter (newBatTeamId < "99") %>% mutate (Team_Id = as.numeric(newBatTeamId)) %>%
  left_join (teams, by = "Team_Id") %>% 
  select (TeamName_Bat = Team_Name, Team_Batting:Ball_id, cumRuns:Over20Wkts, 
          venueGround:winner, Team_Bowling, newBowlTeamId) %>%
  mutate ( Team_Id = as.numeric(newBowlTeamId)) %>% left_join (teams, by = "Team_Id") %>%
  select(TeamName_Bat:winner, TeamName_Bowl = Team_Name, Team_Bowling)

matDetChar <- matDet %>% mutate (newTeamId = as.character (Team_Batting)) %>% filter (newTeamId > "99")
# IDs are already stored as names for Batting and Bowling teams for this set if rows in the original data file
matDetChar <- matDetChar %>% mutate (TeamName_Bat = Team_Batting, TeamName_Bowl = Team_Bowling) %>%
  select(TeamName_Bat, Team_Batting:Ball_id, cumRuns:winner, TeamName_Bowl, Team_Bowling)
# Now combine the 2 data.frames back into 1:
matDet <- rbind(matDetNum, matDetChar) %>%
  arrange(TeamName_Bat, Season, Match_id, Innings_No, Over_id, Ball_id)

#Finally write the new data frame to a csv file so that we can do exploration and then create predictive model
#writing to current working directory which is defined in teh variable filesDir
write.csv (matDet, "wrangled_ballByBallData.csv")
  

# build new data frame for analysis from sorted data files for 2008
#matDet2008 <- subset(matDet, subset = Season == 2008)
