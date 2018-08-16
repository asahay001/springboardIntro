# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")


library ("tidyverse")
library("dplyr")



# Set files dirctory from where to read data (csv files). 

filesDir <- "C:\\work\\dataScience\\springboard"


# Read Team.csv which has Team ID and Team Names. There are 13 teams that have played from 2008-2017 in IPL T20 tournament
#teamss$Team_Name will have the names of the teams, while teams$Team_Id will have the IDs for cross-referencing across files
teams <- read.csv(file= 'C:\\work\\dataScience\\springboard\\Team.csv', header = TRUE, sep = ",")

# Read Match.csv which has summary of each match played: opponents, venue, runs scored by each team, match result
matSumm <- read.csv(file= 'C:\\work\\dataScience\\springboard\\sorted_Match.csv', header = TRUE, sep = ",")
matSumm <- matSumm[order(matSumm$match_id),]

# Finally read the details of each match from Ball_By_Ball.csv
matDetFile <- "C:\\work\\dataScience\\springboard\\small_Ball_By_Ball.csv"
matDet <- read.csv(file = matDetFile, header = TRUE, sep = ",")


# select only the needed columns, then sort, then find cumulative runs scored after each ball, and then further reduce columns no longer needed
matDet <- matDet %>% select (Team_Batting, Season, Match_id = MatcH_id, Innings_No, Over_id, Ball_id, 
                             Runs_Scored, Extra_runs, Bowler_Wicket, Run_out, Team_Bowling)  %>%
  arrange (Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id) %>%
  group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  mutate (cumRuns = cumsum(Runs_Scored + Extra_runs), cumWkts = cumsum(Bowler_Wicket + Run_out)) %>%
  select (-c(Runs_Scored, Extra_runs, Bowler_Wicket, Run_out))  # Discard columns not needed anymore since cumRuns and cumWkts are built

# Now cull out only the rows pertaining to end of 5th,10th, 15th and 20th overs to get the match situations (runs, wickets) at that stage
# Since an innings may end before the scheduled 20 overs (all out before that or crossed opponenet score before that), determine when End of Innings happens
# Also remember that not every over will end after 6 balls: extras can make an over go longer. Hence determine which was truly the last ball of an over

matDetEOI <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == max(Over_id)) %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = -99)  # -99 signifies end of an innings
  
matDet5Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 5)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 5)  # -At the end of over 5
matDet10Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 10)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 10)  # -At the end of over 10
matDet15Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 15)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 15)  # -At the end of over 15
matDet20Over <- matDet %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>%
  filter (Over_id == 20)   %>% group_by (Team_Batting, Season, Match_id, Innings_No) %>% # need to group by again since I cant get it work in one group by
  filter (Ball_id == max(Ball_id)) %>% mutate (inningsMarker = 20)  # -At the end of over 20

#Now combine the data frames from EOI, 5th, 10th, 15th and 20th overs to get consolidated data frame for analysis
matDet <- rbind(matDet5Over, matDet10Over, matDet15Over, matDet20Over, matDetEOI) %>% 
  arrange (Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id)  # Go back to the original sorting by team, year and match

# Now add the team name with the lookup table team
##matDet <- matDet %>% left_join (teams, by = c("Team_Batting" = "Team_Id"))

matDetNum <- matDet %>% filter(as.numeric(Team_Batting) < 20)
matDetNum <- matDetNum %>% left_join (teams, by = c(as.numeric("Team_Batting") = "Team_Id"))

matDet <- matDet %>% left_join(matSumm, by = c("Match_id" = "match_id")) %>%
  select(Team_Batting, Season, Match_id, Innings_No, Over_id, Ball_id, Team_Bowling, cumRuns, cumWkts, inningsMarker, Venue_Name, match_winner)



















# build new data frame for analysis from sorted data files for 2008
matDet2008 <- subset(matDet, subset = Season == 2008)

# initialize variables
matchID <- 0
prev_matchID <- matchID
inningsNum <- 0
prev_inningsNum <- inningsNum
overNum <- 1
prev_overNum <- overNum
runsOnThisBall <- 0
wktsOnThisBall <- 0
wktsInOvers1To5 <- 0
wktsInOvers6To10 <- 0
wktsInOvers11To15 <- 0
wktsAtEndOfInnings <- 0
runsInOvers1To5 <- 0
runsInOvers6To10 <- 0
runsInOvers11To15 <- 0
runsInOvers16To20 <- 0
runsAtEndOfInnings <- 0
overNum <- 0  # current over being read
ballNum <- 0  # current ball being read
overGroup <- 0    # 5 for Overs 1-5, or 10 for overs 6-10, or 15 for overs 10-15, or 20 for overs 16-20
oversBatted <- 0    # Total number of overs faced by a team in a match
teamName <- ""
season <- 0
venue <- ""
opponentTeam <- ""
batFirst <- TRUE


"  # comment out everything that was following the read line by line logic

teamPerf08_Df <- data.frame(teamName, season, matchID, venue, opponentTeam, batFirst, oversBatted, runsAtEndOfInnings, 
                          runsInOvers1To5, wktsInOvers1To5, runsInOvers6To10, wktsInOvers6To10, 
                          runsInOvers11To15, wktsInOvers11To15, runsInOver16To20, wktsAtEndOfInnings) 

for (rowD in 1:nrow(matDet2008)){
    matchID <- matDet2008[rowD, 1]
    inningsNum <- matDet2008[rowD, 4]
   
  if (rowD == 1) { # 1st row being read from the data file; initialize
    prev_matchID <- matchID
    prev_inningsNum <- inningsNum
    
  }

  if (matchID == prev_matchID) {     # Data for the same match and same innings
    inningsNum <- inningsNum <- matDet2008[rowD, 4]
    
    if (inningsNum == prev_inningsNum) { # same innings within a match
      runsOnThisBall <- sum(matDet2008[rowD, 9], matDet2008[rowD, 10])
      wktsOnThisBall <- matDet2008[rowD, 20] + matDet2008[rowD, 27]
      ##overNum <- matDet2008[rowD, matDet2008$Over_id]
      overNum <- matDet2008[rowD, 2]
      ballNum <- matDet2008[rowD, 3]
      #ballNum <- matDet2008[rowD, matDet2008$Ball_id]
      if (overNum < 6) {
        overGroup <- 5
        runsInOvers1To5 <- runsInOvers1To5 + runsOnThisBall
        wktsInOvers1To5 <- wktsInOvers1To5 + wktsOnThisBall
      } else if (overNum > 5 & overNum < 11) {
        overGroup <- 10
        runsInOvers6To10 <- runsInOvers6To10 + runsOnThisBall
        wktsInOvers6To10 <- wktsInOvers6To10 + wktsOnThisBall
      } else if (overNum > 10 & overNum < 16) {
        overGroup <- 15
        runsInOvers11To15 <- runsInOvers11To15 + runsOnThisBall
        wktsInOvers11To15 <- wktsInOvers11To15 + wktsOnThisBall
      } else {
        overGroup <- 20
        runsInOvers16To20 <- runsInOvers16To20 + runsOnThisBall
        wktsInOvers16To20 <- wktsInOvers16To20 + wktsOnThisBall
      }
      prev_inningsNum <- inningsNum
      oversBatted <- overNum + (ballNum / 10)
      
      }   # end of an inings within a match
    # write out data about the innings
    
    }     # end of a match
}         # finished reading the ball by ball detail data 
" # end of large commented out portion which was reading line by line
