# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")

library ("tidyverse")
library("dplyr")

    # Set files dirctory from where to read data (csv files). 

filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
setwd(filesDir)

    # Read *Match.csv which has summary of each match played: opponents, venue, match result
matSumm2018 <- read.csv(file= 'ipl2018Match_raw.csv', header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
matSumm2018 <- matSumm2018[order(matSumm2018$id),]
    # select only the  matches from season 2018 from both files;
    # also select only needed columns:
matSumm2018 <- matSumm2018 %>% filter (between (season, 2018, 2018) ) %>% # only want 2018 matches
  select(Season = season, Match_id = id, BatFirst = team1, BatSecond = team2,
         toss = toss_winner, winner = winner, 
         #TeamBattingWon = as.character(BatFirst) == as.character(winner),
         venueGround = venue, venueCity = city) %>%
  mutate (TeamBattingFirstWon = (BatFirst == winner)
          ) 
    
# Read the details of each match, by delivery (ball-by-ball): 
matDet2018 <- read.csv(file = 'ipl2018deliveries_raw.csv', header = TRUE, sep = ",",
                       stringsAsFactors = FALSE)
matDet2018 <- matDet2018[order(matDet2018$match_id),]
    # Now work with matDet2018 to filter rows to only these 4 knockout matches, and also to 
    # get cumulative totals at the end of each over
matDet2018 <- matDet2018 %>% filter (inning <= 2) %>%   # Only Innings 1 and 2 (not tie-breaker innings)
  select(Match_id = match_id, inning, over, ball, total_runs, dismissal_kind) %>%
  inner_join(matSumm2018, by = c("Match_id" = "Match_id")) %>%
  select (Season, Match_id, BatFirst, BatSecond, toss, winner, TeamBattingFirstWon,
          venueGround, venueCity, 
          inning, over, ball, total_runs, dismissal_kind) %>%
  group_by(Match_id, inning) %>%  # Add up the wickets lost by over
  mutate (cumOver = (over - 1) + 
            round(ifelse (ball > 6, 0.0, (ball %% 6) / 6), 2) +   # If an over had extra balls, count them as 6 balls
            ifelse(ball >= 6, ball%/% 6, 0 ), 
          wkts = ifelse(as.character(dismissal_kind) == "", 0, 1) ) %>%  # wkts is dediced from dismissal_kind factor
  mutate (cumRuns = cumsum(total_runs), 
          cumWkts = cumsum(wkts)) %>% group_by (Match_id, inning, over) %>%
  filter (over %in% seq(1, to = 20, by =1 ) & ball == max(ball, na.rm = TRUE)) %>%
  mutate (EOIInnOvers = paste ("Inn", as.character(inning), "EOIOvers", sep = ""),
        EOIInnRuns = paste ("Inn", as.character(inning), "EOIRuns", sep = ""),
        EOIInnWkts = paste ("Inn", as.character(inning), "EOIWkts", sep = ""),
        cumOver1 = cumOver, cumRuns1 = cumRuns, cumWkts1 = cumWkts)
matDet2018 <- spread(matDet2018, EOIInnOvers, cumOver1)
matDet2018 <- spread(matDet2018, EOIInnRuns, cumRuns1)
matDet2018 <- spread(matDet2018, EOIInnWkts, cumWkts1)
matDet2018 <- matDet2018 %>% 
  group_by(Match_id) %>%
  mutate (Inn1EOIOvers = max(Inn1EOIOvers,na.rm=TRUE), Inn2EOIOvers = max(Inn2EOIOvers, na.rm = TRUE),
          Inn1EOIRuns = max(Inn1EOIRuns,na.rm=TRUE), Inn2EOIRuns = max(Inn2EOIRuns, na.rm = TRUE), 
          Inn1EOIWkts = max(Inn1EOIWkts,na.rm=TRUE), Inn2EOIWkts = max(Inn2EOIWkts, na.rm = TRUE)
  ) %>%
  mutate (over = ifelse (inning == 2, over + 20, over)) %>% # 2nd Innings starts with the 21st over of the match
  mutate (cumOverRuns = paste ("Over", as.character(over), "Runs", sep = ""),
          cumOverWkts = paste ("Over", as.character(over), "Wkts", sep = "")
  )
              ##    !!(varOverRuns) := ifelse (cumOver == 2 & Innings_No ==1, cumRuns, 0))
matDet2018 <- spread(matDet2018, cumOverRuns, cumRuns)
matDet2018 <- spread(matDet2018, cumOverWkts, cumWkts)
            #matDet2018[is.na(matDet2018)] <- 0  # NA values can be safely replaced with 0 in this dataset for further computation
matDet2018 <- matDet2018 %>% 
  select ( -ball, -total_runs, - dismissal_kind, -cumOver, -wkts) %>%
  group_by(Match_id) %>%
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
          #, BatFirstWonLastMat = 0, BatSecondWonLastMat = 0, 
          #BatFirstWinsInLast3Mat = 0, BatSecondWinsInLast3Mat = 0,
          #BatFirstWinsInLast5Mat = 0, BatSecondWinsInLast5Mat = 0
  ) %>% 
  group_by(Match_id) %>% filter (over == min(over)) %>%
  select (Season, Match_id, BatFirst, BatSecond, toss, winner, TeamBattingFirstWon,
          venueGround, venueCity, 
          Inn1EOIOvers, Inn1EOIRuns, Inn1EOIWkts, Inn2EOIOvers, Inn2EOIRuns, Inn2EOIWkts, 
          #BatFirstWonLastMat:BatSecondWinsInLast5Mat,
          Over1Runs, Over1Wkts,  Over2Runs, Over2Wkts,  Over3Runs, Over3Wkts, 
          Over4Runs, Over4Wkts, Over5Runs, Over5Wkts, Over6Runs, Over6Wkts, 
          Over7Runs, Over7Wkts, Over8Runs, Over8Wkts, Over9Runs, Over9Wkts, 
          Over10Runs, Over10Wkts, Over11Runs, Over11Wkts, Over12Runs, Over12Wkts, 
          Over13Runs, Over13Wkts,  Over14Runs, Over14Wkts, Over15Runs, Over15Wkts, 
          Over16Runs, Over16Wkts, Over17Runs, Over17Wkts, Over18Runs, Over18Wkts,  
          Over19Runs, Over19Wkts, Over20Runs, Over20Wkts, Over21Runs, Over21Wkts,  
          Over22Runs, Over22Wkts,  Over23Runs, Over23Wkts, Over24Runs, Over24Wkts,  
          Over25Runs, Over25Wkts,  Over26Runs, Over26Wkts, Over27Runs, Over27Wkts,  
          Over28Runs, Over28Wkts,  Over29Runs, Over29Wkts, Over30Runs, Over30Wkts,  
          Over31Runs, Over31Wkts,  Over32Runs, Over32Wkts, Over33Runs, Over33Wkts,  
          Over34Runs, Over34Wkts,  Over35Runs, Over35Wkts, Over36Runs, Over36Wkts,  
          Over37Runs, Over37Wkts, Over38Runs, Over38Wkts, Over39Runs, Over39Wkts, 
          Over40Runs, Over40Wkts ) %>%
  arrange (Season, Match_id)

# Let's clean out some data: 
# data.frame (matDet2018 %>% group_by(venueCity) %>% tally())
# Bangalore and Bengaluru are really the same cities, with the same stadium/ground, 
# and Mohali is represented as Chandigarh in the Training data:
matDet2018$venueCity <- ifelse(matDet2018$venueCity == "Bengaluru", 
                                    "Bangalore", matDet2018$venueCity)
matDet2018$venueCity <- ifelse(matDet2018$venueCity == "Hyderabad (Deccan)", 
                                    "Hyderabad", matDet2018$venueCity)
matDet2018$venueCity <- ifelse(matDet2018$venueCity == "Mohali", 
                                    "Chandigarh", matDet2018$venueCity)

# Add a few more columns for Feature Engineering, which may help in better prediction models:
# First, update with recent winners between 2 teams from the most recent matches (1, 3 and 5):
#matDet2018 = updateRecentWinsInDataSet_fn (matDet2018)
# Note these values wil be the same as in the complete Training dataset
# So read the Training dataset first:
write.csv (matDet2018, "wrangled_matchSummaryDataIPL2018only.csv") # want to read as char, not factors
matSummDet <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, 
                       sep = "," , stringsAsFactors = FALSE)

matDet2018 <- read.csv(file= "wrangled_matchSummaryDataIPL2018only.csv", header = TRUE, 
                       sep = "," , stringsAsFactors = FALSE)
matSummDet <- matSummDet %>%   #Using matSummDet as a teamporary workfile for relevant columns only
  select(Match_id, BatFirst, BatSecond, BatFirstWonLastMat, BatSecondWonLastMat, 
         BatFirstWinsInLast3Mat, BatSecondWinsInLast3Mat, BatFirstWinsInLast5Mat, 
         BatSecondWinsInLast5Mat, BatFirstMatchesPlayed, BatFirstMatchesWon, 
         BatFirstWinPercentage, BatSecondMatchesPlayed, BatSecondMatchesWon,
         BatSecondWinPercentage) %>%
  group_by (BatFirst, BatSecond) %>% 
  filter(Match_id == max(Match_id))
matDet2018 <- matDet2018 %>% 
  left_join(matSummDet, by = c("BatFirst", "BatSecond")) %>%
  select(Season, Match_id = Match_id.x, BatFirst:venueCity, 
         BatFirstMatchesPlayed:BatSecondWinPercentage,
         Inn1EOIOvers, Inn1EOIRuns, Inn1EOIWkts, Inn2EOIOvers, Inn2EOIRuns, Inn2EOIWkts, 
         BatFirstWonLastMat:BatSecondWinsInLast5Mat,
         Over1Runs:Over40Wkts)
  

write.csv (matDet2018, "wrangled_matchSummaryDataIPL2018only.csv")
  
  
  
