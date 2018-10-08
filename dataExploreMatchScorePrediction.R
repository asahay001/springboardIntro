
# set env
# install.packages ("dplyr")
# install.packages ("tidyverse")

# Set files dirctory from where to read data (csv files). 
filesDir <- "C:\\work\\dataScience\\springboard\\springboardIntro"
#setwd(filesDir)

library ("tidyverse")
library("dplyr")
library("ggplot2")

# Read the wrangled match summary csv files previously prepared by scorePred_ver1.R
matSumm <- read.csv(file= "wrangled_matchSummaryDataIPL.csv", header = TRUE, sep = ",",
                    stringsAsFactors = FALSE)
matSumm$toss <- ifelse(matSumm$toss == "Rising Pune Supergiant", "Rising Pune Supergiants",
       matSumm$toss)

# Create stats for matches played by each team and win percentage of the matches played:
batFirstCount <- matSumm %>% group_by(BatFirst) %>% count(BatFirst)
batSecondCount <- matSumm %>% group_by(BatSecond) %>% count(BatSecond)
playedCount <- batFirstCount %>% inner_join(batSecondCount, by = c("BatFirst" = "BatSecond")) %>%
  mutate(matchesPlayed = n.x + n.y) %>% select(team = BatFirst, matchesPlayed)
matchesWonStat <- matSumm %>% select (winner) %>% group_by(winner) %>% count(winner) %>%
  inner_join(playedCount, by = c("winner" = "team")) %>% 
  mutate(winPercentage = round((n / matchesPlayed) * 100,2)) %>% 
  select (team = winner, matchesPlayed, matchesWon = n, winPercentage)

###### # Start data exploration and WRITE THE PLOTS TO a pdf file ######################
#########################################

# First, check the correlation among different independent variables:
matSumm_cor1 <- na.omit(subset(matSumm, na.rm = TRUE, 
                               select = c("Inn1EOIRuns", "Over2Runs", "Over2Wkts","Over3Runs", "Over3Wkts", 
                                          "Over4Runs", "Over4Wkts", "Over5Runs", "Over5Wkts",
                                          "Over6Runs", "Over6Wkts" 
                                          )))
cor(matSumm_cor1)
# As expected, correlation with EOI Runs is higher as the number of overs played increases; 
# Runs scored have a positive correlation with EOI Runs and Wickets lost have a negative impact.

# Next check the corelation between a team winning this match with matches won against that opponent recently:
matSumm_cor2 <- na.omit(subset(matSumm, na.rm = TRUE, 
                               select = c("TeamBattingFirstWon", "BatFirstWonLastMat", 
                                          "BatFirstWinsInLast3Mat", "BatFirstWinsInLast5Mat" )))
cor(matSumm_cor2)
matSumm_cor3 <- na.omit(subset(matSumm, na.rm = TRUE, 
                               select = c("TeamBattingFirstWon", "BatSecondWonLastMat", 
                                          "BatSecondWinsInLast3Mat", "BatSecondWinsInLast5Mat" )))
cor(matSumm_cor3)
# These show that the last match result is not so consequential as the result over the last 5 matches

# Finally explore the correlation between match won by a team and prior number of matches played 
# and win percentage of the team:
matSumm_cor4 <- na.omit(subset(matchesWonStat, na.rm = TRUE, 
                               select = c("matchesPlayed", 
                                          "matchesWon", "winPercentage" )))
cor(matSumm_cor4)


dev.copy(pdf, "plots.pdf")   # Needed to create a plot 1st above for this command to work; otherwise complains of null device

# Before looking at covariations among independent variables, let's explore variation within the 2
# independent variables, one at a time: match winners, and runs scored in each innings
ggplot(mapping = aes(x = winner), data = matSumm) +
  geom_bar() +
  coord_flip()
# Shows Mumbai Indians have won most matches, followed by Chennai Super Kings, but this does not
# take into account how many matches each team has played; that is stored in matchesWonStat dataframe
ggplot(mapping = aes(x = team, y = winPercentage), data = matchesWonStat) +
  geom_point() +
  coord_flip()

ggplot(data = matSumm) +
  geom_histogram(mapping = aes(x = Inn1EOIRuns), binwidth = 0.5)
# Other than a couple of outliers at either end, this shows the runs scored are reasonably well distributed

# Now look at the covariations between the independent variables: 
# First, at the outset of a match, let's explore what the impact of a venue have on the final score of each team:
ggplot(mapping = aes(x = venueCity, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  coord_flip() 
ggplot(data = matSumm) +
  geom_histogram(mapping = aes(x = Inn1EOIRuns), binwidth = 0.5) +
  facet_wrap(~venueCity, nrow = 5)
# These 2 charts show that the effect of Venue/pitch is a short 20 over innings is not much; final scores are close to normally distributed

# Let's explore how often the winner of the toss (an initial advantage at the match start) goes on to win the match 
ggplot (data = matSumm) +
  geom_bar(mapping = aes (x= toss, fill = (toss == winner)),
           position = "fill") +
  coord_flip()
ggplot (data = matSumm) +
  geom_bar(mapping = aes (x= toss, fill = (toss == winner))) +
  coord_flip()
# This shows no significant affinity that the toss winner ultimately is also the match winner

# winner of a match based on past results:
ggplot(data = matSumm) +
  geom_point(mapping = aes(x=BatFirstWinsInLast5Mat, y = TeamBattingFirstWon),
             position = "jitter") +
  facet_wrap(~winner, nrow = 5)

ggplot(mapping = aes(x = BatFirstWonLastMat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill")
ggplot(mapping = aes(x = BatFirstWinsInLast3Mat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill")
ggplot(mapping = aes(x = BatFirstWinsInLast5Mat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill")
# This plot shows that the chances of a team winning today increase if it has won
# more of the last 5 matches against today's opponent.Result of just the last
# match does not seem to have a major bearing on this match's result

# Now explore effect of runs scored in powerplay overs on final score
ggplot(mapping = aes(x=Over6Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth()
# Continue plotting as we get deeper into an innings (predictions at th eend of 6th, 10th and 15th overs)  
ggplot(mapping = aes(x=Over10Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth()
ggplot(mapping = aes(x=Over15Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth()
# shows strong correlation of runs scored with the final score as we move from the 6th over towards 15th over
# of the max 20 overs-a-side innings: Points are grouped much more tightly along the lm line
# at the end of th 15th over than at the end of the 10th over, while at the end of the 6th over, they
# are more widely scattered

# Now explore the effect on the final score of the loss of wickets at the end of 6th, 10th and 15th overs:
ggplot(mapping = aes(x=Over6Wkts, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth
ggplot(mapping = aes(x=Over10Wkts, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth()
ggplot(mapping = aes(x=Over15Wkts, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth()
# These 3 plots show that while the number of wickets lost definitely have a negative impact on the
# final score, the points are scattered quite loosely about the lm line (even disregarding a few outliers)











##########################################################################
                  # Explore Variation in Runs Scored, Overs faced, Wickets Lost
                  # 1st try plain vanilla
                  ggplot(data = matSumm) +
                    geom_histogram (mapping = aes(x = EOIRuns), binwidth = 10)
                  
                  #ggplot(data = matSumm ) +
                  #  geom_histogram(mapping = aes (x = EOIOver), binwidth = 2)
                  #ggplot(data = matSumm) +
                  #  geom_histogram(mapping = aes (x = EOIWkts), binwidth = 1)
                  
                  #Now explore against specific teams
                  ggplot(data = matSumm, mapping = aes(x = EOIRuns)) +
                    geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 10)
                  
                  #ggplot(data = matSumm, mapping = aes(x = EOIOver)) +
                  #  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 2)
                  #ggplot(data = matSumm, mapping = aes(x = EOIWkts)) +
                  #  geom_freqpoly (mapping = aes (color = TeamNameBowl), binwidth = 1)
                  
                  #Finally explore performance at specific venues
                  ggplot(data = matSumm, mapping = aes(x = EOIRuns)) +
                    geom_freqpoly (mapping = aes (color = venueCity), binwidth = 10)
                  
                  #ggplot(data = matSumm, mapping = aes(x = EOIOver)) +
                  #  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 2)
                  #ggplot(data = matSumm, mapping = aes(x = EOIWkts)) +
                  #  geom_freqpoly (mapping = aes (color = venueCity), binwidth = 1)
                  
                  # These above plots show there is variation in the final total by CSK. Time to explore covariations
                  
                  #First let's explore covariation between total runs scored (EOIRuns) and opponents
                  ggplot(matSumm) +
                    geom_boxplot(aes(
                      x = reorder(TeamNameBowl, EOIRuns, FUN = median),
                      y = EOIRuns)) +
                    coord_flip()
                  # The above plot confirms that over the years, CSK has scored consistently higher against King's X1 Punjab
                  # than other opponents; and amongst the lowest against Rajasthan Royals. But there is a large
                  # variation in the number of runs against all opponets as shown by the margins of the inter-quartile range
                  
                  # Next explore EOIRuns at different venues (again continuous variable vs categorical variable)
                  ggplot(matSumm) +
                    geom_boxplot(aes(
                      x = reorder(venueCity, EOIRuns, FUN = median),
                      y = EOIRuns)) +
                    coord_flip()
                  # The above plot shows that over the years, the pitch at Kochi produces the least number of runs on an average 
                  # for CSK team and Vishakapatnam produces the highest runs on an average (median).
                  # But the inter-quartile range is high at almost all venues, so there are other factors involved as
                  # well in the final score for CSK in each match
                  # Let's plot the final scores for CSK against opponents at different venues and look at the smooth lm line:
                  ggplot(data = matSumm, mapping = aes( x= EOIRuns, y = venueCity)) +
                    geom_smooth(
                      mapping = aes(linetype = TeamNameBowl, color = TeamNameBowl),
                      method = "lm", se = FALSE)
                  # The above plot shows considerable variation at the same venue aginst the same opposition.
                  # There are some venues, like its home ground of Chennai, where CSK has played multiple opponents
                  # They have also played multiple opponents at Mumbai
                  
                  # Finally check EOIRuns scored in 1st innings (setting a target) vs 2nd innings (chasing a target)
                  ggplot(matSumm, aes(x = Innings_No, y = EOIRuns, color = TeamBattingWon)) +
                    geom_point(alpha = 0.2)
                  # While the above plot shows runs in Innings 1 are a bit higher for CSK than when it chases (Innings 2),
                  # but that may be because it is surpassing the opponent's scores in the 2nd innings before the allotted 
                  # 20 overs
                  
                  # Now explore covariation between 2 continuous variables by binning one of the 2 continuous variables
                  # Say number of wickets lost in the Powerplay (6 overs) vs total runs scored (EOIRuns)
                  ggplot(data = matSumm, mapping = aes(x = Over6Wkts, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over6Wkts, 1)), varwidth = TRUE)
                  # This plot clearly shows the fewer wickets lost in the 1st 6 overs, the higher the final total runs scored
                  # Let's see the effect of wickets lost at the end of 4th, 8th and 10th overs also on the EOIRuns:
                  ggplot(data = matSumm, mapping = aes(x = Over4Wkts, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over4Wkts, 1)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over8Wkts, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over8Wkts, 1)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over10Wkts, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over10Wkts, 1)), varwidth = TRUE)
                  # The above plots confirm that fewer wickets lost, higher the median final runs scored
                  
                  # Now explore covariation between 2 other continuos variables: Runs scored early on vs Final score
                  ggplot(data = matSumm, mapping = aes(x = Over4Runs, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over4Runs, 5)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over6Runs, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over6Runs, 5)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over8Runs, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over8Runs, 10)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over10Runs, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over10Runs, 10)), varwidth = TRUE)
                  ggplot(data = matSumm, mapping = aes(x = Over12Runs, y = EOIRuns)) +
                    geom_boxplot(mapping = aes(group = cut_width(Over12Runs, 10)), varwidth = TRUE)
                  # These above plots reveal that while the final score is less directly proportional to the runs scored
                  # at the end of 4th and 6th overs, the dependency is very strong by the end of 8th over and stays that
                  # way at the end of 10th and 12th overs as well
                  
                  # Next look at some facet grids: First the impact of wickets lost in Power Play on the final score:
                  ggplot(data = matSumm) +
                    geom_point(mapping = aes(x = EOIRuns, y = TeamNameBowl)) +
                    facet_wrap(~Over6Wkts, nrow = 3)
                  # The above facet plot shows number of wickets lost in the 1st 6 overs (Powerlay overs) significantly
                  # influence the final score of CSK regardless of the opponent
                  
                  # Next look at the impact of run rate (runs scored) in the PowerPlay (1st 6 overs) on the final score:
                  ggplot(data = matSumm) +
                    geom_point(mapping = aes(x = EOIRuns, y = TeamNameBowl)) +
                    facet_wrap(~ (group = cut_width(Over4Runs, 5)), nrow = 3)
                  # the above plot seems to suggest that the initial run rate in the 1st 6 overs is less of a factor for the final score
                  
                  dev.off()
                  
                                        # The above exploration should next be repeated for each opponent of CSK to explore what variables may 
                                        # predict its final score against the particular opponent, say KKR:
                                        innSumm_cskKKR <- filter(innSumm_csk, TeamNameBowl == "Kolkata Knight Riders")
                                        matDet_cskKKR <- filter(matDet_csk, TeamNameBowl == "Kolkata Knight Riders")
                                        
                                        # explore the ipact of ball-by-ball run scorin at different venues and in different innings:
                                        ggplot(data = matDet_cskKKR, mapping = aes( x = cumRuns, y = cumOver)) +
                                          geom_smooth(
                                            mapping = aes(linetype = as.factor(Innings_No), color = venueCity),
                                            method = "lm", se = FALSE)
                                        
                                        ggplot(data = innSumm_cskKKR, mapping = aes( y = EOIRuns, x = Over6Wkts)) +
                                          geom_smooth(
                                            mapping = aes(linetype =  venueCity),
                                            method = "lm", se = FALSE)
                                        
                  
                                                    # Start data modeling 
                                                    # First have a smaller file for one team: try Rajasthan Royals and CSk as the teams, one by one
                                                    #matDet_csk <- filter (matDet, TeamNameBat == "Rajasthan Royals" & Season < 2015)
                                                    #innSumm_csk <- filter (innSumm, TeamNameBat == "Rajasthan Royals" & Season < 2015)
                                                    
                                                    matDet_csk <- filter (matDet, TeamNameBat == "Chennai Super Kings" & Season < 2015)
                                                    innSumm_csk <- filter (innSumm, TeamNameBat == "Chennai Super Kings" & Season < 2015)
                                                    
                                                    
                                                    cskEOIModel = lm(EOIRuns ~ venueCity, data = innSumm_csk)
                                                    summary (cskEOIModel)
                                                    # Above shows poor corelation between EOIRuns and VenueCity
                                                    
                                                    #Obviously there is a strong positive corelation betwen EOIRuns and EOIRunRate as shown below:
                                                    
                                                    plot(innSumm_csk$EOIRunRate, innSumm_csk$EOIRuns)
                                                    cskEOIModel = lm(EOIRuns ~ EOIRunRate, data = innSumm_csk)
                                                    summary (cskEOIModel)
                                                    
                                                    #But let's explore relationships between EOIRuns and runs/wickets at the end of certain overs:
                                                    plot(innSumm_csk$Over6Runs, innSumm_csk$EOIRuns )
                                                    cskEOIModel = lm(EOIRuns ~ Over6Runs, data = innSumm_csk)
                                                    summary (cskEOIModel)
                                                    
                                                    plot(innSumm_csk$Over10Wkts, innSumm_csk$EOIRuns )
                                                    cskEOIModel = lm(EOIRuns ~ Over10Wkts, data = innSumm_csk)
                                                    summary (cskEOIModel)
                                                    
                                                    # Above shows no corelation in these single independent variables either with the dependent variable EOIRuns
                                                    
                                                    
                                                    #Now try 2 independent variables in the model: Runs and Wickets at various points in an innings, esp early on
                                                    # like end of the powerplay (6 overs): runs scored and wickeets lost: 
                                                    cskEOIModel = lm (EOIRuns ~ Over12Runs + Over6Wkts, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    # The above just shows that number of wickets lost is not a significant independent variable; only Runs scored
                                                    
                                                    
                                                    cskEOIModel = lm (EOIRuns ~ Over6Runs + Over10Wkts + TeamNameBowl + venueCity, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    # R2 value is still quite low for these combinations (0.3 to 0.44 for 4 and 6 over runs; wickets lost continue to not be significant
                                                    
                                                    cskEOIModel = lm (EOIRuns ~ Over4Runs + Over6Runs + Over8Runs +TeamNameBowl + venueCity, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    #No significant improvement even after adding over6Runs and over8Runs!! 
                                                    
                                                    
                                                    # So now try in which overs milestone runs came:30th rin, 50 runs...75 runs...100 runs.....
                                                    cskEOIModel = lm (EOIRuns ~ Runs100InOver, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    #Not till the 75th run, does R2 raches 0.46; at 100th run, it improves marginally to 0.52 Not good enough!
                                                    
                                                    #Next try the milestone wickets falling:
                                                    cskEOIModel = lm (EOIRuns ~ Wkts5InOver, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    # Even with the 5th wicket down, R2 gets up to only 0.34. Wickets again have even less of a correlation
                                                    
                                                    # Try a combination of these milestone: runs + wickets:
                                                    cskEOIModel = lm (EOIRuns ~ Runs100InOver + Wkts4InOver, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    #Even 50 runs and 5 wickets gives an R2 of only 0.47!! 50 runs and 2 or 3 wickets have the same R2 of 0.40
                                                    # 100 runs and 4 wickets takes up to 0.61. Number of wickets lost is not significant!!
                                                    
                                                    cskEOIModel = lm (EOIRuns ~ Runs30InOver + Over10Runs, data = innSumm_csk)
                                                    summary(cskEOIModel)
                                                    #Only 0.46 R2 for the above for RR, and only 0.42 for CSk!
                                                    
                                                    # cor(innSumm_csk)
                  
                  
