
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
matSumm_cor4 <- na.omit(subset(matSumm, na.rm = TRUE, 
                               select = c("TeamBattingFirstWon", "BatFirstMatchesWon", 
                                          "BatFirstWinPercentage", "BatSecondMatchesWon",
                                          "BatSecondWinPercentage")))
cor(matSumm_cor4)

dev.off()  # remove all old plots


summary_df1 <- matSumm %>% group_by(winner) %>% summarize(MatchesWon = n())
ggplot(mapping = aes(x = reorder(winner, MatchesWon), y = MatchesWon), data = summary_df1) +
  geom_bar(stat = "identity") +
  coord_flip()

dev.copy(pdf, "plots.pdf")   # Needed to create a plot 1st above for this command to work; otherwise complains of null device
# Before looking at covariations among independent variables, let's explore variation within the 2
# independent variables, one at a time: match winners, and runs scored in each innings
summary_df1 <- matSumm %>% group_by(winner) %>% summarize(MatchesWon = n())
ggplot(mapping = aes(x = reorder(winner, MatchesWon), y = MatchesWon), data = summary_df1) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs( y= "Number of matches won", x = "IPL Teams", 
    title = paste("Top 8 teams are fairly close in terms of number of matches won"),
                  subtitle = paste("        And we will examine the win percentage too in the next graph"),
                                   caption = "IPL20 Cricket Data 2008 - 2017"
   )
# Shows Mumbai Indians have won most matches, followed by Chennai Super Kings, but this does not
# take into account how many matches each team has played; so let's explore Win Percentages: 
# (matches won / matches played)
summary_df1 <- matSumm %>% select (BatFirst, BatFirstWinPercentage) %>% 
  group_by(BatFirst) %>% arrange(BatFirstWinPercentage) %>% distinct()
ggplot(mapping = aes(x = reorder(BatFirst, BatFirstWinPercentage), y = BatFirstWinPercentage), 
       data = summary_df1) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y= "Percentage of matches won", x = "IPL Teams",
    title = paste("Top 4 teams have won more than 50% of their matches, and top 8 more than 45%"),
    subtitle = paste("  So matches tend to be fairly close and predicting winner with certainty will be difficult"),
    caption = "IPL20 Cricket Data 2008 - 2017"
  )

ggplot(data = matSumm) +
  geom_histogram(mapping = aes(x = Inn1EOIRuns), binwidth = 2) +
  labs(x= "Final Score in an Innings", y = "Count",
       title = paste("Runs scored have a normal distribution"),
       subtitle = paste("  We will next break it down by independent variables"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# Other than a couple of outliers at either end, this shows the runs scored are reasonably 
# well distributed

# Now look at the covariations between the independent variables: 
# First, at the outset of a match, let's explore what the impact of a venue have on the final score of each team:
ggplot(mapping = aes(x = venueCity, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  coord_flip() 
ggplot(data = matSumm) +
  geom_histogram(mapping = aes(x = Inn1EOIRuns), binwidth = 0.5) +
  facet_wrap(~venueCity, nrow = 6) +
  labs(x= "Final Score in an Innings", y = "Count",
       title = paste("Each venue seems to have some high and some low scores, and some in between"),
       subtitle = paste("  So next look at the median distribution to confirm"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# These 2 charts show that the effect of Venue/pitch in a short 20 over innings is not much; final scores are close to normally distributed
ggplot(matSumm) +
  geom_boxplot(aes(
    x = reorder(venueCity, Inn1EOIRuns, FUN = median),
    y = Inn1EOIRuns)) +
  coord_flip() +
  labs(x= "Final Score in an Innings", y = "Venue",
       title = paste("High inter-quartile means venue may not be a big contributor"),
       subtitle = paste("  The outliers, Dharamshala and the South African grounds had very few matches"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# The venue BoxPlot suggests that Dharamshala ground is high-scoring and Bloemfontein is low-scoring,
# but the number of data points for these 2 venues is very small. At venues with a higher
# number of matches, the inter-quartile range is high, meaning runs scored fluctuate 
# even at the same venue; will see what the model will show

# Let's explore how often the winner of the toss (an initial advantage at the match start) goes on to win the match 
ggplot (data = matSumm) +
  geom_bar(mapping = aes (x= toss, fill = (toss == winner)),
           position = "fill") +
  coord_flip()  +
  labs(x= "Toss Winner", y = "Percentage that the toss winner wins the match",
       title = paste("Percentage times a team wins a match after winning toss is 40-60% "),
       subtitle = paste("Does not appear that winning the toss is significant"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot (data = matSumm) +
  geom_bar(mapping = aes (x= toss, fill = (toss == winner))) +
  coord_flip() +
  labs(x= "Toss Winner", y = "Number of times the toss winner wins the match",
       title = paste("Does not appear any team wins often enough after winning the toss"),
       subtitle = paste("Toss can safely be discarded as an influencer to winning matches"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# This shows no significant affinity that the toss winner ultimately is also the match winner

# winner of a match based on past results:
ggplot(data = matSumm) +
  geom_point(mapping = aes(x=BatFirstWinsInLast5Mat, y = TeamBattingFirstWon),
             position = "jitter") +
  facet_wrap(~winner, nrow = 5)

ggplot(mapping = aes(x = BatFirstWonLastMat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill") +
  labs(x= "Did Team A win the last match between the two", 
       y = "Likley % that Team A will win today ",
       title = paste("Team that won last match has just over 50% chnace of winning today"),
       subtitle = paste("Team that lost the last match has 60% chance of losing today"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot(mapping = aes(x = BatFirstWinsInLast3Mat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill") +
  labs(x= "Count Team A has won last 3 matches between the two", 
       y = "Likley % that Team A will win today",
       title = paste("Even if you have lost the last 3 matches..."),
       subtitle = paste("...you have 30% chance of winning today against the same opponent"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot(mapping = aes(x = BatFirstWinsInLast5Mat, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill") +
  labs(x= "Win % of Team A against all teams", 
       y = "Likley % that Team A will win today ",
       title = paste("Your chances of winning today are much better if..."),
       subtitle = paste("...you have won more of your reccent matches against this opponent"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# This plot shows that the chances of a team winning today is better if it has won
# more of the last 5 matches against today's opponent. Result of just the last
# match between these 2 teams does not seem to have a major bearing on this match's result

# Another thing to examine is a team's past record against all teams:
ggplot(mapping = aes(x = BatFirstWinPercentage, fill = TeamBattingFirstWon)) +
  geom_bar (data = matSumm, position = "fill") +
  labs(x= "Win % of Team A against all teams", 
       y = "Likley % that Team A will win today ",
       title = paste("Unless a team has an excellent win %, the match is too close to call"),
       subtitle = paste("Your chances of losing are high if you have been winning less than 50%"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )


# Now explore effect of runs scored in powerplay overs on final score
ggplot(mapping = aes(x=Over6Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x= "Runs scored at the end of 6 overs", 
       y = "Final score (End of Innings)",
       title = paste("The higher your score is at Over 6, the higher your final score"),
       subtitle = paste("  But there is significant scatter outside the trend line"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# Continue plotting as we get deeper into an innings (predictions at th eend of 6th, 10th and 15th overs)  
ggplot(mapping = aes(x=Over10Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x= "Runs scored at the end of 10 overs", 
       y = "Final score (End of Innings)",
       title = paste("Deeper into an innings, the trend line is sharper"),
       subtitle = paste("  Definite positive correlation, with less outliers"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot(mapping = aes(x=Over15Runs, y = Inn1EOIRuns), data = matSumm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x= "Runs scored at the end of 15 overs", 
       y = "Final score (End of Innings)",
       title = paste("Strong positive correlation deep into an innings"),
       subtitle = paste("  Points are grouped much closer to the model line"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# shows strong correlation of runs scored with the final score as we move from the 6th over towards 15th over
# of the max 20 overs-a-side innings: Points are grouped much more tightly along the lm line
# at the end of th 15th over than at the end of the 10th over, while at the end of the 6th over, they
# are more widely scattered

# Now explore the effect on the final score of the loss of wickets at the end of 6th, 10th and 15th overs:
# We look at the covariation between 2 continuous variables by binning one of the 2 continuous variables:
ggplot(data = matSumm, mapping = aes(x = Over6Wkts, y = Inn1EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over6Wkts, 1)), varwidth = TRUE) +
  labs(x= "Wickets Lost at the end of 6 overs", 
       y = "Final score (End of Innings)",
       title = paste("The more wickets you lose, the less your final score"),
       subtitle = paste("  Recovering to a 'good' score after losing 3 or more wickets seems difficult"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot(data = matSumm, mapping = aes(x = Over10Wkts, y = Inn1EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over10Wkts, 1)), varwidth = TRUE) +
  labs(x= "Wickets Lost at the end of 10 overs", 
       y = "Final score (End of Innings)",
       title = paste("The more wickets you lose, the less your final score"),
       subtitle = paste("  Recovering to a 'good' score after losing 5 or more wickets seems difficult"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
ggplot(data = matSumm, mapping = aes(x = Over15Wkts, y = Inn1EOIRuns)) +
  geom_boxplot(mapping = aes(group = cut_width(Over15Wkts, 1)), varwidth = TRUE) +
  labs(x= "Wickets Lost at the end of 15 overs", 
       y = "Final score (End of Innings)",
       title = paste("Deep into the innings"),
       subtitle = paste(" Clearly shows the negative impact of wickets lost on final score"),
       caption = "IPL20 Cricket Data 2008 - 2017"
  )
# These 3 plots clearly show that the number of wickets lost has a negative impact on the
# final score

# From the plots, it looks like the median runs scored and wickets lost early-on have the most positive 
# and most negative impact respectively on the final score posted by a team, among all independent variables

dev.off()

##########################################################################
                  