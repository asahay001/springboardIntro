# Springboard Capstone project ideas                                           Amitav Sahay

## Introduction

TV following of cricket is said to be second among all sports world-wide, behind only soccer. One of the innovations in the last 2 decades has been to shorten the game to maximum 20 overs a side, to allow a match to be finished within 3 hours, rather than allow a match to go on for 5 days or even 8-9 hours. This abridged version is called T20 cricket and ha caught on among cricket-playing nations. (Each over consists of 6 legal balls). 
Indian Premier League (IPL) T20 is the recognized leader in the popularity of T20 cricket. Held annually and promoted with large sponsorships, it has edge-of the seat action and huge fan viewing globally, with international players sought after by the 8-10 IPL clubs. 
Most ardent followers are interested in predicting the winner of any given match and the runs that each of the two teams will socre in each match. Earlier spectators would just guess based on their gut. But with the evolution of data mining, staticians have created working prediction models that TV spectators are shown throughout a match; the predictions change with every ball bowled.

## Idea 1: 
Use my newly learnt data science skills to see if I can predict the final score of team bating first based on their score at the end of 5 overs, then at the end of 10 overs, and finally at the end of 15 overs of the 20 overs a side match. Then I will do the same predictions for the team batting second, and also try to predict the probability of who will be winner of the match at the end of the 5th, 10th and 15th overs of the 2nd team's innings. 

## Idea 2: 
Another idea for the project is to be able to predict what will it take for each of the two teams to win: does the data show the a team is likley to win if their top scorer scores more than a certain number of runs in a certain number of balls (called strike rate), or their best-performing bowler in the match gets a certain number of wickets conceding only a certain number of runs in his allotted 4 overs (called economy rate). Even though the composition of teams changes over years (different players are recruited every few years for each of the clubs), it is worthwhile to see if top one or two performers of a team, whoever they may be, can predictably influence the chances of a favorable result of that match for his club. e.g. Team A wins 85% of matches when Player X scores say more than 50 runs in a match at a strike rate of 125; or Team A wins 70% of its matches, when bowler Y picks up say 4 wickets at an economy rate of 7 runs per over. 

## Who will use the results:
The client for both the project ideas would be the people watching a match live, or more aptly, the TV channel which will be broadcasting each match to the audience. The audience have been fed with these types of predictions and many of them devour such tid-bits throughtout the match which makes for a more active and involved spectator. 

## Data for project
Ball-by-ball data for IPL T20 cricket is available from 2008 through 2017. Its published by cricsheet.org in YAML format. It has also been transformed into four csv files by a data sceintist and is publicly available [here](https://data.world/raghu543/ipl-data-till-2017) . I plan to use these 4 csv files for the R-based project. While the largest file with 15,450 rows has the ball-by-ball details of each match for 10 years, the other 3 csv files contain: a) Details at match level: match id, venue, date of the match, result, margin of victory, etc.; b) Names of each of the 495 players who have particpated for the clubs over the 10 years; and c) which player played in which match for which team; this file has about 14,000 rows. 

## Data Manipulations
Some data wrangling would be necessary for this project. e.g. joining across the 4 files; summing the scores at the end of 5th, 10th and 15th overs for each match for each innings; or calculating the strike rate and economy rate and computing and adding the 2-3 match winning performes for each match to the match file. 

## Conclusion
Based on preliminary discussion with my mentor, Nathan, I will like to proceed with Idea 1 for my Capstone project. 
