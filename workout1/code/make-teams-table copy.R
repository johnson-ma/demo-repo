#Title:NBA 2018 Teams
#Description: This script contains the required variables to be used in the ranking analysis
#Input: data file 'nba2018.csv
#Output: data file 'nba2018-teams.csv'
library(readr)
library(dplyr)
library(ggplot2)
nba2018 <- read.csv('/users/maiahjohnson/stat133/workout1/nba2018.csv', stringsAsFactors = FALSE)
nba2018$experience[nba2018$experience == 'R'] <- '0'
nba2018$salary <- round(nba2018$salary /1000000, 2)
pos_labels <-c("center","power_fwd", "point_guard", "small_fwd", "shoot_guard")
nba2018$position <- factor(nba2018$position, labels = pos_labels)
nba2018$missed_field_goals <- nba2018$field_goals_atts - nba2018$field_goals 
nba2018$missed_free_throws <- nba2018$points1_atts - nba2018$points1 
nba2018$rebounds <- nba2018$total_rebounds  
nba2018$points <- nba2018$points1 + nba2018$points2 + nba2018$points3
nba2018$efficiency <- (nba2018$points + nba2018$rebounds + nba2018$assists + nba2018$steals + nba2018$blocks - nba2018$missed_field_goals - nba2018$missed_free_throws - nba2018$turnovers)/nba2018$games 
mutate(nba2018, nba2018$missed_field_goals, nba2018$missed_free_throws + nba2018$rebounds + nba2018$points +nba2018$efficiency)
sink(file = '/users/maiahjohnson/stat133/workout1/code/efficiency-summary.txt')
summary(nba2018$efficiency)
sink()
teams <- nba2018%>% group_by(team)%>%select(team,experience,salary,points3,points2,points1,points,off_rebounds,def_rebounds,assists,steals,blocks,turnovers,fouls,efficiency)%>%summarise(total_salary = sum(salary), total_points3 = sum(points3), total_points2 = sum(points2), total_points1 = sum(points1),total_points = sum(points), total_off_reb = sum(off_rebounds), total_def_reb = sum(def_rebounds), total_assists = sum(assists), total_steals = sum(steals),total_blocks = sum(blocks),total_turnovers = sum(turnovers),total_fouls = sum(fouls), total_eff = sum(efficiency))
sink(file = 'users/maiahjohnson/stat133/workout1/code/efficiency-summary.txt')
summary(teams)
sink()
write.csv(teams, '/users/maiahjohnson/stat133/workout1/data/nba2018-teams.csv'), row.names = FALSE
sal_g <- ggplot(data = teams, aes(x = team, y = total_salary/1000000)) + geom_bar(stat = 'identity') + coord_flip()
sal_g + labs(title = "NBA Teams Ranked by Total Salary", y = "Salary(in millions", x = "Team") + geom_hline(yintercept = 75)
point_g <- ggplot(data = teams, aes(x = team, y = total_points)) + geom_bar(stat = 'identity') + coord_flip()
point_g + labs(title = "NBA Teams Ranked by Total Points", y = "Points", x = "Team") + geom_hline(yintercept = 4000)
eff_g <- ggplot(data = teams, aes(x = team, y = total_eff)) + geom_bar(stat = 'identity') + coord_flip()
eff_g + labs(title = "NBA Teams Ranked by Total Efficiency", y = "Efficiency", x = "Team") + geom_hline(yintercept = 85)
block_g <- ggplot(data = teams, aes(x = team, y = total_blocks)) + geom_bar(stat = 'identity') + coord_flip()
block_g + labs(title = "NBA Teams Ranked by Total Blocks", y = "Blocks", x = "Team") + geom_hline(yintercept = 0)

       