library(tidyverse)
library(ggplot2)

#Load in schedules
schedule_loaded <- nflreadr::load_schedules(2008:2021)

#Filter for regular season and select wanted data
schedule <- schedule_loaded %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team:home_score) %>%
  nflreadr::clean_homeaway() #Gives two rows for each team/game

#Get Win Percentage
win_pct <- schedule %>%
  mutate(win_points = case_when(team_score > opponent_score ~ 1,
                                team_score < opponent_score ~ 0,
                                team_score == opponent_score ~ .5,
                                TRUE ~ 5)) %>%
  group_by(season, team) %>%
  summarise(wp = round(sum(win_points)/n(),3),
            wins = sum(win_points),
            losses = n() - wins)

#Fix the team abbreviations
win_pct$team <- nflreadr::clean_team_abbrs(win_pct$team)
#write.csv(win_pct, file = 'wins_data.csv', row.names = FALSE)
