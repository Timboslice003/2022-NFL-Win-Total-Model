library(rvest)
library(tidyverse)
library(ggplot2)

#Get Team Abbrs and Team names to help with joining
team_info <- nflreadr::load_teams() %>%
  select(team_name, team_abbr) %>%
  filter(team_abbr != 'LAR')

#Get actual wins data and correct abbreviations
act_wins_df <- read.csv('wins_data.csv')
act_wins_df$team <- nflreadr::clean_team_abbrs(act_wins_df$team,
                                               current_location = TRUE)
act_wins_df <- act_wins_df %>%
  select(season,team,wins)

#Create expected SOS data base
my_df <- NULL

for (year in 2008:2021) {
  #Load url with yearly O/U for each NFL team
  url <- paste0( "https://www.sportsoddshistory.com/nfl-win/?y=",year,"&sa=nfl&t=win&o=t")
  content <- read_html(url)
  
  #Grab Table
  my_table_loaded <- content %>% html_table()
  my_table_reg <- my_table_loaded[[1]]
  
  #Clean df. Use Team names to grab abbreviations
  df <- my_table_reg %>%
    mutate(season = year) %>%
    left_join(team_info, by = c('Team' = 'team_name')) %>%
    select(season, Team, team_abbr, ewin = `Win Total`, awin = `ActualWins`) %>%
    mutate(team_abbr = case_when(str_detect(Team,'Washington') ~ 'WAS',
                                 str_detect(Team,'Rams') ~ 'LA',
                                 TRUE ~ team_abbr))
  
  #Switch all abbreviations to current
  df$team_abbr <- nflreadr::clean_team_abbrs(df$team_abbr, current_location = TRUE)
  
  #Add in prediction for 2011 Colts
  if (year == 2011){
    df <- df %>%
      mutate(ewin = ifelse(is.na(ewin),5.5,ewin))
  }
  my_df <- rbind(my_df, df)
}
rmse(my_df$awin,my_df$ewin)  
