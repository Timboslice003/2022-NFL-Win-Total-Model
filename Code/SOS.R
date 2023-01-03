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
    select(season, Team, team_abbr, ewin = `Win Total`) %>%
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
  
  #Get schedule from nflfastr filter our play
  s_loaded <- nflreadr::load_schedules(year)
  s_loaded <- s_loaded %>% filter(game_type == 'REG')
  s_clean1 <- s_loaded %>%
    select(season,gsis, team_abbr = home_team, opp = away_team, week)
  s_clean2 <- s_loaded %>%
    select(season,gsis, team_abbr = away_team, opp = home_team, week)
  sclean3 <- rbind(s_clean1,s_clean2) %>%
    arrange(gsis)
  
  #Switch all abbreviations to current
  sclean3$team_abbr <- nflreadr::clean_team_abbrs(sclean3$team_abbr, current_location = TRUE)
  sclean3$opp <- nflreadr::clean_team_abbrs(sclean3$opp, current_location = TRUE)
  
  #Get expected and actual win total of every opponent to then get respective SOS 
  total_sch <- df %>%
    left_join(sclean3, by = c('season', 'team_abbr')) %>%
    mutate(ewin = NULL,
           awin = NULL,
           gsis = NULL) %>%
    left_join(df, by = c('season','opp' = 'team_abbr')) %>%
    left_join(act_wins_df, by = c('season','opp' = 'team')) %>%
    group_by(season, team_abbr) %>%
    summarise(exp_sos = round(mean(ewin)/n(),3),
              act_sos = round(mean(wins)/n(),3)) %>%
    ungroup()
  
  #Add to df
  my_df <- rbind(my_df, total_sch)
}


#Visual Check on correlation between actual and expected SOS
plot_sch <- my_df %>%
  filter(!is.na(exp_sos)) %>%
  left_join(act_wins_df, by = c('season', 'team_abbr' = 'team'))

#Get Correlation
cor.test(x = plot_sch$act_sos, y = plot_sch$exp_sos)

#Plot
plot_sch %>% ggplot(aes(x = exp_sos, y = act_sos)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Expected Strength of Schedule',
       y = 'Actual Strength of Schedule',
       title = 'Using Preseason Win Totals to Predict S.O.S.',
       subtitle = 'From 2008 through 2021',
       caption = 'Figure: @Timboslice003 | Data: www.sportsoddshistory.com') +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
      plot.subtitle = element_text(size = 16, hjust = .5),
      axis.title = element_text(size = 17, face = 'bold'),
      axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
      axis.text.x = element_text(size = 13, face = 'bold', color = 'black'),
      plot.background = element_rect('white'),
      panel.border = element_rect(fill = 'transparent', 'black'),
      plot.caption = element_text(size = 13, face = 'bold', color = 'black'))

#ggsave('sos_compare.png', width = 14, height = 10, dpi = 'retina')
#Write to csv for storage
#write.csv(plot_sch, 'sos_wins.csv', row.names = FALSE)
