library(tidyverse)

#QB PFF Grades Dataframe
qb_df <- c()
for (i in 2006:2021) {
  temp_df_grades <- read.csv(paste0('PFF_Data/passing_summary-',i,'.csv')) %>%
    filter(position == 'QB') %>%
    select(team_name, player, dropbacks, grades_offense, player_id) %>%
    mutate(season = i)
  qb_df <- rbind(qb_df, temp_df_grades)
}

#NFL verse IDs anf PFF IDs
ids_loaded <- nflreadr::load_ff_playerids()

ids <- ids_loaded %>%
  select(name, gsis_id, pff_id)

#Week 1 Starting QBs Dataframe
pbp <- nflreadr::load_schedules(2008:2021)
pbp2 <- nflreadr::clean_homeaway(pbp)

#Get week 1 QBs PFF IDs
qbs_week1 <- pbp2 %>%
  filter(game_type == 'REG',
         week == 1 | (week == 2 & season == 2017 & (team %in% c('MIA','TB')))) %>%
  select(team, season, team_qb_name, team_qb_id) %>%
  mutate(team = nflreadr::clean_team_abbrs(team, current_location = TRUE)) %>%
  left_join(ids, by = c('team_qb_id' = 'gsis_id'))

#Find missing IDs
need_ids <- qbs_week1 %>%
  filter(is.na(pff_id)) %>%
  group_by(team_qb_name) %>%
  summarise(tot = n())

#Gather missing PFF Ids
need_pff <- qb_df %>%
  filter(player %in% need_ids$team_qb_name) %>%
  group_by(player) %>%
  summarise(tot = n(), player_id = last(player_id))

#Add in missing IDs
week1_qbs_df <- qbs_week1 %>%
  left_join(need_pff, by = c('team_qb_name' = 'player')) %>%
  mutate(id = as.integer(ifelse(!is.na(pff_id), pff_id, player_id))) %>%
  select(team, season, team_qb_name, id)

#Loop to get last grade
lastPFF <- c()
for (i in 1:length(week1_qbs_df$team)) {
  temp_id <- week1_qbs_df[i,]$id
  temp_season <- week1_qbs_df[i,]$season
  
  temp_df <- qb_df %>%
    filter(player_id == temp_id,
           temp_season - season > 0,
           temp_season - season < 2)
  
  last_grade <- sum(temp_df$dropbacks*temp_df$grades_offense)/(sum(temp_df$dropbacks))
  
  temp_join <- data.frame(id = temp_id, season = temp_season, last_grade)
  lastPFF <- rbind(lastPFF, temp_join)
  
}


#Final DF for csv
final_df <- week1_qbs_df %>%
  left_join(lastPFF, by = c('season', 'id'))

#Check for rookies
rookie_df <- final_df %>%
  filter(is.nan(last_grade))

rooks <- qb_df %>%
  left_join(rookie_df, by = c('player_id' = 'id', 'season')) %>%
  filter(!is.na(team)) %>%
  select(team_name:grades_offense)

rook_mean <- round(mean(rooks$grades_offense),2)

#Sub in rookie mean
csv_df <- final_df %>%
  mutate(last_grade = ifelse(!is.na(last_grade), last_grade, rook_mean))

write.csv(csv_df, 'lastPFF.csv', row.names = FALSE)  
