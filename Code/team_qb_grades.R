library(tidyverse)

#Load Data
qb_df <- c()
for (i in 2008:2021) {
  temp_df_grades <- read.csv(paste0('PFF_Data/passing_summary-',i,'.csv')) %>%
    filter(position == 'QB') %>%
    select(team_name, player, dropbacks, grades_offense) %>%
    mutate(season = i)
  qb_df <- rbind(qb_df, temp_df_grades)
}

#Get Team Grades by season
team_df <- qb_df %>%
  mutate(pts = grades_offense*dropbacks) %>%
  group_by(season, team_name) %>%
  summarise(team_grade = round(sum(pts)/sum(dropbacks),2)) %>%
  ungroup() %>%
  mutate(team_name = nflreadr::clean_team_abbrs(team_name))

write.csv(team_df, 'team_qb_grades.csv', row.names = FALSE)
