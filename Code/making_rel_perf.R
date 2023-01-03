#Load Packages
library(tidyverse)

#Load Play by Play data
pbp <- nflreadr::load_pbp(2008:2021)

#Get offensive EPA/play
off_epa <- pbp %>%
  filter(special_teams_play == 0,
         !is.na(epa), !is.na(posteam),
         season_type == 'REG') %>%
  group_by(posteam, season) %>%
  summarise(epa_play = mean(epa)) %>%
  ungroup()

#Get defensive EPA/play
def_epa <- pbp %>%
  filter(special_teams_play == 0,
         !is.na(epa), !is.na(defteam),
         season_type == 'REG') %>%
  group_by(defteam, season) %>%
  summarise(epa_play = mean(epa)) %>%
  ungroup()

#Weight the offensive 1.5 and defensive with 1
total_epa <- off_epa %>%
  left_join(def_epa, by = c('posteam' = 'defteam', 'season')) %>%
  rename(epa_O = epa_play.x, epa_D = epa_play.y) %>%
  mutate(epa_tot = epa_O*1.5 + (epa_D*-1))

#Standardize
stand_epa <- total_epa %>%
  group_by(season) %>%
  mutate(epa_standard = round((epa_tot - mean(epa_tot))/sd(epa_tot),4)) %>%
  mutate(ranking_row = as.factor(rank(-epa_standard))) %>%
  ungroup()

#Visual check using 2020 and 2021
#-----
#2020 Playoff teams
play20 <- nflreadr::load_schedules(2020) %>%
  filter(game_type != 'REG') %>%
  nflreadr::clean_homeaway() %>%
  group_by(team) %>%
  summarise(tot = n()) %>%
  select(team)

#2021 Playoff teams
play21 <- nflreadr::load_schedules(2021) %>%
  filter(game_type != 'REG') %>%
  nflreadr::clean_homeaway() %>%
  group_by(team) %>%
  summarise(tot = n()) %>%
  select(team)

#Add playoff variable for plotting
data_viz <- stand_epa %>%
  filter(season %in% c(2020,2021)) %>%
  mutate(playoffs = case_when(season == 2020 & posteam %in% play20$team ~ 1,
                              season == 2021 & posteam %in% play21$team ~ 1,
                              TRUE ~ 0))

#Plot 
data_viz %>% ggplot(aes(fct_reorder(ranking_row, -epa_standard), y = epa_standard)) +
  geom_col(aes(fill = posteam, color = posteam, alpha = playoffs)) +
  scale_alpha_continuous(range = c(.55,1), guide = 'none') +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam, alpha = playoffs), width = .04) +
  ggthemes::theme_economist_white() +
  labs(title = 'NFL Teams\' Relative Performance',
       subtitle = 'Playoff Teams Highlighted',
       caption = 'Data: NFLverse | Plot: @Timboslice003',
       x = '',
       y = 'Relative Performance') +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 16, hjust = .5),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        plot.background = element_rect('white'),
        panel.border = element_rect(fill = 'transparent', 'black'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black')) + 
  facet_wrap(~season)

#ggsave('rel_perf.png', width = 14, height = 10, dpi = 'retina')
#-----

#Write to csv for storage
epa_csv <- stand_epa %>%
  select(team = posteam, season, rel_perf = epa_standard)
#write.csv(epa_csv, 'rel_perf.csv', row.names = FALSE)

#Testing Correlation to wins in year N and N + 1
wins_df <- read.csv('wins_data.csv')

#Correlation with current season winning percentage
cor_wp_df <- stand_epa %>%
  select(season, team = posteam, epa_standard) %>%
  left_join(wins_df, by = c('season','team'))

cor(x = cor_wp_df$epa_standard, cor_wp_df$wp)

#Plot
cor_wp_df %>% ggplot(aes(x = epa_standard, y = wp)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(0,1) +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.minor = element_blank(),
        plot.background = element_rect('white'),
        axis.line = element_line(),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black')) +
  labs(title = 'Same Season WP',
       x = 'Relative Performance',
       y = 'Winning Percentage')

#ggsave('rp_wp_cor.png', width = 14, height = 10, dpi = 'retina')
#Correlation with next season winning percentage
cor_wp_df1 <- stand_epa %>%
  select(season, team = posteam, epa_standard) %>%
  mutate(season_next = season + 1) %>%
  left_join(wins_df, by = c('season_next' = 'season','team')) %>%
  filter(!is.na(wp))

#Plot
cor_wp_df1 %>% ggplot(aes(x = epa_standard, y = wp)) +
  geom_point()  +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(0,1) +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.minor = element_blank(),
        plot.background = element_rect('white'),
        axis.line = element_line(),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black')) +
  labs(title = 'Next Season WP',
       x = 'Relative Performance',
       y = 'Winning Percentage')
#ggsave('rp_wp_cor2.png', width = 14, height = 10, dpi = 'retina')

#Correlation Coefficients
cor(x = cor_wp_df1$epa_standard, cor_wp_df1$wp)
cor(x = cor_wp_df$epa_standard, cor_wp_df$wp)

#Correlation of wins in year N vs wins in year N +1
wins_compare <- cor_wp_df1 %>%
  left_join(wins_df %>% select(season, team, wp), by = c('season', 'team'))
cor(x = wins_compare$wp.y, wins_compare$wp.x)

