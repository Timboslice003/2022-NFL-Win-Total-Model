library(rvest)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(gt)

#Get Team Abbrs and Team names to help with joining
team_info <- nflreadr::load_teams() %>%
  select(team_name, team_abbr, team_conf, team_division,
         team_conference_logo, team_wordmark) %>%
  filter(team_abbr != 'LAR')

#Scrape table
url <- paste0( "https://www.sportsoddshistory.com/nfl-win/?y=",2022,"&sa=nfl&t=win&o=t")
content <- read_html(url)

#Grab Table
my_table_loaded <- content %>% html_table()
my_table_reg <- my_table_loaded[[1]]

#Clean df. Use Team names to grab abbreviations. Add Cleveland 8.5
df <- my_table_reg %>%
  mutate(season = 2022) %>%
  left_join(team_info, by = c('Team' = 'team_name')) %>%
  select(season, Team, team_abbr, ewin = `Win Total`) %>%
  mutate(team_abbr = case_when(str_detect(Team,'Washington') ~ 'WAS',
                               str_detect(Team,'Rams') ~ 'LA',
                               TRUE ~ team_abbr)) %>%
  mutate(ewin = ifelse(team_abbr == 'CLE', 8.5, ewin))

#Get Last Rel Perf, Last SOS, Last PFF
rel_perf_load <- read.csv("rel_perf.csv") %>%
  filter(season == 2021) %>%
  select(-season)

last_sos_load <- read.csv("sos_wins.csv") %>%
  filter(season == 2021) %>%
  select(team_abbr, act_sos)

last_pff_load <- read.csv("team_qb_grades.csv") %>%
  filter(season == 2021) %>%
  select(-season)

#Get expected sos
sos_df <- nflreadr::load_schedules(2022) %>%
  nflreadr::clean_homeaway() %>%
  select(team, opponent) %>%
  left_join(df %>% select(team_abbr, ewin), by = c('opponent' = 'team_abbr')) %>%
  group_by(team) %>%
  summarise(exp_sos = mean(ewin/17))

#Get expected PFF QB Grade
pff_loaded <- read.csv("PFF_Data/passing_summary-2021.csv") %>%
  select(pff_id = player_id, grades_offense)

#NFL verse IDs anf PFF IDs
ids_loaded <- nflreadr::load_ff_playerids() %>%
  mutate(pff_id = as.integer(pff_id)) %>%
  select(name, gsis_id, pff_id)

qb_df <- nflreadr::load_schedules(2022) %>%
  nflreadr::clean_homeaway() %>%
  filter(week == 1) %>%
  select(team, team_qb_name, team_qb_id) %>%
  left_join(ids_loaded, by = c('team_qb_id' = 'gsis_id')) %>%
  left_join(pff_loaded, by = 'pff_id') %>%
  select(team, grades_offense)

#Gather data
df_22 <- rel_perf_load %>%
  left_join(last_sos_load, by = c('team' = 'team_abbr')) %>%
  left_join(sos_df, by = 'team') %>%
  left_join(last_pff_load, by = c('team' = 'team_name')) %>%
  left_join(qb_df, by = 'team')

colnames(df_22) <- c('team','last_rel_perf', 'last_sos', 'exp_sos',
                     'last_grade', 'last_QBgrade')

#Load model
my_rf <- read_rds("my_rf_model.rds")

#2022 Prediction
df_22$pred_wins <- predict(my_rf, df_22) * 17
final_df <- df_22 %>%
  left_join(team_info %>% select(-team_name), by = c('team' = 'team_abbr')) %>%
  group_by(team_division) %>%
  mutate(div_rank = row_number(-pred_wins)) %>%
  ungroup() %>%
  mutate(div_rank = ifelse(div_rank == 1, 1, 2)) %>%
  group_by(team_conf) %>%
  mutate(team_seed = rank(order(order(div_rank,-pred_wins)))) %>%
  ungroup() %>%
  arrange(team_seed) %>%
  filter(team_seed < 8) %>%
  mutate(seed_summ = case_when(team_seed %in% c(1,2,3,4) ~ paste0(team_division,' Champ'),
                               TRUE ~ paste0('Wildcard #', (team_seed - 4)))) %>%
  mutate(team_seed = paste0('#',team_seed,' Seed')) %>%
  select(team_wordmark, seed_summ, team_conference_logo, team_seed)

#Data Viz
gt(final_df, groupname_col = 'team_conference_logo') %>%
  gtExtras::gt_img_rows(team_wordmark) %>%
  text_transform(
    locations = cells_row_groups(),
    fn = function(x) {
      lapply(x, function(x) {
        html(
          web_image(
            url = x,
            height = 35
            )
          )
        })
    }
    ) %>%
  gtExtras::gt_merge_stack(col1 = team_wordmark, col2 = team_seed,
                                 palette = c('black','black')) %>%
  tab_header(title = '2022 NFL Playoff Seeding Prediction') %>%
  tab_options(column_labels.hidden = T) %>%
  cols_align(align = 'center') %>%
  tab_options(row_group.as_column = F) %>%
  tab_style(style = cell_fill(color = 'red'),
            locations = cells_row_groups(1)) %>%
  tab_style(style = cell_fill(color = 'blue'),
            locations = cells_row_groups(2)) %>%
  gtExtras::gt_theme_538() %>%
  tab_source_note('Prediction by Random Forest model created by @Timboslice003') %>%
  tab_options(source_notes.font.size = 16,
              heading.align = 'center',
              heading.title.font.size = 24,
              heading.background.color = 'lightgrey',
              source_notes.background.color = 'lightgrey') %>%
  gtsave('2022_pred.png', expand = 10)

#Fix image cropping
magick::image_write(magick::image_trim(magick::image_read('2022_pred.png')),
                    '2022_wo_border.png', 
                    format = 'png')
