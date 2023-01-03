library(tidyverse)
library(randomForest)

#Load csvs
wins <- read.csv('wins_data.csv')
rel_perf <- read.csv('rel_perf.csv')
sos <- read.csv('sos_wins.csv')
team_grades <- read.csv('team_qb_grades.csv')
lastQB_df <- read.csv('lastPFF.csv')


#Create Final DF for model
my_df <- rel_perf %>%
  select(team, season) %>%
  mutate(last_season = season - 1) %>%
  left_join(rel_perf, by = c('last_season' = 'season', 'team')) %>%
  rename(last_rel_perf = rel_perf) %>%
  left_join(sos %>% select(season, team = team_abbr, act_sos),
            by = c('last_season' = 'season', 'team')) %>%
  rename(last_sos = act_sos) %>%
  left_join(team_grades,
            by = c('last_season' = 'season', 'team' = 'team_name')) %>%
  rename(last_grade = team_grade) %>%
  left_join(sos %>% select(season, team = team_abbr, exp_sos),
            by = c('season', 'team')) %>%
  left_join(lastQB_df %>% select(team, season, last_QBgrade = last_grade), 
            by = c('season', 'team')) %>%
  left_join(wins %>% select(season, team, wp),
            by = c('season', 'team'))

#Remove NAs from data
model_ready <-my_df[complete.cases(my_df),] 

#Set seed for continuity
set.seed(26)
##### Split Data in Train, Validate, Test sets #####
#Training and Validation Data
training_rows <- sample(1:nrow(model_ready),0.7*nrow(model_ready),replace = F)
training_data <- model_ready[training_rows,]

#Split Validation Data
validation_rows <- sample(1:nrow(training_data),0.3*nrow(training_data),replace=F)
validation <- training_data[validation_rows,]

#Split Train Data
train <- training_data[-validation_rows,]

#Test Data
model_test_data <- model_ready[-training_rows,]
dim(model_test_data)  
dim(validation)
dim(train)  

#Model
rf1 <- randomForest(data = train, wp ~ last_rel_perf + last_sos + 
               exp_sos + last_grade + last_QBgrade,
             mtry = 4, ntree = 1000)

varImpPlot(rf1)
train$pred_wp <- predict(rf1,train)
plot(train$wp,(train$wp - train$pred_wp))
##### Predict Model #####
validation$pred_wp <- predict(rf1,validation)
model_test_data$pred_wp <- predict(rf1,model_test_data)
cor(validation$wp,validation$pred_wp)
cor(model_test_data$wp,model_test_data$pred_wp)

test_df <- model_test_data %>%
  mutate(wins = ifelse(season == 2021,round(wp*17,1),round(wp*16,1)),
         pred_wins = ifelse(season == 2021,round(pred_wp*17,1),round(pred_wp*16,1)))

test_df %>% ggplot(aes(x = pred_wins, wins)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = 'Predicted Wins',
       y = 'Actual Wins') +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 16, hjust = .5),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 13, face = 'bold', color = 'black'),
        plot.background = element_rect('white'),
        panel.border = element_rect(fill = 'transparent', 'black'),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black'))
#root mean squared error function
rmse <- function(actual, predicted){
  return(round(sqrt(mean((actual-predicted)^2, na.rm=T)),1))
}

#Model check
cor(validation$wp,validation$pred_wp)
cor(model_test_data$wp,model_test_data$pred_wp)
cor(test_df$wins,test_df$pred_wins)
rmse(test_df$wins,test_df$pred_wins)   

#2021 results
res_2021 <- model_ready %>%
  filter(season == 2021) %>%
  mutate(wins = round(wp*17,1))

res_2021$pred_wp = predict(rf1,res_2021)
res_2021$pred_wins = round(res_2021$pred_wp*17,2)
res_2021$diff = res_2021$pred_wins - res_2021$wins
rmse(res_2021$wins,res_2021$pred_wins)

team_info <- nflreadr::load_teams() %>%
  select(team_abbr, team_logo_wikipedia)

#Save model
saveRDS(rf1, 'my_rf_model.rds')

#Data Viz
big6 <- res_2021 %>%
  arrange(diff) %>%
  slice(1:3,30:32) %>%
  left_join(team_info, by = c('team' = 'team_abbr')) %>%
  select(team_logo_wikipedia, pred_wins, wins, diff)

big6 %>% gt::gt() %>%
  gtExtras::gt_img_rows(columns = team_logo_wikipedia) %>%
  gt::cols_label(team_logo_wikipedia = '',
                 pred_wins = 'Predicted Wins',
                 wins = 'Actual Wins',
                 diff = 'Difference') %>%
  gt::cols_align('center') %>%
  gtExtras::gt_theme_538()

