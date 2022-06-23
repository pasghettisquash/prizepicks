install.packages("tidyverse")
install.packages("DBI")
install.packages("RSQLite")
install.packages("purrr")
install.packages("nflreadr")
install.packages("gsisdecoder")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(DBI)
library(gsisdecoder)
library(RSQLite)
library(purrr)
library(nflreadr)
library(scales)

options(scipen = 9999)

##data<-load_pbp(2021)##fast load of 2021 pbp - not set up as DB
##field_descriptions ## getting field drescriptions - below writes to CSV
##write.csv(field_descriptions, "fielddescriptions.csv")

##Updating DB and creating connection 
update_db()

connection <- DBI::dbConnect(RSQLite::SQLite(), "./pbp_db")
DBI::dbListTables(connection)
pbp_db <- dplyr::tbl(connection, "nflfastR_pbp")
  
DBI::dbListFields(connection, "nflfastR_pbp") %>%
  utils::head(10)

##End Update / Connections
########################
### Roster
qbroster<-
  nflreadr::load_rosters(2021) %>% 
  dplyr::select(
    full_name,
    first_name,
    last_name,
    team,
    college,
    status,
    position,
    height,
    weight,
    college,
    gsis_id,
    espn_id,
    sportradar_id,
    yahoo_id,
    rotowire_id,
    pff_id,
    pfr_id,
    fantasy_data_id,
    sleeper_id,
    years_exp
  ) %>% 
  dplyr::filter(position == "QB") %>% 
  dplyr::rename(qb_id = gsis_id) %>% 
  collect()

str(qbroster)

## End Roster
#########################

#########################
### Schedule
 schedule<-
  nflreadr::load_schedules(2021)

schedule %>% 
  dplyr::select(
    game_id,
    season,
    game_type,
    week,
    gameday,
    weekday,
    gametime,
    away_team,
    home_team,
    location,
    away_rest,
    home_rest,
    away_moneyline,
    home_moneyline,
    spread_line,
    away_spread_odds,
    home_spread_odds,
    total_line,
    under_odds,
    over_odds,
    div_game,
    roof,
    surface,
    temp,
    wind,
    away_qb_id,
    home_qb_id,
    referee,
    stadium_id,
    stadium,
    away_score,
    home_score,
    result,
    total,
    overtime,
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    away_qb_name,
    home_qb_name,
    away_coach,
    home_coach
  ) %>% 
  collect()

str(schedule)
write.csv(schedule, "schedule.csv")

### End Schedule
#########################

#########################
### 2021 PbP

offenseraw<-pbp_db %>% 
  dplyr::select(
    play_id,
    game_id,
    home_team,
    away_team,
    season_type,
    week,
    posteam,
    posteam_type,
    defteam,
    side_of_field,
    yardline_100,
    game_date,
    game_half,
    drive,
    sp,
    qtr,
    down,
    goal_to_go,
    yrdln,
    ydstogo,
    desc,
    play_type,
    yards_gained,
    qb_dropback,
    qb_scramble,
    pass_length,
    pass_location,
    air_yards,
    yards_after_catch,
    run_location,
    run_gap,
    td_team,
    td_player_name,
    td_player_id,
    ep,
    epa,
    air_epa,
    yac_epa,
    comp_air_epa,
    comp_yac_epa,
    first_down_rush,
    first_down_pass,
    incomplete_pass,
    interception,
    tackled_for_loss,
    fumble_lost,
    qb_hit,
    rush_attempt,
    pass_attempt,
    sack,
    touchdown,
    pass_touchdown,
    rush_touchdown,
    return_touchdown,
    extra_point_attempt,
    two_point_attempt,
    complete_pass,
    passer_player_id,
    passer_player_name,
    passing_yards,
    receiver_player_id,
    receiver_player_name,
    receiving_yards,
    rusher_player_id,
    rusher_player_name,
    rushing_yards,
    interception_player_id,
    interception_player_name,
    fumbled_1_team,
    fumbled_1_player_id,
    fumbled_1_player_name,
    season,
    cp,
    cpoe,
    series_success,
    series_result,
    start_time,
    time_of_day,
    stadium,
    weather,
    nfl_api_id,
    play_type_nfl,
    result,
    total,
    spread_line,
    total_line,
    div_game,
    roof,
    temp,
    wind,
    success,
    passer,
    rusher,
    receiver,
    pass,
    rush,
    first_down,
    passer_id,
    rusher_id,
    receiver_id,
    name,
    id,
    fantasy_player_name,
    fantasy_player_id,
    fantasy,
    fantasy_id) %>% 
  dplyr::filter(play_type %in% c("pass","run")) %>% 
  dplyr::filter(season ==2021) %>% 
  dplyr::collect()
  
str(offenseraw)

### End 2021 PbP
#########################

#########################
### DEFENSE WEEKLY TOTALS - This block is used to calculate weekly defense totals

defenseweekly<-offenseraw %>% 
  dplyr::select(home_team, away_team, week,posteam,defteam,td_team, yards_after_catch, rushing_yards, passing_yards,yards_gained) %>%
  dplyr::group_by(home_team, away_team, week, defteam) %>%
  dplyr::mutate(pts_scored = 
                  case_when(td_team==posteam ~6, 
                            TRUE ~0)
                ) %>% 
  dplyr::summarise(
    yac_tw = sum (yards_after_catch, na.rm = TRUE), 
    rushing_yards_tw = sum(rushing_yards, na.rm = TRUE), 
    passing_yards_tw = sum (passing_yards, na.rm = TRUE), 
    yards_given_up_tw = sum (yards_gained, na.rm = TRUE),
    pts_given_up_tw = sum (pts_scored)
    
    ) %>% 
  dplyr::collect()

defenseweekly
###END DEFENSE WEEKLY TOTALS
############################

############################
### DEFENSE SEASON TOTALS - This block is used to calculate season defense totals
defenseseason<-defenseweekly %>% 
  dplyr::group_by(defteam) %>%
  dplyr::summarise(
    yac_s = sum (yac_tw, na.rm = TRUE), 
    rushing_yards_s = sum(rushing_yards_tw, na.rm = TRUE), 
    rushing_yards_per_game = mean (rushing_yards_tw, na.rm = TRUE),
    passing_yards_s = sum (passing_yards_tw, na.rm = TRUE), 
    passing_yards_per_game = mean (passing_yards_tw, na.rm = TRUE),
    yards_given_up_s = sum (yards_given_up_tw, na.rm = TRUE),
    yards_given_up_per_game = mean (yards_given_up_tw, na.rm = TRUE),
    pts_given_up_s = sum (pts_given_up_tw, na.rm = TRUE),
    pts_given_up_per_game = mean (pts_given_up_tw, na.rm = TRUE)
  ) %>% 

  dplyr::collect()

defenseseason

###END DEFENSE SEASON TOTALS


###DEFENSE RANKS - this block used to calculate ranks for defenses
defenseseasonranks<- defenseseason %>% 
  dplyr::mutate(yac_s_rank = dense_rank(yac_s),
                rushing_yards_s_rank = dense_rank(rushing_yards_s),
                passing_yards_s_rank = dense_rank(passing_yards_s),
                yards_given_up_s_rank = dense_rank(yards_given_up_s),
                pts_given_up_s_rank = dense_rank(pts_given_up_s)
  ) %>% 
  dplyr::rowwise() %>% 
                mutate(average_season_ranks = mean (c(rushing_yards_s_rank, passing_yards_s_rank, yards_given_up_s_rank, pts_given_up_s_rank))
                ) %>% 
  dplyr::arrange(average_season_ranks) %>% 
  dplyr::collect()

defenseseasonranks
write.csv(defenseseasonranks, "defenseseasonranks.csv")

###END DEFENSE RANKS
######################


######################
### QB Weekly Passing Stats - this block used to get weekly QB stats

qbweekly<-offenseraw %>% 
  dplyr::select(home_team, away_team, week,game_id,posteam,defteam,td_team,passer_player_id,passer_player_name,  
                pass_attempt, pass_touchdown, incomplete_pass,yards_after_catch, rushing_yards, passing_yards,yards_gained) %>%
  dplyr::filter(!is.na(passer_player_name)) %>%
  dplyr::group_by(passer_player_id, game_id,week, defteam, home_team, away_team) %>%
  dplyr::summarise(
    yac_tw = sum (yards_after_catch, na.rm = TRUE), 
    rushing_yards_tw = sum(rushing_yards, na.rm = TRUE), 
    passing_yards_tw = sum (passing_yards, na.rm = TRUE), 
    yards_gained_tw = sum (yards_gained, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(home_game = 
                  case_when(defteam == away_team ~"Home", defteam == home_team ~"Away")) %>% 
  dplyr::rename(qb_id = passer_player_id) %>% 
  dplyr::collect()

qbweekly
###END OF QB WEEKLY
##################


##################
### QB Season Totals & Career Totals

offensehistoricalgame<-pbp_db %>% 
  dplyr::filter(play_type %in% c("pass","run")) %>% 
  dplyr::filter(!is.na(passer_player_name)) %>% 
  dplyr::select(season,week,game_id,defteam,td_team,passer_player_id,  
                pass_attempt, pass_touchdown, incomplete_pass,yards_after_catch, rushing_yards, passing_yards,yards_gained) %>%
  dplyr::group_by(passer_player_id, season, week, game_id) %>%
  dplyr::summarise(
    yac_tw = sum (yards_after_catch, na.rm = TRUE), 
    rushing_yards_tw = sum(rushing_yards, na.rm = TRUE), 
    passing_yards_tw = sum (passing_yards, na.rm = TRUE), 
    yards_gained_tw = sum (yards_gained, na.rm = TRUE),
    pass_attempts_tw = sum (pass_attempt, na.rm = TRUE),
    pass_touchdowns_tw = sum (pass_touchdown, na.rm = TRUE),
    pass_incompletions_tw = sum (incomplete_pass, na.rm = TRUE)
  ) %>% 

  dplyr::mutate(
    pass_completions_tw = pass_attempts_tw - pass_incompletions_tw, 
    completion_perc_tw = pass_completions_tw / pass_attempts_tw
  ) %>% 
  dplyr::rename(qb_id = passer_player_id) %>% 
  dplyr::collect()

str(offensehistoricalgame)

qbhistoricalseason<-offensehistoricalgame %>% 
  dplyr::group_by(qb_id, season) %>%
  dplyr::summarise(
    yac_season = sum (yac_tw, na.rm=TRUE),
    rushing_yards_season = sum (rushing_yards_tw, na.rm = TRUE),
    passing_yards_season = sum (passing_yards_tw, na.rm = TRUE),
    yards_gained_season = sum (yards_gained_tw, na.rm = TRUE),
    pass_attempts_season = sum (pass_attempts_tw, na.rm = TRUE),
    pass_touchdowns_season = sum (pass_touchdowns_tw, na.rm = TRUE),
    pass_incompletions_season = sum (pass_incompletions_tw, na.rm = TRUE),
    pass_completions_season = sum (pass_completions_tw, na.rm = TRUE),
    number_of_games = n()
  ) %>% 
  dplyr::mutate(
    completion_perc_season = pass_completions_season / pass_attempts_season,
    passing_yards_per_game = passing_yards_season / number_of_games,
    pass_attempts_per_game = pass_attempts_season / number_of_games,
    pass_touchdowns_per_game = pass_touchdowns_season / number_of_games,
    pass_incompletions_per_game = pass_incompletions_season / number_of_games,
    pass_completions_per_game = pass_completions_season / number_of_games
  ) %>% 
  collect()

qbcareer<-qbhistoricalseason %>% 
  dplyr::group_by(qb_id) %>% 
  dplyr::summarise(
    rushing_yards_career = sum (rushing_yards_season, na.rm = TRUE),
    passing_yards_career = sum (passing_yards_season, na.rm = TRUE),
    yards_gained_career = sum (yards_gained_season, na.rm = TRUE),
    pass_attempts_career = sum (pass_attempts_season, na.rm = TRUE),
    pass_touchdowns_career = sum (pass_touchdowns_season, na.rm = TRUE),
    pass_incompletions_career = sum (pass_incompletions_season, na.rm = TRUE),
    pass_completions_career = sum (pass_completions_season, na.rm = TRUE),
    number_of_games_career = sum (number_of_games, na.rm = TRUE)
    ) %>% 
  dplyr::mutate(
    completion_perc_career = pass_completions_career / pass_attempts_career,
    passing_yards_per_game_career = passing_yards_career / number_of_games_career,
    pass_attempts_per_game_career = pass_attempts_career / number_of_games_career,
    pass_touchdowns_per_game_career = pass_touchdowns_career/ number_of_games_career,
    pass_incompletions_per_game_career = pass_incompletions_career / number_of_games_career,
    pass_completions_per_game_career = pass_completions_career/ number_of_games_career
  ) %>% 
  collect()

head(qbcareer)


str(offensehistoricalgame)
write.csv(qbweekly, "qbweekly.csv")

str(schedule)
###End QB Season Totals and Career Totals
##############################

##############################
###Model Framework - joining qb historical, schedule, defense

modelframeworkhome<- 
  dplyr::inner_join(schedule, qbroster, by= c("home_team" = "team"), keep = TRUE) %>% 
  collect()

modelframeworkaway<-
  dplyr::inner_join(schedule, qbroster, by= c("away_team" = "team"), keep = TRUE) %>%
  collect()


modelframework<-
  dplyr::union(modelframeworkaway, modelframeworkhome) %>% 
  dplyr::filter(week < 14) %>% 
  collect()


modelframework<-
  dplyr::left_join(modelframework, qbweekly, by = c("game_id", "qb_id")) %>% 
  collect()

mo

str(modelframework)

modelframework<-
  dplyr::left_join(modelframework, qbcareer, by = "qb_id") %>% 
  collect()


qbthisseason<-qbhistoricalseason %>% 
  dplyr::filter(season == 2021) %>% 
  collect()

modelframework<-
  dplyr::left_join(modelframework, qbthisseason, by = "qb_id") %>% 
  collect()

str(modelframework)

###End Join
####################################


####################################
### Final Model Prep

finalmodel<- modelframework %>% 
  dplyr::select(
game_id,
week.x,
gameday,
weekday,
gametime,
away_team.x,
home_team.x,
location,
away_rest,
home_rest,
away_moneyline,
home_moneyline,
spread_line,
away_spread_odds,
total_line,
div_game,
roof,
surface,
temp,
wind,
stadium_id,
stadium,
full_name,
team,
qb_id,
years_exp,
yac_tw,
rushing_yards_tw,
passing_yards_tw,
yards_gained_tw,
rushing_yards_career,
passing_yards_career,
yards_gained_career,
pass_attempts_career,
pass_touchdowns_career,
pass_incompletions_career,
pass_completions_career,
number_of_games_career,
completion_perc_career,
passing_yards_per_game_career,
pass_attempts_per_game_career,
pass_touchdowns_per_game_career,
pass_incompletions_per_game_career,
pass_completions_per_game_career,
yac_season,
rushing_yards_season,
passing_yards_season,
yards_gained_season,
pass_attempts_season,
pass_touchdowns_season,
pass_incompletions_season,
pass_completions_season,
number_of_games,
completion_perc_season,
passing_yards_per_game,
pass_attempts_per_game,
pass_touchdowns_per_game,
pass_incompletions_per_game,
pass_completions_per_game
) %>% 
  dplyr::mutate(qb_home = 
                  case_when(team == home_team.x ~1, team == away_team.x ~0)) %>% 
  dplyr::mutate(who_favored = 
                  case_when (spread_line > 0 ~"home", spread_line < 0 ~"away")) %>% 
  dplyr::mutate(qb_spread = 
                 case_when ((qb_home == 1 & who_favored == "home") ~spread_line,
                            (qb_home == 1 & who_favored == "away") ~spread_line,
                            (qb_home == 0 & who_favored == "home") ~(spread_line*-1),
                            (qb_home == 0 & who_favored == "away") ~(spread_line*-1)),
                 ) %>% 
  dplyr::mutate(opponent = 
                  case_when (team == home_team.x ~away_team.x, 
                             team == away_team.x ~home_team.x)
                ) %>% 
  collect()
str(finalmodel)
### last join to add defense ranks - must be after above mutate
finalmodel<-
  dplyr::left_join(finalmodel, defenseseasonranks, by = c("opponent" = "defteam")) %>% 
  collect()

finalmodel<-finalmodel %>% 
  dplyr::select(
    game_id,
    week.x,
    full_name,
    team,
    opponent,
    qb_id,
    away_team.x,
    home_team.x,
    away_rest,
    home_rest,
    away_moneyline,
    home_moneyline,
    spread_line,
    away_spread_odds,
    total_line,
    div_game,
    yac_tw,
    rushing_yards_tw,
    passing_yards_tw,
    yards_gained_tw,
    rushing_yards_career,
    passing_yards_career,
    rushing_yards_season,
    passing_yards_season,
    yards_gained_season,
    pass_attempts_season,
    pass_touchdowns_season,
    pass_incompletions_season,
    pass_completions_season,
    number_of_games,
    completion_perc_season,
    passing_yards_per_game.x,
    pass_attempts_per_game,
    pass_touchdowns_per_game,
    pass_incompletions_per_game,
    pass_completions_per_game,
    yards_gained_career,
    pass_attempts_career,
    pass_touchdowns_career,
    pass_incompletions_career,
    pass_completions_career,
    number_of_games_career,
    completion_perc_career,
    passing_yards_per_game_career,
    pass_attempts_per_game_career,
    pass_touchdowns_per_game_career,
    pass_incompletions_per_game_career,
    pass_completions_per_game_career,
    yac_season,
    yac_s,
    rushing_yards_s,
    rushing_yards_per_game,
    passing_yards_s,
    passing_yards_per_game.y,
    yards_given_up_s,
    yards_given_up_per_game,
    pts_given_up_s,
    pts_given_up_per_game,
    yac_s_rank,
    rushing_yards_s_rank,
    passing_yards_s_rank,
    yards_given_up_s_rank,
    pts_given_up_s_rank,
    average_season_ranks,
    gameday,
    weekday,
    gametime,
    location,
    roof,
    surface,
    temp,
    wind,
    stadium_id,
    stadium,
    years_exp,
    qb_home,
    who_favored,
    qb_spread
  ) %>%
  collect()


str(finalmodel)
write.csv(finalmodel, "finalmodel.csv")
### End Final Model Prep
####################################


####################################
### Multi Regression


### Creating Training and Test Data Sets

trainmodel<- finalmodel %>% 
  dplyr::filter (week.x < 13) %>% 
  collect()

str(trainmodel)

week13model <- finalmodel %>% 
  dplyr::filter(week.x == 13) %>% 
  collect()

str(week12model)

### End Creating training and test data

### Multi Regression

model<-lm(passing_yards_tw ~ away_rest
          + home_rest
          + total_line
          + div_game
          + years_exp
          + completion_perc_career
          + passing_yards_per_game_career
          + pass_attempts_per_game_career
          + pass_touchdowns_per_game_career
          + pass_incompletions_per_game_career
          + pass_completions_per_game_career
          + completion_perc_season
          + passing_yards_per_game.x
          + pass_attempts_per_game
          + pass_touchdowns_per_game
          + pass_incompletions_per_game
          + pass_completions_per_game
          + qb_home
          + qb_spread
          ,data = trainmodel)

summary(model)
coef(model)
trainoutput<-cbind(trainmodel, model)

write.csv(model, "model.csv")

### Testing
?predict

week13predict <- predict.lm(model, newdata = week13model)
predictoutput<-cbind(week13model, week13predict)
write.csv(predictoutput, "week13.csv")


write.csv(coef(model), "modelcoef.csv")


### End Testing