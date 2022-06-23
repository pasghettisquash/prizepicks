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

qbweekly<-offenseraw %>% 
  dplyr::select(home_team, away_team, week,game_id,posteam,defteam,td_team,passer_player_id,passer_player_name,  
                pass_attempt, pass_touchdown, incomplete_pass,yards_after_catch, rushing_yards, passing_yards,yards_gained) %>%
  dplyr::filter(!is.na(passer_player_name)) %>%
  dplyr::filter(week == 13) %>% 
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

str(qbweekly)
write.csv(qbweekly, "thisweek.csv")