library(nflfastR)
library(nflplotR)
library(cfbfastR)
library(dplyr)
library(tidyr)
library(readr)
library(skimr)

# Load NFL data
nfl_25_schedule <- fast_scraper_schedules(2025) %>%
  select(old_game_id, week, game_type, home_team, home_score, away_team, 
         away_score, gameday, gametime, overtime, roof, surface, temp, 
         wind, home_coach, away_coach, referee, stadium_id, stadium)

nfl_25 <- load_pbp(2025) %>%
  group_by(old_game_id, posteam) %>%
  summarize(
    total_yards = sum(yards_gained, na.rm = TRUE),
    passing_yards = sum(yards_gained[play_type == "pass"], na.rm = TRUE),
    rushing_yards = sum(yards_gained[play_type == "run"], na.rm = TRUE),
    rush_attempts = sum(play_type == "run", na.rm = TRUE),
    pass_attempts = sum(play_type == "pass", na.rm = TRUE),
    completions = sum(complete_pass == 1, na.rm = TRUE),
    incompletions = sum(incomplete_pass == 1, na.rm = TRUE),
    avg_cpoe = mean(cpoe, na.rm = TRUE),
    total_first_downs = sum(first_down == 1, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  filter(posteam != "")

nfl_team_totals <- nfl_25%>%
  group_by(posteam) %>%
  summarize(
    season_total_yards = sum(total_yards, na.rm = TRUE),
    season_passing_yards = sum(passing_yards, na.rm = TRUE),
    season_pass_att = sum(pass_attempts, na.rm = TRUE),
    season_completions = sum(completions, na.rm = TRUE),
    season_incompletions = sum(incompletions, na.rm = TRUE),
    season_rushing_yards = sum(rushing_yards, na.rm = TRUE),
    season_rush_att = sum(rush_attempts, na.rm = TRUE),
    season_avg_cpoe = mean(avg_cpoe, na.rm = TRUE),
    season_total_first_downs = sum(total_first_downs, na.rm = TRUE)
  ) %>%
  ungroup()

nfl_player_totals <- load_pbp(2025) %>%
  group_by(posteam, passer_player_name, rusher_player_name, receiver_player_name) %>%
  summarize(
    total_yards = sum(yards_gained, na.rm = TRUE),
    passing_yards = sum(yards_gained[play_type == "pass"], na.rm = TRUE),
    completions = sum(complete_pass == 1, na.rm = TRUE),
    incompletions = sum(incomplete_pass == 1, na.rm = TRUE),
    rushing_yards = sum(yards_gained[play_type == "run"], na.rm = TRUE)
  ) %>%
  ungroup()

nfl_player_totals <- nfl_player_totals%>%
  pivot_longer(
    cols = c(passer_player_name, rusher_player_name, receiver_player_name),
    names_to = "position",
    values_to = "player_name"
  ) %>%
  filter(player_name != "") %>%
  group_by(posteam, player_name) %>%
  summarize(
    passing_yards = sum(passing_yards, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    total_yards = sum(total_yards, na.rm = TRUE),
    completions = sum(completions, na.rm = TRUE),
    incompletions = sum(incompletions, na.rm = TRUE)
  ) %>%
  ungroup()

# Load CFB data
cfb_25_schedule <- load_cfb_schedules(2025)%>%
  select(game_id, week, season_type, home_team, home_points, away_team, 
         away_points, excitement_index)

cfb_25 <- load_cfb_pbp(2025)%>%
  group_by(game_id, pos_team) %>%
  summarize(
    total_yards = sum(yards_gained, na.rm = TRUE),
    passing_yards = sum(yards_gained[play_type == "Pass Reception"], na.rm = TRUE),
    rushing_yards = sum(yards_gained[play_type == "Rush"], na.rm = TRUE)
  ) %>%
  ungroup()

cfb_player_totals <- load_cfb_pbp(2025) %>%
  group_by(pos_team, passer_player_name, rusher_player_name, receiver_player_name) %>%
  summarize(
    player_total_yards = sum(yards_gained, na.rm = TRUE),
    player_passing_yards = sum(yards_gained[play_type == "Pass Reception"], na.rm = TRUE),
    player_rushing_yards = sum(yards_gained[play_type == "Rush"], na.rm = TRUE)
  ) %>%
  ungroup()

cfb_player_totals <- cfb_player_totals%>%
  pivot_longer(
    cols = c(passer_player_name, rusher_player_name, receiver_player_name),
    names_to = "position",
    values_to = "player_name"
  ) %>%
  filter(player_name != "") %>%
  group_by(pos_team, player_name) %>%
  summarize(
    player_passing_yards = sum(player_passing_yards, na.rm = TRUE),
    player_rushing_yards = sum(player_rushing_yards, na.rm = TRUE),
    player_total_yards = sum(player_total_yards, na.rm = TRUE)
  ) %>%
  ungroup()

# Save data locally
write_csv(nfl_25_schedule, "Data/nfl_25_schedule.csv")
write_csv(nfl_25, "Data/nfl_25.csv")
write_csv(team_totals, "Data/nfl_25_team_totals.csv")
write_csv(nfl_player_totals, "Data/nfl_25_player_totals.csv")
write_csv(cfb_player_totals, "Data/cfb_25_player_totals.csv")
write_csv(cfb_25_schedule, "Data/cfb_25_schedule.csv")
write_csv(cfb_25, "Data/cfb_25.csv")

# Optional: skim the data for a quick overview
skim(nfl_25)
skim(cfb_25)
skim(team_totals)
skim(player_totals)


# End of 01_Data.R
