library(nflfastR)
library(tidyverse)
library(readr)

pbp_r <- load_pbp(2025) %>% filter(season_type == "REG")

points <- pbp_r %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam, game_id) %>%
  summarize(
    total_points = last(posteam_score)
  ) %>%
  arrange(desc(total_points))%>%
  group_by(posteam) %>%
  summarize(points_scored = sum(total_points))

score_diff <- pbp_r %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam, game_id) %>%
  summarize(
    score_diff = posteam_score - defteam_score
  ) %>%
  arrange(desc(score_diff)) %>%
  group_by(posteam) %>%
  summarize(avg_score_diff = mean(score_diff, na.rm = TRUE))

qb_scrambles <- pbp_r %>%
  filter(qb_scramble == 1 & !is.na(yards_gained)) %>%
  group_by(posteam) %>%
  summarize(
    scramble_yards = sum(yards_gained, na.rm = TRUE),
    scramble_attempts = n(),
    avg_scramble_yards = scramble_yards / scramble_attempts,
    .groups = "drop"
  )

offense <- pbp_r %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam) %>%
  summarize(
    # 1. GENERAL CONTEXT
    games = n_distinct(game_id),
    total_plays = n(),
    points_scored = sum(posteam_score_post - posteam_score, na.rm = TRUE), # More accurate point tracking
    
    # 2. EFFICIENCY (The "Headline" Metrics)
    epa_per_play = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    yards_per_play = mean(yards_gained, na.rm = TRUE),
    turnovers = sum(interception + fumble_lost, na.rm = TRUE),
    
    # 3. PASSING METRICS
    pass_yds_pg = sum(passing_yards, na.rm = TRUE) / games,
    pass_tds = sum(pass_touchdown, na.rm = TRUE),
    comp_pct = sum(complete_pass, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    cpoe_avg = mean(cpoe, na.rm = TRUE),
    yac_avg = mean(yards_after_catch, na.rm = TRUE),
    # Optimized: Converted to % for better comparison
    explosive_pass_rate = sum(passing_yards >= 20, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    deep_comp_rate = sum(complete_pass == 1 & pass_length == 'deep', na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    
    # 4. PROTECTIONS & NEGATIVES
    sacks_allowed = sum(sack, na.rm = TRUE),
    qb_hit_pct = mean(qb_hit, na.rm = TRUE),
    tfl_rate = sum(tackled_for_loss, na.rm = TRUE) / sum(rush_attempt, na.rm = TRUE),
    
    # 5. RUSHING METRICS
    rush_yds_pg = sum(rushing_yards, na.rm = TRUE) / games,
    rush_tds = sum(rush_touchdown, na.rm = TRUE),
    explosive_run_rate = sum(rushing_yards >= 10, na.rm = TRUE) / sum(rush_attempt, na.rm = TRUE),
    # Gap specific (Optimized names)
    yds_tackle = sum(rushing_yards[run_gap == "tackle"], na.rm = TRUE),
    yds_guard = sum(rushing_yards[run_gap == "guard"], na.rm = TRUE),
    yds_end = sum(rushing_yards[run_gap == "end"], na.rm = TRUE),
    
    # 6. QB MOBILITY
    scramble_yds = sum(yards_gained[qb_scramble == 1], na.rm = TRUE),
    scramble_atts = sum(qb_scramble, na.rm = TRUE),
    
    # 7. SITUATIONAL
    third_down_pct = mean(third_down_converted[down == 3], na.rm = TRUE),
    # Fixed Logic: TDs / Total RZ Plays (instead of TDs / TDs)
    rz_td_efficiency = sum(touchdown[yardline_100 <= 20], na.rm = TRUE) / sum(yardline_100 <= 20, na.rm = TRUE),
    penalties_pg = sum(penalty, na.rm = TRUE) / games,
    
    .groups = "drop"
  ) %>%
  # Sort by EPA (Best offenses at the top)
  arrange(desc(epa_per_play)) %>%
  # Round all numeric columns to 3 decimal places for readability
  mutate(across(where(is.numeric), ~round(., 3)))

offense <- offense %>%
  left_join(points, by = "posteam") %>%
  left_join(score_diff, by = "posteam") %>%
  left_join(qb_scrambles, by = "posteam") %>%
  # 1. Convert probabilities to percentages
  mutate(across(any_of(c(
    "success_rate", "comp_pct", "explosive_pass_rate", 
    "deep_comp_rate", "qb_hit_pct", "explosive_run_rate", 
    "third_down_pct", "rz_td_efficiency", "tfl_rate"
  )), ~ . * 100)) %>%
  # 2. Round everything for the CSV
  mutate(across(where(is.numeric), ~round(., 3)))

write_csv(offense, "Data/offense_stats.csv")
