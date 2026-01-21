library(nflfastR)
library(tidyverse)
library(readr)

pbp_r <- load_pbp(2025)

# --- Helper: Points Allowed ---
# Aggregates the final score of the opponent (posteam) for every game the defense played
points_allowed_df <- pbp_r %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam, game_id) %>%
  summarize(
    opponent_final_score = last(posteam_score),
    .groups = "drop_last"
  ) %>%
  group_by(defteam) %>%
  summarize(total_points_allowed = sum(opponent_final_score))

# --- Helper: Score Differential ---
# Calculated from the Defense's perspective (Def Score - Off Score)
score_diff_df <- pbp_r %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam, game_id) %>%
  summarize(
    # Current score difference at the end of the game
    game_diff = last(defteam_score) - last(posteam_score),
    .groups = "drop_last"
  ) %>%
  group_by(defteam) %>%
  summarize(avg_score_diff = mean(game_diff, na.rm = TRUE))

# --- Helper: QB Scrambles Allowed ---
qb_scrambles_def <- pbp_r %>%
  filter(qb_scramble == 1 & !is.na(yards_gained)) %>%
  group_by(defteam) %>%
  summarize(
    scramble_yards_allowed = sum(yards_gained, na.rm = TRUE),
    scramble_attempts_faced = n(),
    avg_scramble_yards_allowed = scramble_yards_allowed / scramble_attempts_faced,
    .groups = "drop"
  )

# --- Main Defense Aggregation ---
defense <- pbp_r %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam) %>%
  summarize(
    # 1. GENERAL CONTEXT
    games = n_distinct(game_id),
    total_plays_faced = n(),
    # Sum of points added by the opponent while this team was on defense
    points_allowed_calc = sum(posteam_score_post - posteam_score, na.rm = TRUE), 
    
    # 2. EFFICIENCY (Lower is Better for Defense)
    epa_allowed = mean(epa, na.rm = TRUE),
    success_rate_allowed = mean(success, na.rm = TRUE),
    yards_allowed_per_play = mean(yards_gained, na.rm = TRUE),
    turnovers_forced = sum(interception + fumble_lost, na.rm = TRUE),
    
    # 3. PASS DEFENSE
    pass_yds_allowed_pg = sum(passing_yards, na.rm = TRUE) / games,
    pass_tds_allowed = sum(pass_touchdown, na.rm = TRUE),
    comp_pct_allowed = sum(complete_pass, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    cpoe_allowed_avg = mean(cpoe, na.rm = TRUE),
    yac_allowed_avg = mean(yards_after_catch, na.rm = TRUE),
    # Rate of explosive passes allowed (20+ yards)
    explosive_pass_rate_allowed = sum(passing_yards >= 20, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    deep_comp_rate_allowed = sum(complete_pass == 1 & pass_length == 'deep', na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    
    # 4. PRESSURE & DISRUPTION (Higher is Better)
    sacks_forced = sum(sack, na.rm = TRUE),
    qb_hit_pct = mean(qb_hit, na.rm = TRUE),
    tfl_forced_rate = sum(tackled_for_loss, na.rm = TRUE) / sum(rush_attempt, na.rm = TRUE),
    
    # 5. RUSH DEFENSE
    rush_yds_allowed_pg = sum(rushing_yards, na.rm = TRUE) / games,
    rush_tds_allowed = sum(rush_touchdown, na.rm = TRUE),
    explosive_run_rate_allowed = sum(rushing_yards >= 10, na.rm = TRUE) / sum(rush_attempt, na.rm = TRUE),
    # Gap specific yards allowed
    yds_allowed_tackle = sum(rushing_yards[run_gap == "tackle"], na.rm = TRUE),
    yds_allowed_guard = sum(rushing_yards[run_gap == "guard"], na.rm = TRUE),
    yds_allowed_end = sum(rushing_yards[run_gap == "end"], na.rm = TRUE),
    
    # 6. SITUATIONAL DEFENSE
    third_down_allowed_pct = mean(third_down_converted[down == 3], na.rm = TRUE),
    # Red Zone Efficiency: How often opponent scores TD when in RZ
    rz_td_allowed_efficiency = sum(touchdown[yardline_100 <= 20], na.rm = TRUE) / sum(yardline_100 <= 20, na.rm = TRUE),
    penalties_committed_pg = sum(penalty, na.rm = TRUE) / games,
    
    .groups = "drop"
  ) %>%
  # Sort by EPA Allowed Ascending (Best Defenses [lowest EPA] at the top)
  arrange(epa_allowed) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# --- Joins ---
defense <- defense %>%
  left_join(points_allowed_df, by = "defteam") %>%
  left_join(score_diff_df, by = "defteam") %>%
  left_join(qb_scrambles_def, by = "defteam")

# --- Export ---
write_csv(defense, "Data/defense_stats.csv")
