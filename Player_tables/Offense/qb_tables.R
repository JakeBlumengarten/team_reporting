library(nflfastR)
library(tidyverse)
library(readr)

pbp_r <- load_pbp(2025) %>% filter(season_type == "REG")

pbp_pass <- pbp_r%>%
  filter(pass == 1 & !is.na(yards_gained))

qbs <- pbp_pass %>%
  group_by(passer_id, passer) %>%
  summarize(
    team = first(posteam),
    games_played = n_distinct(game_id), 
    completions = sum(complete_pass == 1, na.rm = TRUE),
    attempts = n(), 
    comp_pct = completions / attempts * 100, 
    total_air_yds = sum(air_yards, na.rm = TRUE),
    air_yds_per_pass = total_air_yds / attempts,
    avg_cpoe = mean(cpoe, na.rm = TRUE),
    success = sum(success == 1, na.rm = TRUE),
    success_rate = success / completions * 100,
    ints = sum(interception == 1, na.rm = TRUE),
    tds = sum(pass_touchdown == 1, na.rm = TRUE),
    totalepa = sum(epa, na.rm = TRUE),
    epa_per_pass = totalepa / attempts,
    rushing_yards = sum(rushing_yards[passer_id == passer_id], na.rm = TRUE),
    
    rushing_tds_rz = sum(rush_touchdown[passer_id == passer_id & yardline_100 <= 20], na.rm = TRUE),
    passing_yds_rz = sum(yards_gained[passer_id == passer_id & yardline_100 <= 20], na.rm = TRUE),
    pass_attempts_rz = sum(pass[passer_id == passer_id & yardline_100 <= 20], na.rm = TRUE),
    pass_tds_rz = sum(pass_touchdown[passer_id == passer_id & yardline_100 <= 20], na.rm = TRUE),
    completions_rz = sum(complete_pass[passer_id == passer_id & yardline_100 <= 20], na.rm = TRUE),
    comp_pct_rz = completions_rz / pass_attempts_rz * 100,
    
    .groups = "drop"
  ) %>%
  filter(attempts >= 200 & !is.na(passer))

qb_when_hit <- pbp_pass%>%
  filter(qb_hit == 1)%>%
  group_by(passer_id, passer)%>%
  summarize(
    completions = sum(complete_pass == 1, na.rm = TRUE),
    attempts = n(), 
    comp_pct_wh = completions / attempts * 100, 
    total_air_yds_wh = sum(air_yards, na.rm = TRUE),
    air_yds_per_pass_wh = total_air_yds_wh / attempts,
    avg_cpoe_wh = mean(cpoe, na.rm = TRUE),
    success = sum(success == 1, na.rm = TRUE),
    success_rate_wh = success / attempts * 100,
    ints_wh = sum(interception == 1, na.rm = TRUE),
    tds_wh = sum(pass_touchdown == 1, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(passer_id %in% qbs$passer_id)%>%
  select(comp_pct_wh, total_air_yds_wh, air_yds_per_pass_wh, avg_cpoe_wh, success_rate_wh, ints_wh, tds_wh, passer_id)

qbs <- qbs%>%
  left_join(qb_when_hit, by = "passer_id")
  
qbs_downs <- pbp_pass %>%
  group_by(down, passer) %>%
  filter(passer %in% qbs$passer, !is.na(down)) %>%
  summarize(
    pass_attempts = sum(pass == 1, na.rm = TRUE),
    cmp_pct = sum(complete_pass == 1, na.rm = TRUE) / pass_attempts * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = down, values_from = c(pass_attempts, cmp_pct), names_prefix = "d")

qbs <- qbs %>%
  left_join(qbs_downs, by = c("passer" = "passer"))

qbs <- qbs %>%
  mutate(across(where(is.numeric), ~round(., 3)))
                
write_csv(qbs, "Data/qb_stats.csv")
