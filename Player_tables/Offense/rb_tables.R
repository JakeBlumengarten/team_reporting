library(nflfastR)
library(tidyverse)
library(readr)

pbp_r <- load_pbp(2025) %>% filter(season_type == "REG")

pbp_run <- pbp_r%>%
  filter(rush == 1 & !is.na(rushing_yards))

rbs <- pbp_run %>%
  group_by(rusher_id, rusher) %>%
  summarize(
    team = first(posteam),
    games_played = n_distinct(game_id), 
    carries = n(),
    total_rush_yds = sum(rushing_yards, na.rm = TRUE),
    ypc = total_rush_yds / carries,
    # Simplified success calculation
    rush_success = sum(success, na.rm = TRUE), 
    rush_success_rate = (rush_success / carries) * 100,
    tds = sum(rush_touchdown, na.rm = TRUE),
    
    # NEW: Yards by gap using conditional summing
    yds_at_tackle = sum(rushing_yards[run_gap == "tackle"], na.rm = TRUE),
    yds_at_guard = sum(rushing_yards[run_gap == "guard"], na.rm = TRUE),
    yds_at_end = sum(rushing_yards[run_gap == "end"], na.rm = TRUE),
    
    explosive_runs = sum(rushing_yards >= 10, na.rm = TRUE),
    
    d3_touch_pct = (sum(down == 3, na.rm = TRUE) / carries) *100,
    d4_touch_pct = (sum(down == 4, na.rm = TRUE) / carries) *100,
    
    fumbles = sum(fumble == 1, na.rm = TRUE),
    
    avg_wpa = mean(wpa, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(carries >= 150 & !is.na(rusher))

rbs_catches <- pbp_pass%>%
  filter(complete_pass == 1 & receiver_id %in% rbs$rusher_id)%>%
  group_by(receiver_id, receiver)%>%
  summarize(
    receptions = n(),
    total_rec_yds = sum(yards_gained, na.rm = TRUE),
    ypr = total_rec_yds / receptions,
    rec_success = sum(success == 1, na.rm = TRUE),
    pass_success_rate = rec_success / receptions * 100,
    .groups = "drop"
  )%>%
  filter(receiver_id %in% rbs$rusher_id)

rbs <- rbs %>%
  left_join(rbs_catches, by = c("rusher_id" = "receiver_id"))

touches_by_down <- pbp_r %>%
  group_by(down, rusher)%>%
  filter(rusher %in% rbs$rusher, !is.na(down))%>%
  summarize(
    rush_attempts = sum(rush == 1, na.rm = TRUE),
    rush_yds = sum(rushing_yards, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(names_from = down,
              values_from = c(rush_attempts, rush_yds),
              names_prefix = "d")

touches_by_down[is.na(touches_by_down)] <- 0

rbs <- rbs %>%
  left_join(touches_by_down, by = "rusher")

# rbs <- rbs %>%
#   select(-receiver)%>%
#   rename(player = rusher)

rbs <- rbs %>%
  mutate(across(where(is.numeric), ~round(., 3)))
  
write_csv(rbs, "Data/rb_stats.csv")
