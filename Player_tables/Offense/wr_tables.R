library(nflfastR)
library(tidyverse)
library(readr)

pbp_r <- load_pbp(2025)

pbp_pass <- pbp_r%>%
  filter(pass == 1 & !is.na(receiving_yards))

wr <- pbp_pass%>%
  group_by(receiver_id, receiver) %>%
  summarize(
    team = first(posteam),
    games_played = n_distinct(game_id), 
    receptions = n(),
    total_rec_yds = sum(receiving_yards, na.rm = TRUE),
    ypr = total_rec_yds / receptions,
    rec_success = sum(success, na.rm = TRUE), 
    rec_success_rate = (rec_success / receptions) * 100,
    tds = sum(pass_touchdown, na.rm = TRUE),
    
    explosive_receptions = sum(receiving_yards >= 20, na.rm = TRUE),
    
    d3_rec_pct = (sum(down == 3, na.rm = TRUE) / receptions) *100,
    d4_rec_pct = (sum(down == 4, na.rm = TRUE) / receptions) *100,
    
    fumbles = sum(fumble == 1, na.rm = TRUE),
    
    avg_wpa = mean(wpa, na.rm = TRUE),
    
    .groups = "drop"
  )%>%
  filter(receptions >= 25 & !is.na(receiver))

touches_by_down <- pbp_r %>%
  group_by(down, receiver)%>%
  filter(receiver %in% wr$receiver, !is.na(down))%>%
  summarize(
    rec_targets = sum(pass == 1, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(names_from = down, values_from = rec_targets, names_prefix = "receptions_down_")%>%
  mutate(
    receiver = as.character(receiver)
  )
wr <- wr %>%
  left_join(touches_by_down, by = "receiver")

wr <- wr%>%
  replace_na(list(receptions_down_1 = 0, receptions_down_2 = 0, receptions_down_3 = 0, receptions_down_4 = 0))
  
wr <- wr %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_csv(wr, "Data/wr_stats.csv")
