library(tidyverse)
library(nflfastR)

pbp <- load_pbp(2025)

fourth_down_grouped <- pbp %>%
  # 1. Filter for 4th Down & Actual Attempts
  filter(down == 4, (rush == 1 | pass == 1)) %>%
  
  # 2. Create the Distance Buckets
  mutate(
    dist_bucket = case_when(
      ydstogo <= 3 ~ "1-3 yds",
      ydstogo <= 6 ~ "4-6 yds",
      TRUE         ~ "7+ yds"
    )
  ) %>%
  
  # 3. Group by Team AND the new Bucket
  group_by(posteam, dist_bucket) %>%
  
  # 4. Summarize
  summarise(
    attempts = n(),
    conversions = sum(yards_gained >= ydstogo, na.rm = TRUE),
    # Optional: Calculate percentage right here
    conv_pct = mean(yards_gained >= ydstogo, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(fourth_down_grouped, "Data/fourth_down_conversions.csv")
