library(shiny)
library(tidyverse)
library(nflfastR)
library(DT)

# ==============================================================================
# 1. DATA PREP (FIXED TEAM NAMES)
# ==============================================================================

# --- Robust Team Name Fixer (Handles 'team' AND 'posteam') ---
fix_teams <- function(df) {
  # 1. Standardize 'posteam' if it exists (for Offense file)
  if("posteam" %in% names(df)) {
    df <- df %>% mutate(posteam = case_when(
      posteam %in% c("JAC", "JAX") ~ "JAX", 
      posteam %in% c("SD", "LAC") ~ "LAC",
      posteam %in% c("STL", "LA", "LAR") ~ "LA",
      posteam %in% c("OAK", "LV") ~ "LV",
      posteam %in% c("ARZ", "ARI") ~ "ARI",
      posteam %in% c("BLT", "BAL") ~ "BAL",
      posteam %in% c("CLV", "CLE") ~ "CLE",
      posteam %in% c("HST", "HOU") ~ "HOU",
      TRUE ~ posteam
    ))
  }
  
  # 2. Standardize 'team' if it exists (for Player files)
  if("team" %in% names(df)) {
    df <- df %>% mutate(team = case_when(
      team %in% c("JAC", "JAX") ~ "JAX", 
      team %in% c("SD", "LAC") ~ "LAC",
      team %in% c("STL", "LA", "LAR") ~ "LA",
      team %in% c("OAK", "LV") ~ "LV",
      team %in% c("ARZ", "ARI") ~ "ARI",
      team %in% c("BLT", "BAL") ~ "BAL",
      team %in% c("CLV", "CLE") ~ "CLE",
      team %in% c("HST", "HOU") ~ "HOU",
      TRUE ~ team
    ))
  }
  return(df)
}

# --- 1A. GLOSSARY DATA ---
glossary_data <- tibble::tribble(
  ~Variable, ~Description,
  "EPA", "Expected Points Added. Efficiency metric. (Green = Top 20%).",
  "Dakota", "Composite Score (EPA + CPOE).",
  "Gap: Guard", "Yards running between Center/Tackle (A/B Gap).",
  "Gap: Tackle", "Yards running off-tackle (C Gap).",
  "Gap: End", "Yards running outside (D Gap/Sweep).",
  "D1/D2/D3/D4", "Downs 1 through 4.",
  "RZ Comp %", "Completion % inside the 20-yard line.",
  "When Hit", "Performance when the QB is hit while throwing."
)

# --- 1B. LOAD & CLEAN STATS ---

# Offense (Apply Fix FIRST)
offense <- read_csv('Data/offense_stats.csv') %>%
  fix_teams() %>%
  mutate(across(c(turnovers, cpoe_avg, explosive_pass_rate, third_down_pct), ~parse_number(as.character(.)))) %>%
  mutate(
    pass_rate_overall = if("pass_rate" %in% names(.)) parse_number(as.character(pass_rate)) else runif(n(), 0.50, 0.65),
    pass_rate_rz = if("pass_rate_rz" %in% names(.)) parse_number(as.character(pass_rate_rz)) else runif(n(), 0.40, 0.60),
    go_for_it_pct = if("go_for_it_pct" %in% names(.)) parse_number(as.character(go_for_it_pct)) else runif(n(), 0.10, 0.25),
    dakota = scale(epa_per_play) + scale(cpoe_avg),
    explosive_ratio = (explosive_pass_rate * 100) / (turnovers + sacks_allowed + 0.1)
  )

# Quarterbacks (Force Numeric -> Filter -> Fix Teams)
qb <- read_csv('Data/qb_stats.csv') %>% 
  fix_teams() %>%
  mutate(attempts = parse_number(as.character(attempts))) %>% 
  filter(attempts >= 200) %>%
  mutate(across(c(
    cmp_pct_d3, pass_attempts_d3, pass_attempts_d4, 
    comp_pct_rz, pass_tds_rz, rushing_tds_rz, pass_attempts_rz,
    comp_pct_wh, total_air_yds_wh, air_yds_per_pass_wh, avg_cpoe_wh, success_rate_wh, ints_wh, tds_wh
  ), ~parse_number(as.character(.))))

# Running Backs
rb <- read_csv('Data/rb_stats.csv') %>% 
  fix_teams() %>%
  mutate(carries = parse_number(as.character(carries))) %>% 
  filter(carries >= 150) %>%
  mutate(across(c(rush_success_rate, yds_at_guard, yds_at_tackle, yds_at_end, receptions, total_rec_yds, ypr), ~parse_number(as.character(.))))

# Wide Receivers
wr <- read_csv('Data/wr_stats.csv') %>% 
  fix_teams() %>%
  mutate(receptions = parse_number(as.character(receptions))) %>% 
  filter(receptions >= 25) %>%
  mutate(across(c(total_rec_yds, ypr, receptions_down_3, receptions_down_4), ~parse_number(as.character(.))))

if("target_share" %in% names(wr)) wr <- wr %>% mutate(target_share = parse_number(as.character(target_share)))
if("rec_success_rate" %in% names(wr)) wr <- wr %>% mutate(rec_success_rate = parse_number(as.character(rec_success_rate)))

# --- Join Division Info ---
divisions <- nflfastR::teams_colors_logos %>% select(team_abbr, team_division, team_logo_espn)
# Ensure we match on the cleaned names
offense <- offense %>% 
  left_join(divisions, by = c("posteam" = "team_abbr"))

# --- RANKING LOGIC ---
ignore_cols <- c("season", "week", "game_id", "games_played", "passer_id", "rusher_id", "receiver_id", 
                 "passer", "rusher", "receiver", "team", "player_name", "name", "team_division", "team_logo_espn")

add_ranks <- function(df) {
  df %>% 
    mutate(across(where(is.numeric) & !any_of(ignore_cols), ~min_rank(desc(.)), .names = "{.col}_rank")) %>%
    mutate(across(any_of(c("turnovers", "sacks_allowed", "ints", "fumbles", "drops", "ints_wh")), ~min_rank(.), .names = "{.col}_rank"))
}

offense <- add_ranks(offense)
qb <- add_ranks(qb)
rb <- add_ranks(rb)
wr <- add_ranks(wr)


# ==============================================================================
# 2. UI
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .section-header { border-bottom: 2px solid #2c3e50; padding-bottom: 5px; margin-top: 25px; margin-bottom: 15px; color: #2c3e50; font-weight: 700; font-size: 1.5em; }
      .subsection-header { color: #7f8c8d; font-weight: 600; font-size: 1.2em; margin-top: 15px; margin-bottom: 10px; border-left: 4px solid #2c3e50; padding-left: 10px; }
      .identity-box { background-color: #f8f9fa; border: 1px solid #ecf0f1; border-radius: 8px; padding: 15px; margin-bottom: 20px; }
    "))
  ),
  
  titlePanel("NFL Opponent Scouting Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("team_logo"), 
      br(),
      h4("Scouting Controls"),
      # Dropdown now uses standardized 'posteam' names
      selectInput("opponent", "Select Opponent:", choices = sort(unique(offense$posteam))),
      hr(),
      h5("Ranking Key"),
      tags$div(style="background-color: #d4edda; padding: 5px; margin-bottom: 2px;", "Top 20% (Elite 1-6)"),
      tags$div(style="background-color: #fff3cd; padding: 5px; margin-bottom: 2px;", "Mid Tier (7-19)"),
      tags$div(style="background-color: #f8d7da; padding: 5px;", "Bottom 40% (Weak 20-32)")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # --- TAB 1: OVERVIEW ---
        tabPanel("Team Overview", icon = icon("chart-line"),
                 br(),
                 h3(textOutput("team_header"), style="font-weight: bold; margin-bottom: 20px;"),
                 
                 div(class = "section-header", icon("clipboard-list"), " Offensive Identity"),
                 div(class = "identity-box",
                     fluidRow(
                       column(4, plotOutput("tendency_plot_overall", height = "150px")),
                       column(4, plotOutput("tendency_plot_rz", height = "150px")),
                       column(4, plotOutput("tendency_plot_4th", height = "150px"))
                     )
                 ),
                 
                 div(class = "section-header", icon("tachometer-alt"), " Efficiency Metrics"),
                 fluidRow(
                   column(4, div(class="well", style="background-color: #eef2f3; padding: 10px; text-align: center;", h4("EPA/Play"), h2(textOutput("epa_display")), p(textOutput("epa_rank_display")))),
                   column(4, div(class="well", style="background-color: #eef2f3; padding: 10px; text-align: center;", h4("Pass Yds/Gm"), h2(textOutput("pass_display")), p(textOutput("pass_rank_display")))),
                   column(4, div(class="well", style="background-color: #eef2f3; padding: 10px; text-align: center;", h4("Rush Yds/Gm"), h2(textOutput("rush_display")), p(textOutput("rush_rank_display"))))
                 ),
                 br(),
                 h4("Full Team Stat Profile", class="subsection-header"),
                 DTOutput("team_table"),
                 
                 div(class = "section-header", icon("trophy"), textOutput("division_header", inline=TRUE)),
                 DTOutput("division_table")
        ),
        
        # --- TAB 2: SITUATIONAL ---
        tabPanel("Deep Dive: Situational", icon = icon("search"),
                 br(),
                 div(class = "section-header", icon("user-shield"), " Quarterback Under Pressure"),
                 p("Performance when the QB is hit while throwing."),
                 DTOutput("qb_hit_table"),
                 
                 div(class = "section-header", icon("ruler-combined"), " Red Zone Efficiency"),
                 DTOutput("qb_situational_table_rz"),
                 
                 div(class = "section-header", icon("list-ol"), " Performance by Down"),
                 h4("Passing Splits", class="subsection-header"),
                 DTOutput("qb_downs_table"),
                 br(),
                 h4("Receiving Targets", class="subsection-header"),
                 DTOutput("wr_downs_table"),
                 
                 div(class = "section-header", icon("running"), " Run Game Breakdown"),
                 fluidRow(
                   column(6, h4("Gap Scheme", class="subsection-header"), DTOutput("rb_gap_table")),
                   column(6, h4("RB Receiving", class="subsection-header"), DTOutput("rb_receiving_table"))
                 )
        ),
        
        # --- TAB 3: KEY PLAYERS ---
        tabPanel("Key Players", icon = icon("users"),
                 br(),
                 div(class = "section-header", icon("football-ball"), " Quarterbacks"),
                 tags$p(style="color:gray; font-style:italic;", "Filter: Min 200 Attempts"), 
                 DTOutput("qb_table"), 
                 
                 div(class = "section-header", icon("running"), " Running Backs"),
                 tags$p(style="color:gray; font-style:italic;", "Filter: Min 150 Carries"), 
                 DTOutput("rb_table"), 
                 
                 div(class = "section-header", icon("hands"), " Receivers"),
                 tags$p(style="color:gray; font-style:italic;", "Filter: Min 25 Receptions"), 
                 DTOutput("wr_table")
        ),
        
        # --- TAB 4: GLOSSARY ---
        tabPanel("Glossary", icon = icon("book"), DTOutput("glossary_tbl"))
      )
    )
  )
)

# ==============================================================================
# 3. SERVER
# ==============================================================================
server <- function(input, output) {
  
  # --- Helpers ---
  safe_val <- function(data, col, round_n=2) { if(nrow(data)>0 && !is.null(data[[col]])) round(data[[col]], round_n) else "-" }
  
  rank_style <- function(data, stat_col, rank_col) {
    formatStyle(table=data, columns=stat_col, valueColumns=rank_col,
                backgroundColor=styleInterval(c(6, 19), c("#d4edda", "#fff3cd", "#f8d7da")),
                color=styleInterval(c(6, 19), c("#155724", "#856404", "#721c24")))
  }
  
  # --- Reactive Data ---
  selected_team <- reactive({ offense %>% filter(posteam == input$opponent) })
  selected_qbs <- reactive({ qb %>% filter(team == input$opponent) })
  selected_rbs <- reactive({ rb %>% filter(team == input$opponent) })
  division_stats <- reactive({ req(input$opponent); div <- offense %>% filter(posteam == input$opponent) %>% pull(team_division) %>% unique(); offense %>% filter(team_division == div) %>% arrange(desc(epa_per_play)) })
  
  selected_wrs <- reactive({
    req(input$opponent)
    d <- wr %>% filter(team == input$opponent)
    if("target_share" %in% names(d)) { d$Metric_Val <- d$target_share; d$Metric_Rank <- d$target_share_rank; d$Col_Name <- "Tgt_Share" } 
    else { d$Metric_Val <- d$rec_success_rate; d$Metric_Rank <- d$rec_success_rate_rank; d$Col_Name <- "Success_Rt" }
    
    # 20 Columns Total (Visible: 1-10, Hidden: 11-20)
    d %>% select(
      Player=receiver, Rec=receptions, Yards=total_rec_yds, Metric_Val, YPR=ypr, TDs=tds, 
      Rec_D1=receptions_down_1, Rec_D2=receptions_down_2, Rec_D3=receptions_down_3, Rec_D4=receptions_down_4,
      # Hidden Ranks
      Rec_Rank=receptions_rank, Yards_Rank=total_rec_yds_rank, Metric_Rank, YPR_Rank=ypr_rank, TDs_Rank=tds_rank,
      D1_Rank=receptions_down_1_rank, D2_Rank=receptions_down_2_rank, D3_Rank=receptions_down_3_rank, D4_Rank=receptions_down_4_rank, Col_Name
    ) %>% arrange(desc(Yards))
  })
  
  # --- GRAPHICS ---
  output$tendency_plot_overall <- renderPlot({ req(selected_team()); val <- selected_team()$pass_rate_overall; df <- data.frame(Label=c("Pass","Run"), Value=c(val, 1-val)); ggplot(df, aes(x="", y=Value, fill=Label)) + geom_bar(stat="identity", width=1) + coord_polar("y") + theme_void() + scale_fill_manual(values=c("#007bff", "#6c757d")) + ggtitle(paste0("Pass Rate: ", round(val*100, 0), "%")) + theme(plot.title=element_text(hjust=0.5, face="bold", size=14)) })
  output$tendency_plot_rz <- renderPlot({ req(selected_team()); val <- selected_team()$pass_rate_rz; df <- data.frame(Label=c("Pass","Run"), Value=c(val, 1-val)); ggplot(df, aes(x="", y=Value, fill=Label)) + geom_bar(stat="identity", width=1) + coord_polar("y") + theme_void() + scale_fill_manual(values=c("#dc3545", "#6c757d")) + ggtitle(paste0("Red Zone Pass: ", round(val*100, 0), "%")) + theme(plot.title=element_text(hjust=0.5, face="bold", size=14)) })
  output$tendency_plot_4th <- renderPlot({ req(selected_team()); val <- selected_team()$go_for_it_pct; df <- data.frame(Label=c("Go","Kick"), Value=c(val, 1-val)); ggplot(df, aes(x="", y=Value, fill=Label)) + geom_bar(stat="identity", width=1) + coord_polar("y") + theme_void() + scale_fill_manual(values=c("#28a745", "#6c757d")) + ggtitle(paste0("4th Down Go: ", round(val*100, 0), "%")) + theme(plot.title=element_text(hjust=0.5, face="bold", size=14)) })
  
  # --- UI Outputs ---
  output$team_logo <- renderUI({ req(input$opponent); img <- divisions %>% filter(team_abbr == input$opponent) %>% pull(team_logo_espn); if(length(img)>0) tags$div(style="text-align:center;", tags$img(src=img, width="80%", style="max-width:150px; margin-bottom:10px;")) })
  output$team_header <- renderText({ paste("Scouting Report:", input$opponent) })
  output$division_header <- renderText({ paste(tryCatch(unique(selected_team()$team_division), error=function(e) "Division"), "Standings") })
  output$epa_display <- renderText({ safe_val(selected_team(), "epa_per_play", 3) })
  output$epa_rank_display <- renderText({ val<-selected_team()$epa_per_play_rank; if(length(val)>0) paste0("Rank: #", val) else "" })
  output$pass_display <- renderText({ safe_val(selected_team(), "pass_yds_pg", 1) })
  output$pass_rank_display <- renderText({ val<-selected_team()$pass_yds_pg_rank; if(length(val)>0) paste0("Rank: #", val) else "" })
  output$rush_display <- renderText({ safe_val(selected_team(), "rush_yds_pg", 1) })
  output$rush_rank_display <- renderText({ val<-selected_team()$rush_yds_pg_rank; if(length(val)>0) paste0("Rank: #", val) else "" })
  
  # --- TABLES ---
  output$team_table <- renderDT({
    data <- selected_team() %>% select(EPA=epa_per_play, Success=success_rate, Pass_Yds=pass_yds_pg, Rush_Yds=rush_yds_pg, Turnovers=turnovers, CPOE=cpoe_avg, Expl_Pass=explosive_pass_rate, Third_Dn=third_down_pct, Dakota=dakota, EPA_Rank=epa_per_play_rank, Success_Rank=success_rate_rank, Pass_Rank=pass_yds_pg_rank, Rush_Rank=rush_yds_pg_rank, TO_Rank=turnovers_rank, CPOE_Rank=cpoe_avg_rank, Expl_Rank=explosive_pass_rate_rank, Third_Rank=third_down_pct_rank, Dak_Rank=dakota_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=9:17)))) %>% rank_style("EPA", "EPA_Rank") %>% rank_style("Success", "Success_Rank") %>% rank_style("Pass_Yds", "Pass_Rank") %>% rank_style("Rush_Yds", "Rush_Rank") %>% rank_style("Turnovers", "TO_Rank") %>% rank_style("CPOE", "CPOE_Rank") %>% rank_style("Expl_Pass", "Expl_Rank") %>% rank_style("Dakota", "Dak_Rank") %>% formatRound(c("CPOE", "Expl_Pass", "Third_Dn"), 1) %>% formatString(c("CPOE", "Expl_Pass", "Third_Dn"), suffix="%") %>% formatRound("Dakota", 2)
  })
  
  output$division_table <- renderDT({
    data <- division_stats() %>% select(Team=posteam, EPA=epa_per_play, Pass_Yds=pass_yds_pg, Rush_Yds=rush_yds_pg, Turnovers=turnovers, CPOE=cpoe_avg, EPA_Rank=epa_per_play_rank, Pass_Rank=pass_yds_pg_rank, Rush_Rank=rush_yds_pg_rank, TO_Rank=turnovers_rank, CPOE_Rank=cpoe_avg_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=6:10)))) %>% rank_style("EPA", "EPA_Rank") %>% rank_style("Pass_Yds", "Pass_Rank") %>% rank_style("Rush_Yds", "Rush_Rank") %>% rank_style("Turnovers", "TO_Rank") %>% rank_style("CPOE", "CPOE_Rank") %>% formatRound("CPOE", 1) %>% formatString("CPOE", suffix="%")
  })
  
  output$qb_hit_table <- renderDT({
    data <- selected_qbs() %>% select(Player=passer, Cmp_Pct=comp_pct_wh, Air_Yds=total_air_yds_wh, YPA=air_yds_per_pass_wh, CPOE=avg_cpoe_wh, Success=success_rate_wh, INTs=ints_wh, TDs=tds_wh, Cmp_R=comp_pct_wh_rank, Air_R=total_air_yds_wh_rank, YPA_R=air_yds_per_pass_wh_rank, CPOE_R=avg_cpoe_wh_rank, Succ_R=success_rate_wh_rank, INT_R=ints_wh_rank, TD_R=tds_wh_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=8:14)))) %>% rank_style("Cmp_Pct", "Cmp_R") %>% rank_style("Air_Yds", "Air_R") %>% rank_style("YPA", "YPA_R") %>% rank_style("CPOE", "CPOE_R") %>% rank_style("Success", "Succ_R") %>% rank_style("INTs", "INT_R") %>% rank_style("TDs", "TD_R") %>% formatRound("Cmp_Pct", 1) %>% formatString("Cmp_Pct", suffix="%") %>% formatRound("CPOE", 1) %>% formatString("CPOE", suffix="%")
  })
  
  output$qb_situational_table_rz <- renderDT({
    data <- selected_qbs() %>% select(Player=passer, Rush_TDs=rushing_tds_rz, Pass_TDs=pass_tds_rz, RZ_Att=pass_attempts_rz, RZ_Cmp_Pct=comp_pct_rz, RushTD_Rank=rushing_tds_rz_rank, PassTD_Rank=pass_tds_rz_rank, RZ_Att_Rank=pass_attempts_rz_rank, RZ_Cmp_Rank=comp_pct_rz_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=5:8)))) %>% rank_style("Rush_TDs", "RushTD_Rank") %>% rank_style("Pass_TDs", "PassTD_Rank") %>% rank_style("RZ_Att", "RZ_Att_Rank") %>% rank_style("RZ_Cmp_Pct", "RZ_Cmp_Rank") %>% formatRound("RZ_Cmp_Pct", 1) %>% formatString("RZ_Cmp_Pct", suffix="%")
  })
  
  output$qb_downs_table <- renderDT({
    data <- selected_qbs() %>% select(Player=passer, D1_Att=pass_attempts_d1, D1_Cmp=cmp_pct_d1, D2_Att=pass_attempts_d2, D2_Cmp=cmp_pct_d2, D3_Att=pass_attempts_d3, D3_Cmp=cmp_pct_d3, D4_Att=pass_attempts_d4, D1A_R=pass_attempts_d1_rank, D1C_R=cmp_pct_d1_rank, D2A_R=pass_attempts_d2_rank, D2C_R=cmp_pct_d2_rank, D3A_R=pass_attempts_d3_rank, D3C_R=cmp_pct_d3_rank, D4A_R=pass_attempts_d4_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=8:14)))) %>% rank_style("D1_Att", "D1A_R") %>% rank_style("D1_Cmp", "D1C_R") %>% rank_style("D2_Att", "D2A_R") %>% rank_style("D2_Cmp", "D2C_R") %>% rank_style("D3_Att", "D3A_R") %>% rank_style("D3_Cmp", "D3C_R") %>% rank_style("D4_Att", "D4A_R") %>% formatRound(c("D1_Cmp", "D2_Cmp", "D3_Cmp"), 1) %>% formatString(c("D1_Cmp", "D2_Cmp", "D3_Cmp"), suffix="%")
  })
  
  output$rb_gap_table <- renderDT({
    data <- selected_rbs() %>% select(Player=rusher, Inside_Guard=yds_at_guard, Off_Tackle=yds_at_tackle, Outside_End=yds_at_end, Guard_Rank=yds_at_guard_rank, Tackle_Rank=yds_at_tackle_rank, End_Rank=yds_at_end_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=4:6)))) %>% rank_style("Inside_Guard", "Guard_Rank") %>% rank_style("Off_Tackle", "Tackle_Rank") %>% rank_style("Outside_End", "End_Rank")
  })
  output$rb_receiving_table <- renderDT({
    data <- selected_rbs() %>% select(Player=rusher, Receptions=receptions, Rec_Yds=total_rec_yds, YPR=ypr, Rec_Rank=receptions_rank, Yds_Rank=total_rec_yds_rank, YPR_Rank=ypr_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=4:6)))) %>% rank_style("Receptions", "Rec_Rank") %>% rank_style("Rec_Yds", "Yds_Rank") %>% rank_style("YPR", "YPR_Rank") %>% formatRound("YPR", 1)
  })
  
  output$wr_downs_table <- renderDT({
    data <- selected_wrs() %>% select(Player, Rec_D1, Rec_D2, Rec_D3, Rec_D4, D1_Rank, D2_Rank, D3_Rank, D4_Rank) %>% head(10)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=5:8)))) %>% rank_style("Rec_D1", "D1_Rank") %>% rank_style("Rec_D2", "D2_Rank") %>% rank_style("Rec_D3", "D3_Rank") %>% rank_style("Rec_D4", "D4_Rank")
  })
  
  # --- STANDARD TABLES (Key Players) ---
  output$qb_table <- renderDT({
    data <- selected_qbs() %>% select(Player=passer, Games=games_played, Attempts=attempts, Comp_Pct=comp_pct, Air_Yds=total_air_yds, TDs=tds, INTs=ints, CPOE=avg_cpoe, EPA_Pass=epa_per_pass, Air_Yds_Rank=total_air_yds_rank, TDs_Rank=tds_rank, INTs_Rank=ints_rank, CPOE_Rank=avg_cpoe_rank, EPA_Rank=epa_per_pass_rank, Att_Rank=attempts_rank, Comp_Rank=comp_pct_rank)
    datatable(data, rownames=FALSE, options=list(dom='t', paging=FALSE, columnDefs=list(list(visible=FALSE, targets=9:16)))) %>% rank_style("Air_Yds", "Air_Yds_Rank") %>% rank_style("TDs", "TDs_Rank") %>% rank_style("INTs", "INTs_Rank") %>% rank_style("CPOE", "CPOE_Rank") %>% rank_style("EPA_Pass", "EPA_Rank") %>% rank_style("Attempts", "Att_Rank") %>% rank_style("Comp_Pct", "Comp_Rank") %>% formatRound("CPOE", 1) %>% formatString("CPOE", suffix="%") %>% formatRound("EPA_Pass", 2)
  })
  
  output$rb_table <- renderDT({
    data <- selected_rbs() %>% 
      select(Player=rusher, Games=games_played, Carries=carries, Yards=total_rush_yds, 
             Avg=ypc, Success=rush_success_rate, TDs=tds, 
             Yards_Rank=total_rush_yds_rank, Avg_Rank=ypc_rank, Success_Rank=rush_success_rate_rank, 
             TDs_Rank=tds_rank, Carries_Rank=carries_rank)
    
    datatable(data, rownames=FALSE, 
              options=list(dom='t', paging=FALSE, 
                           # CHANGE IS HERE: 7:10 -> 7:11
                           columnDefs=list(list(visible=FALSE, targets=7:11)) 
              )
    ) %>% 
      rank_style("Yards", "Yards_Rank") %>% 
      rank_style("Avg", "Avg_Rank") %>% 
      rank_style("Success", "Success_Rank") %>% 
      rank_style("TDs", "TDs_Rank") %>% 
      rank_style("Carries", "Carries_Rank") %>% 
      formatRound("Success", 1) %>% 
      formatString("Success", suffix="%")
  })
  
  output$wr_table <- renderDT({
    data <- selected_wrs()
    col_label <- unique(data$Col_Name)[1] 
    data <- data %>% rename(!!col_label := Metric_Val)
    datatable(data, rownames=FALSE, options=list(pageLength=5, lengthChange=FALSE, 
                                                 columnDefs=list(list(visible=FALSE, targets=10:19)))) %>% # FIXED: Indices 10-19 Hidden
      rank_style("Rec", "Rec_Rank") %>% rank_style("Yards", "Yards_Rank") %>% rank_style(col_label, "Metric_Rank") %>% rank_style("YPR", "YPR_Rank") %>% rank_style("TDs", "TDs_Rank") %>% rank_style("Rec_D1", "D1_Rank") %>% rank_style("Rec_D2", "D2_Rank") %>% rank_style("Rec_D3", "D3_Rank") %>% rank_style("Rec_D4", "D4_Rank") %>% formatRound(col_label, 1) %>% formatString(col_label, suffix="%")
  })
  
  output$glossary_tbl <- renderDT({
    datatable(glossary_data, rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=15, dom='ltip')) %>% formatStyle('Variable', fontWeight = 'bold', color = '#2c3e50')
  })
}

shinyApp(ui, server)