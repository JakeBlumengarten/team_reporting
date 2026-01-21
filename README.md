# NFL Opponent Scouting Dashboard

A comprehensive analytics platform designed for football coaches and decision-makers to gain actionable insights into opponent performance, game preparation, and strategic planning using advanced NFL statistics.

[![R](https://img.shields.io/badge/R-4. 0%2B-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive-brightgreen)](https://shiny.rstudio.com/)
[![nflfastR](https://img.shields.io/badge/nflfastR-latest-orange)](https://www.nflfastr.com/)

## 🎯 Project Overview

This interactive dashboard provides **real-time opponent analysis** for the 2025 NFL season, aggregating play-by-play data into actionable scouting reports.  Designed for coaching staffs and front office personnel, it delivers: 

- **Team Overview**:  Offensive identity, efficiency metrics, and league-wide rankings
- **Deep Dive:  Situational Analysis**:  QB performance under pressure, 4th down tendencies, red zone efficiency
- **Key Players**: Individual player breakdowns including passing splits, receiving targets, and run game schemes
- **Visual Analytics**: Interactive charts, heat-mapped tables, and color-coded performance indicators

### 🏈 Use Cases
- **Game Preparation**: Identify opponent tendencies (pass rate, 4th down aggression, red zone strategy)
- **Personnel Evaluation**: Track key player performance by down, distance, and field position
- **Strategic Planning**: Exploit weaknesses in opponent pass protection, run defense, or situational play-calling
- **Post-Game Analysis**: Compare predicted vs.  actual performance metrics

## 🚀 Key Features

### 📊 Team Overview Tab
- **Offensive Identity**: 
  - Overall pass rate vs. run rate (pie charts)
  - Red zone pass percentage
  - Total 4th down go rate
- **Efficiency Metrics**: 
  - EPA/Play (Expected Points Added per play)
  - Pass yards per game & Rush yards per game
  - League rankings for all metrics
- **Full Team Stat Profile**:
  - Success rate, CPOE (Completion Percentage Over Expected), turnovers
  - Explosive pass rate, third down conversion %
  - Division standings

### 🔍 Deep Dive:  Situational Tab
- **Quarterback Under Pressure**:
  - Completion %, air yards, YPA (Yards Per Attempt)
  - CPOE, success rate, interceptions, touchdowns
- **4th Down Conversions by Distance**:
  - Attempts, conversions, and success percentage for 1-3 yds, 4-6 yds, 7+ yds
- **Red Zone Efficiency**:  Scoring rate inside the 20-yard line
- **Performance by Down**:  Passing splits (attempts & completion %) for 1st, 2nd, 3rd, 4th downs

### 👥 Key Players Tab
- **Passing Splits by Down**:  QB performance breakdown (D1, D2, D3, D4)
- **Receiving Targets**:  Target distribution across downs for WRs/TEs
- **Run Game Breakdown**:
  - **Gap Scheme**: Yards by Inside Guard, Off Tackle, Outside End
  - **RB Receiving**: Receptions, receiving yards, YPR (Yards Per Reception)

### 📚 Glossary Tab
- **Metric Definitions**:  EPA, Dakota score, CPOE, gap concepts, and more
- **Color Coding Legend**:
  - 🟢 **Green**: Top 20% (Elite 1-6)
  - 🟡 **Yellow**: Mid Tier (7-19)
  - 🔴 **Red**: Bottom 40% (Weak 20-32)

## 📁 Repository Structure

```
team_reporting/
├── Data/                              # Processed datasets
│   ├── offense_stats.csv             # Team offensive metrics
│   ├── defense_stats.csv             # Team defensive metrics
│   ├── qb_stats.csv                  # Quarterback statistics
│   ├── rb_stats.csv                  # Running back statistics
│   ├── wr_stats.csv                  # Wide receiver statistics
│   ├── fourth_down_conversions.csv   # 4th down decision data
│   ├── nfl_25_schedule.csv           # 2025 NFL schedule
│   └── cfb_25_schedule.csv           # 2025 CFB schedule (future expansion)
├── Player_tables/                     # Player-specific HTML tables (generated)
├── Team_tables/                       # Team-specific HTML tables (generated)
├── renv/                              # R environment management
├── 01_Data.R                          # Data collection & preprocessing
├── app.R                              # Shiny dashboard application
├── renv.lock                          # Package version lock file
├── team_reporting.Rproj               # RStudio project file
└── README.md                          # Project documentation
```

## 🛠️ Technology Stack

**R (100%)**
- `nflfastR` - NFL play-by-play data API (2025 season)
- `nflplotR` - Team logos and color schemes
- `cfbfastR` - College football data (future expansion)
- `shiny` - Interactive web application framework
- `tidyverse` (`dplyr`, `tidyr`, `readr`) - Data manipulation
- `DT` - Interactive data tables
- `renv` - Reproducible R environment management

**Data Pipeline**
1. **Data Collection**: `nflfastR:: load_pbp()` pulls real-time NFL play-by-play data
2. **Feature Engineering**:  Aggregation of 50+ variables (EPA, CPOE, success rate, etc.)
3. **Team Matching**: Robust team name standardization (LAR→LA, SD→LAC, etc.)
4. **Interactive UI**: Dropdown team selection, tabbed navigation, color-coded rankings

## 🚀 Getting Started

### Prerequisites

**R 4.0+** and **RStudio** (recommended)

```r
# Install required packages
install. packages(c(
  "shiny",
  "nflfastR",
  "nflplotR",
  "cfbfastR",
  "tidyverse",
  "DT",
  "renv"
))
```

### Installation

1. **Clone the repository**: 
   ```bash
   git clone https://github.com/JakeBlumengarten/team_reporting.git
   cd team_reporting
   ```

2. **Restore R environment** (optional but recommended):
   ```r
   renv::restore()
   ```

3. **Run data collection** (pulls latest 2025 NFL data):
   ```r
   source("01_Data.R")
   ```
   ⚠️ **Note**: This script downloads ~500MB of play-by-play data. Runtime:  3-5 minutes.

4. **Launch the Shiny app**:
   ```r
   shiny::runApp("app.R")
   ```
   The dashboard will open in your browser at `http://127.0.0.1:5865/` (or similar port).

### Quick Start (Pre-loaded Data)

If you want to skip data collection and use existing CSV files:

```r
# Just launch the app (uses Data/*. csv files)
shiny::runApp("app.R")
```

## 📈 Methodology

### Data Collection Pipeline

1. **Play-by-Play Scraping**:
   - `nflfastR::load_pbp(2025)` retrieves every play from the 2025 NFL season
   - Data includes: down, distance, field position, play type, yards gained, EPA, CPOE, pressure data, etc.

2. **Team-Level Aggregation**:
   - Group by `posteam` (possession team) and aggregate by game/season
   - Calculate per-game averages, success rates, and efficiency metrics

3. **Player-Level Aggregation**:
   - Extract QB, RB, and WR statistics from play-level data
   - Filter for statistical significance (e.g., QBs with 200+ attempts)

4. **Ranking & Color Coding**:
   - Percentile-based rankings (Top 20% = Green, etc.)
   - Contextual coloring for easy pattern recognition

### Key Metrics Explained

| Metric | Description | Why It Matters |
|--------|-------------|----------------|
| **EPA/Play** | Expected Points Added per play | Best overall efficiency measure (accounts for field position, down, distance) |
| **CPOE** | Completion % Over Expected | Measures QB accuracy beyond difficulty of throws |
| **Dakota** | Composite score (EPA + CPOE) | Holistic QB performance metric |
| **Success Rate** | % of plays gaining 40%+ of yards needed (1st/2nd), 100% (3rd/4th) | Consistency indicator |
| **Explosive Pass Rate** | % of passes gaining 15+ yards | Big-play capability |
| **4th Down Go Rate** | % of 4th downs where team goes for it (not punt/FG) | Aggressiveness indicator |
| **Gap Scheme** | Yards by run direction (Inside/Tackle/Outside) | Run game tendencies |

## 📊 Sample Insights

### Example: Scouting the Arizona Cardinals (ARI)

**Offensive Identity** (from Image 1):
- **Pass Rate**: 52% (balanced offense)
- **Red Zone Pass**: 42% (slightly run-heavy in RZ)
- **4th Down Go Rate**: 15% (conservative)

**Efficiency Metrics**:
- **EPA/Play**:  -0.021 (Rank #25) ⚠️ *Below average*
- **Pass Yds/Gm**: 256.1 (Rank #5) ✅ *Elite passing volume*
- **Rush Yds/Gm**:  93.1 (Rank #31) ❌ *Weak run game*

**Key Takeaway**: Arizona is pass-dependent with a struggling run game.  Expect high pass volume, but inefficient offense overall.

**QB Under Pressure** (from Image 2):
- **C. Williams**: 25. 3% completion, 6.7 YPA, 0.139% CPOE when hit
- **4th Down**:  48% success on 1-3 yds, 43% on 4-6 yds, 100% on 7+ yds (small sample)

**Defensive Strategy**:  Pressure the QB heavily.  Williams struggles under duress. 

## ⚠️ Limitations

- **Data Availability**: Only includes 2025 NFL season data (no historical trends yet)
- **Sample Size**: Early-season data may have small sample sizes for situational stats
- **Team Name Mapping**:  Relocation teams (LAR, LAC, LV) require manual mapping
- **CFB Integration**: College football data included in repo but not yet integrated into dashboard

## 🔮 Future Enhancements

- [ ] **Historical Trends**: Multi-season comparison (2020-2025)
- [ ] **Defensive Dashboard**: Mirror offensive metrics for defensive performance
- [ ] **Predictive Modeling**: Win probability, score prediction based on team stats
- [ ] **Play-Calling Sequencing**: Analyze play-calling patterns (e.g., run after incompletion)
- [ ] **Injury Impact**: Integrate injury data to adjust projections
- [ ] **CFB Integration**: Expand to college football scouting reports
- [ ] **Export Reports**: Generate PDF scouting reports for offline use

## 📚 Data Sources

- **nflfastR**: Official NFL play-by-play data (2025 season)
  - Gilani, S., Easwaran, A., Lee, J., & Hess, E. (2021). *nflfastR: Functions to Efficiently Access NFL Play by Play Data. * https://www.nflfastr.com/
- **nflplotR**: Team logos, color schemes, and wordmarks
- **cfbfastR**: College football data (SportsDataverse)

## 👤 Author

**Jake Blumengarten**  
📧 blumengartenjake@gmail.com  
🔗 [LinkedIn](https://www.linkedin.com/in/jake-blumengarten)  
💻 [GitHub](https://github.com/JakeBlumengarten)

---

*This project showcases data engineering, sports analytics, and interactive dashboard development for football operations roles.  All code and analysis are original work.*

## 📄 License

This project is available for educational and portfolio purposes.  Data sourced from publicly available NFL APIs (nflfastR).

---

⭐ **If you find this dashboard useful for your scouting process, please consider starring the repository! **

## 🤝 Contributing

Contributions are welcome! If you have ideas for new metrics, visualizations, or features:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-metric`)
3. Commit your changes (`git commit -m 'Add new metric: X'`)
4. Push to the branch (`git push origin feature/new-metric`)
5. Open a Pull Request

## 📞 Support

For questions, bug reports, or feature requests: 
- Open an issue on GitHub
- Email:  blumengartenjake@gmail.com

---

**Built for football coaches and analysts**
