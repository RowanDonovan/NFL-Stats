#Regular Season Data

# Important Packages to Load----
library(tidyr)
library(dplyr)

# Read in Data----
data <- read.csv('play_by_play_2023.csv')

##Winning Percentage
homeWinPercentage <- data |> filter(season_type == 'REG',
                                desc == 'END GAME') |>
    mutate(hWin = case_when(home_score > away_score ~ 1,
                            home_score <= away_score ~ 0),
           aWin = case_when(home_score < away_score ~ 1,
                            home_score >= away_score ~ 0),
           tie = case_when(home_score == away_score ~ 1,
                           home_score != away_score ~0)) |>
    select(game_id, home_team, away_team, home_score, away_score, hWin, aWin, tie) |>
    group_by(home_team) |>
    summarise(nWins = sum(hWin),
              nLoss = sum(aWin),
              ntie = sum(tie),
              n = n()) |>
    rename(team = home_team)

awayWinPercentage <- data |> filter(season_type == 'REG',
                                    desc == 'END GAME') |>
    mutate(hWin = case_when(home_score > away_score ~ 1,
                            home_score <= away_score ~ 0),
           aWin = case_when(home_score < away_score ~ 1,
                            home_score >= away_score ~ 0),
           tie = case_when(home_score == away_score ~ 1,
                           home_score != away_score ~0)) |>
    select(game_id, home_team, away_team, home_score, away_score, hWin, aWin, tie) |>
    group_by(away_team) |>
    summarise(nWins = sum(aWin),
              nLoss = sum(hWin),
              ntie = sum(tie),
              n = n()) |>
    rename(team = away_team)

winPercentage <- rbind(homeWinPercentage, awayWinPercentage)

winPercentage <- winPercentage |> group_by(team) |>
    summarise(wins = sum(nWins),
              losses = sum(nLoss),
              ties = sum(ntie)) |>
    mutate(percentage = round(wins / (wins + losses), digits = 3)) |>
    select(team, percentage)
