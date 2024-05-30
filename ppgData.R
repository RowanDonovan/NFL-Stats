######################
# Data Manipulations #
######################

# Important Packages to Load
library(tidyr)
library(dplyr)

# Read in Data
data <- read.csv('play_by_play_2023.csv')

# Manipulations to find post season PPG
ppgPlayoffs <- data |> filter(season_type == 'POST') |>
    group_by(game_id) |>
    summarise(home = home_team,
              away = away_team,
              homeScore = max(total_home_score),
              awayScore = max(total_away_score )) |>
    distinct(game_id, home, away, homeScore, awayScore) |>
    pivot_longer(cols = c(home, away),
                 values_to = 'team',
                 names_to = 'names') |>
    mutate(h = case_when(names == 'home' ~ 1,
                         names == 'away' ~ 0),
           a = case_when(names == 'home' ~ 0,
                         names == 'away' ~ 1),
           hScore = homeScore * h,
           aScore = awayScore * a,
           totScore = hScore + aScore) |>

    group_by(team) |>
    summarise(nGames = n(),
              points = sum(totScore)) |>
    mutate(perGame = round((points / nGames), digits = 2))

ski <- data |> filter(season_type == 'POST',
                      half_seconds_remaining <= 120) |>
    filter(half_seconds_remaining >= 118) |>
    pivot_longer(cols = c(home_timeouts_remaining, away_timeouts_remaining),
                 names_to = 'names',
                 values_to = 'values') |>
    mutate(h = case_when(names == 'home_timeouts_remaining' ~ 1,
                     names == 'away_timeouts_remaining' ~ 0),
           a = case_when(names == 'home_timeouts_remaining' ~ 0,
                         names == 'away_timeouts_remaining' ~ 1),
           hto = values * h,
           ato = values * a) |>
    select(game_id, home_team,
           away_team,
           ato,
           hto,
           half_seconds_remaining,
           game_half)

firstHome <- ski |> filter(game_half == 'Half1') |>
    distinct(game_id,
             hto,
             ato,
             home_team) |>
    group_by(home_team) |>
    summarise(totTimeouts = sum(hto),
              n = n()) |>
    rename(team = home_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

firstAway <- ski |> filter(game_half == 'Half1')|>
    distinct(game_id,
             ato,
             hto,
             away_team) |>
    group_by(away_team) |>
    summarise(totTimeouts = sum(ato),
              n = n()) |>
    rename(team = away_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

secondHome <- ski |> filter(game_half == 'Half2') |>
    distinct(game_id,
             hto,
             ato,
             home_team) |>
    group_by(home_team) |>
    summarise(totTimeouts = sum(hto),
              n = n()) |>
    rename(team = home_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

secondAway <- ski |> filter(game_half == 'Half2') |>
    distinct(game_id,
             ato,
             hto,
             away_team) |>
    group_by(away_team) |>
    summarise(totTimeouts = sum(ato),
              n = n()) |>
    rename(team = away_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

df <- rbind(firstAway, firstHome, secondAway, secondHome)

warningTimeoutsPlayoffs <- df |> group_by(team) |>
    summarise(totalTimeouts = sum(totTimeouts),
              nHalves = sum(n)/2) |>
    mutate(avgTimeouts = round((totalTimeouts / nHalves), 2)) |>
    select(team, avgTimeouts)




trdh <- data |> filter(season_type == 'POST',
                            down == 3,
                            c(play_type == 'pass' | play_type == 'run' ))|>
    select(play_type, game_id, away_team, home_team, third_down_converted, third_down_failed) |>
    group_by(home_team) |>
    summarise(n = n(),
              conv = sum(third_down_converted),
              fail = sum(third_down_failed)) |>
    mutate(convRate = conv / n) |>
    rename(team = home_team)

trda <- data |> filter(season_type == 'POST',
                       down == 3,
                       c(play_type == 'pass' | play_type == 'run' ))|>
    select(play_type, game_id, away_team, home_team, third_down_converted, third_down_failed) |>
    group_by(away_team) |>
    summarise(n = n(),
              conv = sum(third_down_converted),
              fail = sum(third_down_failed)) |>
    mutate(convRate = conv / n) |>
    rename(team = away_team)

df3 <- rbind(trdh, trda)

thirdDownConversionRate <- df3 |> group_by(team) |>
    summarize(conversionRate = round((sum(conv) / sum(n)), digits = 2))

write.csv(ppgPlayoffs, 'ppgPlayoffs.csv')
write.csv(warningTimeoutsPlayoffs, 'warningTimeoutsPlayoffs.csv')
write.csv(thirdDownConversionRate, 'thirdDownConversionRate.csv')

#Creating Final Data Set----
playoffData <- ppgPlayoffs |> right_join(warningTimeoutsPlayoffs, by = 'team') |>
    right_join(thirdDownConversionRate, by = 'team')

write.csv(playoffData, 'playoffData.csv')
