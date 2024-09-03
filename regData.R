#Regular Season Data

# Important Packages to Load----
library(tidyr)
library(dplyr)

# Read in Data----
data <- read.csv('play_by_play_2023.csv')

#Statistics of interest----

##Winning Percentage----
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
    mutate(percentage = round(wins / (wins + losses), digits = 3),
           aboveAvgPerc = case_when(percentage >= median(percentage) ~ 1,
                                    percentage < median(percentage) ~ 0)) |>
    select(team, percentage, aboveAvgPerc)

##Division Winning Percentage----

div <- data |> filter(season_type == 'REG',
                      desc == 'END GAME',
                      div_game == 1)

homeDivWin <- div |> mutate(hWin = case_when(home_score > away_score ~ 1,
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

awayDivWin<- div |> mutate(hWin = case_when(home_score > away_score ~ 1,
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

divWinPercentage <- rbind(homeDivWin, awayDivWin)

divWinPercentage <- divWinPercentage |> group_by(team) |>
    summarise(wins = sum(nWins),
              losses = sum(nLoss),
              ties = sum(ntie)) |>
    mutate(percentageDiv = round(wins / (wins + losses), digits = 3),
           aboveAvgDiv = case_when(percentageDiv >= median(percentageDiv) ~ 1,
                                   percentageDiv < median(percentageDiv) ~ 0)) |>
    select(team, percentageDiv, aboveAvgDiv)
##Points Per Game----

homePpgReg <- data |> filter(season_type == 'REG',
                         desc == 'END GAME') |>
    group_by(home_team) |>
    summarize(score = sum(home_score),
              n = n()) |>
    rename(team = home_team)

awayPpgReg <- data |> filter(season_type == 'REG',
                             desc == 'END GAME') |>
    group_by(away_team) |>
    summarize(score = sum(away_score),
              n = n()) |>
    rename(team = away_team)

ppgReg <- rbind(homePpgReg, awayPpgReg)

ppgReg <- ppgReg |> group_by(team) |>
    summarise(points = sum(score),
              nGames = sum(n)) |>
    mutate(perRegGame = round((points / nGames), digits = 3),
           aboveAvgPerReg = case_when(perRegGame >= median(perRegGame) ~ 1,
                                      perRegGame < median(perRegGame) ~ 0)) |>
    select(team, perRegGame, aboveAvgPerReg)

##Points Allowed per Game----

hRegAllowed <- data|> filter(season_type == 'REG',
                          desc == 'END GAME') |>
    select(game_id, home_team, away_team, home_score, away_score) |>
    group_by(home_team) |>
    summarise(allowed = sum(away_score),
              n = n()) |>
    rename(team = home_team)

aRegAllowed <- data|> filter(season_type == 'REG',
                          desc == 'END GAME') |>
    select(game_id, home_team, away_team, home_score, away_score) |>
    group_by(away_team) |>
    summarise(allowed = sum(home_score),
              n = n()) |>
    rename(team = away_team)

allowedReg <- rbind(hRegAllowed, aRegAllowed)

allowedReg <- allowedReg |> group_by(team) |>
    summarise(pointsAllowed = sum(allowed),
              nGames = sum(n)) |>
    mutate(avgAllowed = round((pointsAllowed / nGames), digits = 3),
           aboveAvgAllowed = case_when(avgAllowed >= median(avgAllowed) ~ 1,
                                       avgAllowed < median(avgAllowed) ~ 0)) |>
    select(team, avgAllowed, aboveAvgAllowed)

##Timeouts Left at Two minute Warning----

toi <- data |> filter(season_type == 'REG',
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

firstHome <- toi |> filter(game_half == 'Half1') |>
    distinct(game_id,
             hto,
             ato,
             home_team) |>
    group_by(home_team) |>
    summarise(totTimeouts = sum(hto),
              n = n()) |>
    rename(team = home_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

firstAway <- toi |> filter(game_half == 'Half1')|>
    distinct(game_id,
             ato,
             hto,
             away_team) |>
    group_by(away_team) |>
    summarise(totTimeouts = sum(ato),
              n = n()) |>
    rename(team = away_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

secondHome <- toi |> filter(game_half == 'Half2') |>
    distinct(game_id,
             hto,
             ato,
             home_team) |>
    group_by(home_team) |>
    summarise(totTimeouts = sum(hto),
              n = n()) |>
    rename(team = home_team) |>
    mutate(n = ifelse((n %% 2) > 0, n+1, n))

secondAway <- toi |> filter(game_half == 'Half2') |>
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

warningTimeoutsReg <- df |> group_by(team) |>
    summarise(totalTimeouts = sum(totTimeouts),
              nHalves = sum(n)/2) |>
    mutate(avgTimeouts = round((totalTimeouts / nHalves), 2),
           aboveAvgTimeouts = case_when(avgTimeouts >= median(avgTimeouts) ~ 1,
                                        avgTimeouts < median(avgTimeouts) ~ 0)) |>
    select(team, avgTimeouts, aboveAvgTimeouts)

##Third Down Conversion Rate----

h3 <- data |> filter(season_type == 'REG',
                     down == 3,
                     c(play_type == 'pass' | play_type == 'run')) |>
    group_by(home_team) |>
    summarise(n = n(),
              conv = sum(third_down_converted),
              fail = sum(third_down_failed)) |>
    mutate(convRate = conv / n) |>
    rename(team = home_team)

a3 <- data |> filter(season_type == 'REG',
                           down == 3,
                           c(play_type == 'pass' | play_type == 'run')) |>
    group_by(away_team) |>
    summarise(n = n(),
              conv = sum(third_down_converted),
              fail = sum(third_down_failed)) |>
    mutate(convRate = conv / n) |>
    rename(team = away_team)

thirdDownReg <- rbind(h3, a3)

thirdDownReg <- thirdDownReg |> group_by(team) |>
    summarise(conversionRate = round((sum(conv) / sum(n)), digits = 3),
              aboveAvgConv= case_when(conversionRate >= median(conversionRate) ~ 1,
                                       conversionRate < median(conversionRate) ~ 0))

#Create Final Data Set----

regularData <- winPercentage |> right_join(ppgReg, by = 'team') |>
    right_join(allowedReg, by = 'team') |>
    right_join(warningTimeoutsReg, by = 'team') |>
    right_join(thirdDownReg, by = 'team') |>
    right_join(divWinPercentage, by = 'team')

write.csv(regularData, 'regularData.csv')

