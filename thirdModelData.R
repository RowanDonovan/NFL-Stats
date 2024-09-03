#Data for Third Down Prediction Model

# Important Packages to Load----
library(dplyr)
# Read in Data----
data <- read.csv('play_by_play_2023.csv')

# Manipulations----

ryuzaki <- data |> mutate_at(vars(rushing_yards), ~replace(., is.na(.), 0)) |>
    mutate(homePos = case_when(home_team == posteam ~ 1,
                               home_team != posteam ~ 0),
           awayPos = case_when(away_team == posteam ~ 1,
                               away_team != posteam ~0),
           homeRush = rushing_yards * homePos,
           awayRush = rushing_yards * awayPos) |>
    group_by(game_id) |>
    mutate(homeRunningRush = cumsum(homeRush),
           awayRunningRush = cumsum(awayRush)) |>
    ungroup() |>
    mutate(runningRush = case_when(homePos == 1 ~ homeRunningRush,
                                   homePos == 0 ~ awayRunningRush)) |>

    mutate_at(vars(passing_yards), ~replace(., is.na(.), 0)) |>
    mutate(homePass = passing_yards * homePos,
           awayPass = passing_yards * awayPos) |>
    group_by(game_id) |>
    mutate(homeRunningPass = cumsum(homePass),
           awayRunningPass = cumsum(awayPass)) |>
    ungroup() |>
    mutate(runningPass = case_when(homePos == 1 ~ homeRunningPass,
                                   homePos == 0 ~ awayRunningPass)) |>
    filter(down == 3,
           play_type != 'punt',
           play_type != 'field_goal',
           play_type != 'no_play') |>
    select(posteam,
           game_seconds_remaining,
           ydstogo,
           posteam_score,
           first_down,
           play_type,
           runningRush,
           runningPass,
           yardline_100) |>
    relocate(play_type, .after = yardline_100)

apples <- ryuzaki |> arrange(posteam)
apples <- split(apples, f = apples$posteam)

chief <- ryuzaki |> select(-posteam)
#Create .csv File----
write.csv(chief, 'thirdModelData.csv')
saveRDS(apples, 'apples.rds')
