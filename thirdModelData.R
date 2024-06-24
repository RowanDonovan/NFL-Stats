#Data for Third Down Prediction Model

# Read in Data----
data <- read.csv('play_by_play_2023.csv')

# Manipulations----

modelData <- data |> mutate_at(vars(rushing_yards), ~replace(., is.na(.), 0)) |>
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
    filter(down == 3,
           play_type != 'punt',
           play_type != 'field_goal',
           play_type != 'no_play') |>
    select(game_seconds_remaining,
           ydstogo,
           posteam_score,
           first_down,
           play_type,
           runningRush,
           yardline_100)

#Create .csv File----
write.csv(modelData, 'thirdModelData.csv')
