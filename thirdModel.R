#  Important Packages to Load
library(RCurl)
library(caret)
library(randomForest)
library(dplyr)

light <- read.csv('thirdModelData.csv')

light <- na.omit(light)
light$play_type <- as.factor(light$play_type)

set.seed(724)
trainingIndex <- createDataPartition(light$first_down, p = .8,
                                     list = FALSE,
                                     times = 1)
trainingData <- light[trainingIndex,]
testingData <- light[-trainingIndex,]

rfModel <- randomForest(play_type ~ game_seconds_remaining + ydstogo + posteam_score + first_down + runningRush + yardline_100,
                        data = trainingData,
                        importance = TRUE,
                        ntree = 500,
                        mtry = 3)

saveRDS(rfModel, 'thirdModel.rds')
