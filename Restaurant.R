setwd("~/Desktop/Kaggle/restaurant-revenue-prediction")
testfood <- read.csv("test.csv")
testtrain <- read.csv("train.csv")



regression <- lm(revenue ~ City, data = testtrain)
summary(regression)

x <- testtrain[, c("City", "revenue")]



library(tidyverse)
library(caret)
mygrid <- expand.grid(mtry = 3, splitrule = 'maxstat', min.node.size = 10)
rf <- train(revenue~P28 + P20 + P6,
            data=(testtrain[,c("P28", "P20", "P6", "revenue")]),
            method="ranger",
            metric = 'RMSE',
            trControl=trainControl(method="repeatedcv",
                                   number=10, 
                                   repeats=3),
            tuneGrid = mygrid
)



rf.preds <- data.frame(Id=testfood$Id, Prediction=predict(rf, newdata=testfood))
write_csv(x=rf.preds, file = ("~/restuarantprediction2.csv"))

