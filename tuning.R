library(caret)
glimpse(train1)

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3)

rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(train1) - 1, by = 1))

glimpse(train1)
model<-train(x = train1[,c(-1,-6,-5,-4)],
             y = train1$Survived,
             method = "rf",
             trControl = repeatedCV,
             importance = TRUE,
             tuneGrid = rf_grid)
model
varImp(model)
glimpse(train1)

train2<-train1[,c(-1,-5,-4)]
library(randomForest)
fit.forest <-randomForest(Survived~.,data=train2,importance=TRUE, ntree=5000)
submission<-data.frame(PassengerId=test$PassengerId)
submission$Survived<-predict(fit.forest,test1[,c(-1,-4,-5,-6)])
write.csv(submission, file = "1_random_forest_r_submission2.csv",row.names = FALSE)

glimpse(test1)
fit<-lm(Survived~Sex+Pclass+pp+IsChild_FE+Total_group_size_FE+Cabin,
        train1)


