newdata<-read.csv('train.csv')
glimpse(newdata)
library(class)
newdata$Sex<-as.numeric(newdata$Sex=='female')

probknn<-knn(newdata1[,-c(5,6,7)],
             newdata1[,-c(5,6,7)],newdata1$Survived, k = 3,prob = TRUE)
probknn



fit.logit2<-glm(Survived~.,
                train1,family = binomial())
logit.fit.reduced <- step(fit.logit2)
pre_train_glm<-predict(logit.fit.reduced,type="response",importance=T)
library(InformationValue)
optCutOff <- optimalCutoff(train1$Survived, pre_train_glm)[1] 
train_logistic<-data.frame(PassengerId=Id2[,1],Survived=ifelse(pre_train_glm<optCutOff,0,1))
train_logistic

library(randomForest)
newdata2<-train1
glimpse(newdata2)
fit.forest <-randomForest(Survived~.,data=newdata2,importance=TRUE, ntree=5000)
probRF<-predict(fit.forest)

A<-read.csv("train.csv")
A_1<-data.frame(knn=train_logistic$Survived,RF=probRF)
A_2<-cbind(A,A_1)

A_2 %>%
  filter(knn==Survived & Sex=='male') %>%
  count()


A_2 %>%
  filter(RF==Survived &  Sex=='male') %>%
  count()


