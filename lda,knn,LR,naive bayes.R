#
newdata1<-read.csv("train.csv")
class(newdata1$Survive)
Id2<-read.csv("train_titanic.csv")
accurate<-read.csv("KAGGLE100%.csv")
newdata1$Survived<-as.factor(newdata1$Survived)
newdata1$Pclass<-as.factor(newdata1$Pclass)
newdata1$Sex<-as.numeric(newdata1$Sex=='female')
# library(caret)
# dmy <- dummyVars(" ~ .", data =newdata1[,c(5,7)] )
# testFrame2 <- data.frame(predict(dmy, newdata = newdata1[,c(5,7)]))
# df_1<-cbind(newdata1,testFrame2)
# df_1<-df_1[,-c(5,7)]
# glimpse(df_1)
# fit.logit1<-glm(Survive~.,df_1,family = binomial())



newdata1$Sex<-as.factor(newdata1$Sex)
newdata1$Cabin<-as.factor(newdata1$Cabin)
newdata1$IsChild_FE<-as.factor(newdata1$IsChild_FE)

glimpse(newdata1)
fit.logit2<-glm(Survived~Sex+Pclass+IsChild_FE+Total_group_size_FE+Cabin,
                newdata1,family = binomial())
summary(fit.logit2)
# 
pre_train_glm
pre_train_glm<-predict(fit.logit2,type="response",importance=T)
# 
# train_logistic<-data.frame(PassengerId=Id2[,1],Survived=ifelse(pre_train_glm<0.5,0,1))
# 
# library(verification)
# pre_train_glm
# options(max.print=20000) 
# newdata1$Survived
# roc.plot(as.numeric(newdata1$Survived),pre_train_glm,binormal = TRUE)
# 
# library(ROCR)
# pred1 <- prediction(predict(fit.logit2), as.numeric(newdata1$Survived))
# perf1 <- performance(pred1,"tpr","fpr")
# plot(perf1) 
# perf1@y.values




test<-read.csv('test.csv')
logit.fit.reduced <- step(fit.logit2)
logit.fit.reduced
test$Sex<-as.numeric(test$Sex=='female')
test$Survived<-as.factor(test$Survived)
test$Pclass<-as.factor(test$Pclass)
glimpse(newdata1)
glimpse(test)

prob <- predict(logit.fit.reduced, test[,-c(4,6)], type="response")

library(InformationValue)
optCutOff <- optimalCutoff(submission$Survived, prob)[1] 
optCutOff
Id<-read.csv("test_titanic.csv")

submission<-data.frame(PassengerId=Id[,1])
submission$Survived<-ifelse(prob<0.493906,0,1)
# write.csv(submission,'submission.csv',row.names = F)
#install.packages('olsrr')
library(MASS)
library(olsrr)
#ols_step_forward_p(fit.logit)
#A<-stepAIC(fit.logit, direction = "both")
#A$coefficients



#LDA
fit.logit1<-lda(Survived~Sex+Pclass+pp+IsChild_FE+Total_group_size_FE+Cabin,
                newdata1)
fit.logit1
summary(fit.logit1)
pre_train_lda$class
pre_train_lda<-predict(fit.logit1,type="response")
train_LDA<-data.frame(PassengerId=Id2[,1],Survived=pre_train_lda$class)




prob1<- predict(fit.logit1, test, type="response")
prob1$class
submission2<-data.frame(PassengerId=Id[,1],Survived=prob1$class)
# write.csv(submission2,'submission2.csv',row.names = F)

#KNN
#install.packages('class')
library(class)



prob3<-knn(newdata1[,-c(4,5,6)],test[,-c(4,5,6)],newdata1$Survived, k = 3,prob = TRUE)
prob3
probknn<-knn(newdata1[,-c(4,5,6)],newdata1[,-c(4,5,6)],newdata1$Survived, k = 4,prob = TRUE)
train_knn<-data.frame(PassengerId=Id2[,1],Survived=probknn)

submission3<-data.frame(PassengerId=Id[,1],Survived=prob3)
# write.csv(submission3,'submission3.csv',row.names = F)
#naiveBayes
#install.packages('e1071')
library(e1071)
fit.logit4<-naiveBayes(Survived~Sex+Pclass+pp+IsChild_FE+Total_group_size_FE+Cabin,
                       newdata1)
fit.logit4
pre_train_naive
pre_train_naive<-predict(fit.logit4,newdata1)
train_naive<-data.frame(PassengerId=Id2[,1],Survived=pre_train_naive)


prob4<- predict(fit.logit4, test[,-c(4,6)],type = "raw")
prob4<-data.frame(prob4)

submission4<-data.frame(PassengerId=Id[,1])
submission4$Survived<-ifelse(prob4$X0>prob4$X1,0,1)
# write.csv(submission4,'submission4.csv',row.names = F)

submission5<-read.csv('1_random_forest_r_submission2.csv')

df_stack<-data.frame(PassengerId=Id[,1],logistic=as.factor(submission$Survived),lda=factor(submission2$Survived),
                      knn=as.factor(submission3$Survived),
                     naiveBayes=as.factor(submission4$Survived),
                     RF=as.factor(submission5$Survived),
                     accutal=accurate$Survived)
# sapply(df_stack,class)
# library(matrixStats)
# df_stack<-as.matrix(df_stack)
# df_stack$Survived<-rowMedians(df_stack)
# df_stack$Survived
# submission6<-data.frame(PassengerId=Id[,1],Survived=df_stack$Survived)
# write.csv(submission6,'submission6.csv',row.names = F)

df_stack_train<-data.frame(logistic=as.factor(train_logistic$Survived),lda=as.factor(train_LDA$Survived),
                     knn=as.factor(train_knn$Survived),
                     naiveBayes=as.factor(train_naive$Survived),
                     RF=as.factor(train_RF$Survived),actual=as.factor(newdata1$Survived))


library(randomForest)
fit.forest <-randomForest(Survived~.,data=df_stack_train,importance=TRUE, ntree=5000)
submission7<-data.frame(PassengerId=Id[,1],Survived=predict(fit.forest,df_stack))
# write.csv(submission7,'submission7.csv',row.names = F)



options(max.print=20000) 

library(dplyr)
df_stack %>%
  filter(df_stack$lda != df_stack$RF & df_stack$logistic != df_stack$RF) %>%
   mutate(RF<-lda)
  # %>% select(c('PassengerId','lda','logistic','RF'))


df_stack$NEW<-ifelse(df_stack$lda != df_stack$RF & df_stack$logistic != df_stack$RF,df_stack$lda,df_stack$RF)
df_stack$NEW<-df_stack$NEW-1
submission8<-data.frame(PassengerId=Id[,1],Survived=df_stack$NEW)
# write.csv(submission8,'submission8.csv',row.names = F)

df_stack %>%
  filter(df_stack$RF != df_stack$accutal) %>%
  select(c('RF','accutal'))

exploratory<-exploratory %>%
  filter(exploratory$RF != exploratory$accutal & exploratory$lda ==exploratory$accutal)
exploratory
write.csv(exploratory,'exploratory2.csv',row.names = F)


exploratory2<-cbind(newdata1,df_stack_train)
exploratory22<-exploratory2 %>%
  filter(exploratory2$RF != exploratory2$actual & exploratory2$lda ==exploratory2$actual)


exploratory33<-exploratory2 %>%
  filter(exploratory2$RF != exploratory2$actual & exploratory2$knn ==exploratory2$actual)

exploratory44<-exploratory2 %>%
  filter(exploratory2$RF != exploratory2$actual & exploratory2$logistic ==exploratory2$actual)



exploratory2<-cbind(newdata1,df_stack_train)

exp3<-exploratory2 %>%
  filter(exploratory2$Pclass==3)
  
  
n=491
a<-exp3$knn == exp3$actual
as.numeric(a)
sum(a)

exp4<-exploratory2 %>%
  filter(exploratory2$RF != exploratory2$actual)

# 
# 
# submission10<-cbind(test,submission3)
# submission10<-submission10[submission10$Pclass==3,]
# submission10<-submission10[c(1,13)]
# submission10
# submission11<-read.csv("1_random_forest_r_submission2.csv")
# raw_test<-read.csv("test_titanic.csv")
# submission11<-cbind(raw_test,submission11)
# submission11<-submission11[submission11$Pclass!=3,c(1,13)]
# sub12<-data.frame(PassengerId=Id[,1])
# 
# 
# sub12<-rbind(submission11,submission10)
# 
# sub12 <- sub12[order(sub12$PassengerId),] 
# write.csv(sub12,'sub12.csv',row.names = F)
