library(readr) # import
library(dplyr) # data manipulation
library(ggplot2) # visualizations
library(gridExtra) # visualizations
library(tictoc) # timing models
library(caret) # tuning & cross-validation

options(max.print=20) 
# extractFeatures <- function(data) {
# features <- c("Pclass",
#                 "Age",
#                 "Sex",
#                 "Parch",
#                 "SibSp",
#                 "Fare",
#                 "Embarked")
#   fea <- data[,features]
#   fea$Age[is.na(fea$Age)] <- -1
#   fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
#   fea$Embarked[fea$Embarked==""] = "S"
#   fea$Sex      <- as.factor(fea$Sex)
#   fea$Embarked <- as.factor(fea$Embarked)
#   return(fea)
# }

missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i]))
    missing_prop[i] <- missing[i] / nrow(x)
  }
  (missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>% 
      arrange(desc(missing_prop)))
}

#warning!!!!!
train<-read.csv('train_titanic.csv')
y<-train$Survived
#train.1<-train[,-2]
test<-read.csv('test_titanic.csv')
all_data<-merge(train,test,all = T)
all_data2<-merge(train,test,all = T)
missing_vars(all_data)
summary(all_data$Age)
head(all_data)
#install.packages('rpart')



#################################Title################################



all_data$Title <- gsub('(.*, )|(\\..*)', '', all_data$Name)
all_data %>%
  group_by(Title) %>%
  count() %>%
  arrange(desc(n))
all_data$Title <- ifelse(all_data$Title %in% c("Mr", "Miss", "Mrs", "Master"), 
                                all_data$Title, 
                                "Other")
all_data$Title 
all_data
##############################cabin#################################
all_data$Cabin <- ifelse(all_data$Cabin=="", 0, 1)
all_data$Cabin<-as.factor(all_data$Cabin)
typeof(all_data$Cabin)
all_data$Cabin


#############################Fare#########################################
fare_pp
fare_pp<-all_data %>%
  group_by(Ticket, Fare) %>%
  count()
fare_pp$pp<-fare_pp$Fare/fare_pp$n
all_data<- left_join(all_data, fare_pp, by = c("Ticket", "Fare"))

A<-all_data %>%
  group_by(Ticket) %>%
  summarize(Group_size_FE = n()) %>%
  arrange(desc(Group_size_FE))

A<-data.frame(A)
############################group size##################################

  all_data$total_group_size<-all_data$SibSp+all_data$Parch+1
  all_data %>% 
  select(c(total_group_size,n,Ticket))
  all_data$judge<-ifelse(all_data$total_group_size<all_data$n,'strange','1')
  all_data
  

  ggplot(all_data, aes(x=Embarked, y=pp)) + geom_point()
  
  family<-all_data%>%
    select(Survived)%>%
    filter(all_data$judge=='1'& !is.na(all_data$Survived))
  #& (all_data$n-all_data$total_group_size)>4 
  # family<-all_data%>%
  #  select(c(Pclass,Ticket,n,total_group_size,pp,Embarked,Survived,PassengerId,Sex))%>%
  #  filter(all_data$judge=='1' & !is.na(all_data$Survived)) %>%
  #  arrange(Ticket)
  # family_survived_percentile<-family%>%
  #  count(Survived)
  # family_survived_percentile
  #& (all_data$n-all_data$total_group_size)>4 
   friend<-all_data%>%
    select(Survived)%>%
    filter(all_data$judge=='strange'& !is.na(all_data$Survived))
    count(friend)  
   
    plot_1<-data.frame(type=rep('friend',count(friend)),survived=friend)
    plot_2<-data.frame(type=rep('family',count(family)),survived=family)
    plot_3<-merge(plot_1,plot_2,all=TRUE)
    sapply(plot_3,class)
    plot_3$Survived<-as.factor(plot_3$Survived)
    plot_3
    table(plot_3)
    ggplot(plot_3, aes(x=type, fill=Survived)) +
    geom_bar(position="stack") + labs(title='position="stack"')

    all_data$Total_group_size_FE<-pmax(all_data$total_group_size, all_data$n)
#####################################missing###########################################
     missing_vars(all_data)
 ################################Fare and Embarked ####################################### 
    all_data$Embarked<-as.factor(all_data$Embarked)
    levels(all_data$Embarked)
    all_data$Fare[is.na(all_data$Fare)]<-median(all_data$Fare,na.rm = TRUE)
    all_data$pp[is.na(all_data$pp)]<-median(all_data$pp,na.rm = TRUE)
    all_data[all_data$Embarked=="",]
    all_data$Embarked[all_data$Embarked==""]="C"
    all_data$Embarked<-as.factor(as.character(all_data$Embarked))
    levels(all_data$Embarked)
    missing_vars(all_data)
   
    
    all_data$Embarked

    typeof(all_data$Embarked)
    all_data[all_data$Embarked=="",]
    
    
    
  all_data$Survived<-as.factor(all_data$Survived)
  all_data$Pclass<-as.factor(all_data$Pclass)
  all_data$Title<-as.factor(all_data$Title)
  glimpse(all_data)
  sapply(all_data, class)
 ##################################AGE########################################
    library(rpart)
    all_data2<-all_data %>%
    select(c(Pclass,Sex,Age,Title,Cabin,pp,Total_group_size_FE,Embarked))
    sapply(all_data2,class)
    missing_vars(all_data2)
    Agefit <- rpart(Age ~ Pclass + Sex + pp + Total_group_size_FE +Embarked+Title+Cabin,
                    data=all_data2[!is.na(all_data2$Age),], 
                    method="anova")

    all_data2$Age[is.na(all_data2$Age)] <- predict(Agefit, all_data2[is.na(all_data2$Age),])
    all_data$Age<-as.integer(all_data2$Age)
  
#ID<-all_data[all_data$Age<18 & all_data$Parch==0 & all_data$SibSp==0,]$PassengerId
#typeof(ID)
#all_data<-all_data[-ID,]
#train<-all_data[0:868,]
#as.data.frame(table(ID))
as.factor(all_data$Title)
###########################################child##################################
all_data$IsChild_FE <- factor(ifelse(all_data$Age< 15, 1, 0))    
    
########################################prediction#####################################
glimpse(all_data)    
glimpse(all_data)
    all_data$Have_relatives<-ifelse(all_data$SibSp=='0' & all_data$Parch=='0','0','1')
    all_data$Have_relatives<-as.factor(all_data$Have_relatives)
    #missing_vars(all_data)
  all_data<- all_data %>%
  select(-c(Age,Cabin,PassengerId,Name,SibSp,Parch,Ticket,Fare,n,total_group_size,judge))
  glimpse(all_data)
  
  levels(all_data$Embarked)
train1<-all_data[0:891,]
#missing_vars(train1)
#train1<-cbind(train1[,1],y,train1[,2:ncol(train1)])
#names(train1)[1]<-'PassengerId'
#names(train1)[2]<-'Survive'
train1
test1<-all_data[892:1309,]
# all_data2<-all_data2[,c(1,6,7,8)]
# all_data3<-left_join(all_data,all_data2,by=('PassengerId'))
# all_data3<-left_join(all_data3,A,by=('Ticket'))
# write.csv(all_data,'all_data.csv',row.names = F)
# write.csv(all_data3,'all_data4.csv',row.names = F)
#
repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(train1) - 1, by = 1))

glimpse(train1)
model<-train(x = train1[,-4],
                    y = train1$Survived,
                    method = "rf",
                    trControl = repeatedCV,
                    importance = TRUE,
                    tuneGrid = rf_grid)
model
varImp(model)

# 
# 
# 
# 

library(randomForest)
options(max.print=2000) 

missing_vars(test1)
glimpse(train1)
glimpse(test1)

write.csv(train1,'train.csv',row.names = F)
write.csv(test1,'test.csv',row.names = F)

fit.forest <-randomForest(Survived~.,data=train1,importance=TRUE, ntree=1894)
# error_oob<-fit.forest$err.rate[,1]
# error_oob<-data.frame(ntrees=1:5000,error_oob=error_oob)
# 
# error_oob[,error_oob$error_oob==0.1683502]
par(mfrow = c(2,1))
plot(fit.forest$err.rate[,1], type = "l")
plot(fit.forest)
varImp(fit.forest)
#####################################stack###############
# pre_train_RF<-predict(fit.forest,type="response")
# pre_train_RF
# train_RF<-data.frame(PassengerId=train[,1],Survived=pre_train_RF)
# write.csv(train_RF,'train_rf.csv',row.names = F)


submission<-data.frame(PassengerId=test$PassengerId)
submission$Survived<-predict(fit.forest,test1)
write.csv(submission, file = "1_random_forest_r_submission2.csv",row.names = FALSE)

levels(all_data$Title)

#fit.forest <-randomForest(train1,train1$Survive,importance=TRUE, ntree=2000)
#submission<-data.frame(PassengerId=test$PassengerId)
#submission$Survived<-predict(fit.forest,extractFeatures(test1))

