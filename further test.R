knn<-read.csv('submission2.csv')
knn2<-read.csv('submission.csv')
RF<-read.csv('1_random_forest_r_submission2.csv')
KAGGLE<- read_csv("KAGGLE100%.csv")
test <- read_csv("test.csv")
ALL<-data.frame(log=knn$Survived,lda=knn2$Survived,RF=RF$Survived,KAGGLE=KAGGLE$Survived)
ALL_1<-cbind(test,ALL)
# ALL_1<-ALL_1[ALL_1$Pclass=='3',]
ALL_1 %>%
  filter(lda == KAGGLE & Pclass=='3') %>%
  count()


ALL_1 %>%
  filter(RF== KAGGLE &  Pclass=='3' ) %>%
  count()

all_3<-ALL_1 %>%
  filter(IsChild_FE=='1' &lda!=KAGGLE) %>%
  count()
  

all_4<-ALL_1 %>%
  filter(RF!=KAGGLE)




ALL_1$s<-ifelse(ALL_1$Pclass==3,ALL_1$log,ALL_1$RF)
test22<-read.csv('test_titanic.csv')
submission_1<-data.frame(PassengerId=test22$PassengerId,Survived=ALL_1$s)
write.csv(submission_1,'submission_1.csv',row.names = F)