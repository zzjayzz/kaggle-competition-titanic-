rf_1<-read.csv("submission7.csv")
knn_1<-read.csv('submission3.csv')
kaggle<-read.csv('train.csv')
all_1<-data.frame(rf=rf_1$Survived,knn=knn_1$Survived)
all_2<-cbind(kaggle,all_1)