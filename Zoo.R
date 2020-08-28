zoo<-read.csv("F:/Excelr/Assignments/dataset/KNN/Zoo.csv")
head(zoo)
str(zoo)
zoo1<-zoo[-1]
str(zoo1)
sum(is.na(zoo1$type))

zoo1$type=factor(zoo1$type)
str(zoo1)
View(zoo1)




prop.table(table(zoo1$type))

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

zoo1_n<-as.data.frame(lapply(zoo1[1:16],norm))

str(zoo1_n)
head(zoo1_n)
View(zoo1_n)

?sample
sample(x, size, replace = FALSE, prob = NULL)



set.seed(123)
data<-sample(1:nrow(zoo1_n),size = nrow(zoo1_n)*0.7,replace = FALSE)


zoo1_train<-zoo1_n[data,]
  zoo1_test<-zoo1_n[-data,]





zoo1_train_labels<-zoo1[data,17]
zoo1_test_labels<-zoo1[-data,17]



NROW(zoo1_train_labels)


library(class)
library(caret)

?knn
zoo1_test_pred1<-knn(train=zoo1_train ,test=zoo1_test ,cl=zoo1_train_labels ,k=3)
zoo1_test_pred2<-knn(train=zoo1_train ,test=zoo1_test ,cl=zoo1_train_labels ,k=6)
zoo1_test_pred3<-knn(train=zoo1_train ,test=zoo1_test ,cl=zoo1_train_labels ,k=100)
table(zoo1_test_pred ,zoo1_test_labels)


tabel1<-table(zoo1_test_pred1 ,zoo1_test_labels)
tabel2<-table(zoo1_test_pred2,zoo1_test_labels)
tabel3<-table(zoo1_test_pred3 ,zoo1_test_labels)
View(tabel1)
install.packages("gmodels")
library(gmodels)

#crosstable on predicted and actual
CrossTable(x=zoo1_test_labels ,y=zoo1_test_pred1)
CrossTable(x=zoo1_test_labels ,y=zoo1_test_pred2)
CrossTable(x=zoo1_test_labels ,y=zoo1_test_pred3)



accuracy1<-(sum(diag(tabel1))/sum(tabel1))
accuracy2<-(sum(diag(tabel2))/sum(tabel2))
accuracy3<-(sum(diag(tabel3))/sum(tabel3))
