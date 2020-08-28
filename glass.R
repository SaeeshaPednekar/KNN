glass<-read.csv("F:/Excelr/Assignments/dataset/KNN/glass.csv")

View(glass)
str(glass)
summary(glass)
glass$Type

glass$Type<-as.factor(glass$Type)
str(glass)


table(glass$Type)
prop.table(table(glass$Type))
round(prop.table(table(glass$Type))*100,2)

str(glass)



norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)


set.seed(123)
data<-sample(1:nrow(glass_n),size=nrow(glass_n)*0.7,replace=FALSE)


glass_train<-glass_n[data,]
glass_test<-glass_n[-data,]



glass_train_labels<- glass[data,10]
glass_test_labels<- glass[-data,10]



NROW(glass_train_labels)

round(prop.table(table(glass_train_labels))*100,2)
round(prop.table(table(glass_test_labels))*100,2)


library(class)
library(caret)


glass_test_pred1 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=1)
glass_test_pred2 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
glass_test_pred3 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=12)




table(glass_test_pred1,glass_test_labels)
table(glass_test_pred2,glass_test_labels)
table(glass_test_pred3,glass_test_labels)


tabel1<-table(glass_test_pred1,glass_test_labels)
tabel2<-table(glass_test_pred2,glass_test_labels)
tabel3<-table(glass_test_pred3,glass_test_labels)

install.packages("gmodels")
library(gmodels)

CrossTable(x=glass_test_labels ,y=glass_test_pred1)
CrossTable(x=glass_test_labels ,y=glass_test_pred2)
CrossTable(x=glass_test_labels ,y=glass_test_pred3)






accuracy1<-(sum(diag(tabel1))/sum(tabel1))
accuracy2<-(sum(diag(tabel2))/sum(tabel2))
accuracy3<-(sum(diag(tabel2))/sum(tabel3))










