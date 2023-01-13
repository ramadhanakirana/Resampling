library("caret")
library("rpart")

#input data
Titanic<-read.csv("D:/Actuarial Simulation/Titanic.csv")
str(Titanic)

#change the data type to factor
##method 1
Titanic$Survived<-as.factor(ifelse(Titanic$Survived=="0","No","Yes"))
Titanic$Sex<-as.factor(Titanic$Sex)
Titanic$PClass<-as.factor(Titanic$PClass)
Titanic<-Titanic[,c("Survived", "PClass", "Sex", "Age")]
str(Titanic)
##method 2
with(Titanic, {
  print(table(Survived))
  print(table(Sex))
  print(table(PClass))
})

#check data partition
##method 1
prop.table(table(Titanic$Survived))
prop.table(table(Titanic$Sex))
prop.table(table(Titanic$PClass))
barplot(prop.table(table(Titanic$Survived)),
        col=rainbow(2), ylim=c(0, 0.7), main="Class Distribution")
##method 2
with(Titanic, {
  print(prop.table(table(Survived)))
  print(prop.table(table(Sex)))
  print(prop.table(table(PClass)))
})

#data partition
##method 1
set.seed(100)
test_idx<-createDataPartition(Titanic$Survived, p=0.3, list=FALSE)
Titanic_test<-Titanic[test_idx,] #create data testing
Titanic_train<-Titanic[-test_idx,] #create data training
nrow(Titanic_test)
nrow(Titanic_train)
##method 2
set.seed(123)
index<-sample(2, nrow(Titanic), replace=TRUE, prop=c(0.7, 0.3))
train<-Titanic[index==1,]
test<-Titanic[index==2,]

#Sample
x<-1:10
sample(x, 10) #permutation
sample(x, 4) #permutation
sample(x, 11) #permutation (error)
sample(x, 100, replace=TRUE) #permutation with replacement

#R Base
table(Titanic$Survived)
prop.table(table(Titanic$Survived))
Yes<-which(Titanic$Survived=="Yes") #List of all entries with Survived=Yes
No<-which(Titanic$Survived=="No") #List of all entries with Survived=No
length(Yes)
length(No)

sample(1:450, 863, replace=TRUE)
up<-c(sample(Yes, length(No), replace=TRUE), No)
length(up)
863*2

Titanic.up<-Titanic[up, ] #row number
View(Titanic.up)
str(Titanic.up)
table(Titanic.up$Survived)
prop.table(table(Titanic.up$Survived))

#using caret library
library(caret)
Titanic.up2<-upSample(Titanic[,-1], Titanic$Survived, yname="Survived")
str(Titanic.up2)
table(Titanic.up2$Survived)
prop.table(table(Titanic.up2$Survived))

#using SMOTE
library(remotes)
library(DMwR)
Titanic.up4<-SMOTE(Survived~., Titanic, perc.over=95.8)
table(Titanic.up4$Survived)
prop.table(table(Titanic.up4$Survived))
str(Titanic.up4)
