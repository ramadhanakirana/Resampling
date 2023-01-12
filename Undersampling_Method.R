install.packages("caret")
install.packages("ROSE")

databank<-read.csv("D:/Actuarial Simulation/bank-additional-full.csv", sep=";")
library("caret")
library("rpart") #used for data partitioning
str(databank$y)

#change the variable y: character to factor
databank<-within(databank, (
  y<-as.factor(y)
))
str(databank$y)

nrow(databank) #look at the number of observations

#look at the class distribution of the target variable
table(databank$y)
prop.table(table(databank$y))

#data partition
set.seed(100)
test_idx<-createDataPartition(databank$y, p=0.3, list=FALSE)
databank_test<-databank[test_idx,] #create data testing
databank_train<-databank[-test_idx,] #create data testing
nrow(databank_test)
nrow(databank_train)

#undersampling
set.seed(200)
down_train<-downSample(databank_train[, -ncol(databank_train)], databank_train$y, list=FALSE, yname="Class")
up_train<-upSample()

#yes-no distribution
table(databank_train$y) #before undersampling
table(down_train$Class) #after undersampling

##perform undersampling with ROSE (Random Over Sampling Examples) package
data_balance_under<-ovun.sample(y~., data=databank_train, method="under", N=3248*2, seed=1)$data
table(data_balance_under$y)
