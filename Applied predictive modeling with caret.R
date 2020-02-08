require(caret)
require(AppliedPredictiveModeling)
require(dplyr)
require(ggplot2)
require(plotly)
require(MASS)
require(e1071)
Data1<-cbind(predictors,classes)
str(predictors)
str(classes)
View(Data1)

#Quick scatter plot for the data to see the trend
g1<-ggplot(data=Data1,aes(PredictorA,PredictorB,color=classes))+
  geom_point(alpha=0.5)+labs(title = "Predictors relations wrt to class variable")+
  facet_wrap(aes(theme=classes))

#Producing an interactive scatter plot
ggplotly(g1)
#Data splitting
table(classes)
#The data is faily balanced. we can split the data using simple random sampling from base R or 
#We can use createdata partition from caret
set.seed(123)
index1<-sample(seq(1,nrow(Data1)),replace = FALSE)
length(index1)

#Take 80% of the data as test set using base R
Rand.Train<-predictors[index1[1:166],]
Rand.Train

class.train<-classes[index1[1:166]]

length(class.train)

Rand.test<-predictors[-index1[1:166],]

class.test<-classes[-index1[1:166]]

###Simmilar kind of data splitting can be done using caret
index2<-createDataPartition(classes,p=0.8,list = FALSE)
 index2
New.train<-predictors[index2,] 
nrow(New.train)

New.class.train<-classes[index2]

length(New.class.train)

#Testing set will be

New.test<-predictors[-index2,]

nrow(New.test)

test.class.new<-classes[-index2]

####Resampling using caret

#To create repeated training/test splits we can do as follows

indx11<-createDataPartition(classes,times=5,p=0.8)
 indx11
 
 #This index can be used to create repeated training and test splits as follow 
 
 rpt.train1<-predictors[indx11$Resample1,]#First training set
head(rpt.train1) 

#To create bootstrap sample
set.seed(124)
Bootsamp1<-createResample(classes,times = 5,list = T)

Bootsamp1

Bootstrai1<-predictors[Bootsamp1$Resample1,]
#Note that bootstrap use all data points to create reamples so it is better to take 
#some percentage of data aside for the testing porposes before carries the procedure

#k folds cross validation
folds<-createFolds(classes,k=10,list = T,returnTrain = T)
folds

train.fold1<-predictors[folds$Fold01,] #1st fold train set

head(train.fold1)
 #To create repeated cross validation splits we do as follows
rptfolds<-createMultiFolds(classes,k=10,times=5)#This will create a random indices of 10 
#folds repeated 5 times ie 50 resamples in one iteration

train.multfold1<-predictors[rptfolds$Fold01.Rep1,] #First train resample
head(train.multfold1)

####Fitting basic models using caret
#knn model. Let fit 5 knn model using caret

fit1<-knn3(x=New.train,y=New.class.train,k=5) #Fits knn model with 5 neighbors
summary(fit1)
fit1$learn

#Predicting the new set of data
pre1<-predict(fit1,New.test,type = "class")# get the prediction
pre1

c1<-table(pre1,test.class.new)

confusionMatrix(c1)# Get model fits parameters


