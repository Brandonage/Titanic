setwd('/Users/alvarobrandon/Dropbox/Master/Data Mining/Project')

library(plyr)
library(foreign)

# Load the data sets
train <- read.csv("train.csv") 
test <- read.csv("test.csv")    
data = c(train, test)


test$Survived<-0

# Convert catagorical variables to factors
train$Survived <- factor(train$Survived)
train$Sex <- factor(train$Sex)
train$Pclass <- factor(train$Pclass)
test$Survived <- factor(test$Survived)
test$Sex <- factor(test$Sex)
test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)




library(stringr)
train$Sex.Name <- 0
test$Sex.Name <- 0


### We change the wide range of title names to something smaller with only Miss, Mr, Mast and Mrs
train$Sex.Name[!is.na(str_extract(train$Name, "Mr"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Mrs"))] <- "Mrs"
train$Sex.Name[!is.na(str_extract(train$Name, "Mme"))] <- "Mrs"
train$Sex.Name[!is.na(str_extract(train$Name, "Miss"))] <- "Miss"
train$Sex.Name[!is.na(str_extract(train$Name, "Ms"))] <- "Miss"
train$Sex.Name[!is.na(str_extract(train$Name, "Mlle"))] <- "Miss"
train$Sex.Name[!is.na(str_extract(train$Name, "Capt"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Major"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Col"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Master"))] <- "Mast"
train$Sex.Name[!is.na(str_extract(train$Name, "Rev"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Dr"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Don"))] <- "Mr"
train$Sex.Name[!is.na(str_extract(train$Name, "Countess"))] <- "Mrs"
train$Sex.Name[!is.na(str_extract(train$Name, "Jonkheer"))] <- "Mr"

test$Sex.Name[!is.na(str_extract(test$Name, "Mr"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Mrs"))] <- "Mrs"
test$Sex.Name[!is.na(str_extract(test$Name, "Mme"))] <- "Mrs"
test$Sex.Name[!is.na(str_extract(test$Name, "Miss"))] <- "Miss"
test$Sex.Name[!is.na(str_extract(test$Name, "Ms"))] <- "Miss"
test$Sex.Name[!is.na(str_extract(test$Name, "Mlle"))] <- "Miss"
test$Sex.Name[!is.na(str_extract(test$Name, "Capt"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Major"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Col"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Master"))] <- "Mast"
test$Sex.Name[!is.na(str_extract(test$Name, "Rev"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Dr"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Don"))] <- "Mr"
test$Sex.Name[!is.na(str_extract(test$Name, "Countess"))] <- "Mrs"
test$Sex.Name[!is.na(str_extract(test$Name, "Jonkheer"))] <- "Mr"

test$Name[test$Sex.Name == 0]
train$Name[train$Sex.Name == 0]

train$Sex.Name <- factor(train$Sex.Name)
test$Sex.Name <- factor(test$Sex.Name)


#Model missing values for age and fare
full <- join(test, train, type = "full")
names(full)

## We use linear regression to calculate the missing values for Fare and Age
Age.lm <- lm(Age ~ Pclass + SibSp + factor(Sex.Name), data = full)
#Age.lm <- lm(Age ~ Pclass + Sex + SibSp, data = full)
#Fare.lm<- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age, data = full)
Fare.lm<- lm(Fare ~ Pclass + Sex + SibSp + Parch, data = full)


# Replace missing values in AGE and Fare with prediction
sum(is.na(train$Age))
sum(is.na(test$Age))
sum(is.na(train$Fare))
sum(is.na(test$Fare))
train$Age[is.na(train$Age)] <- predict(Age.lm, train)[is.na(train$Age)]
test$Age[is.na(test$Age)] <- predict(Age.lm, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(Fare.lm, test)[is.na(test$Fare)]
sum(is.na(train$Age))
sum(is.na(test$Age))
sum(is.na(train$Fare))
sum(is.na(test$Fare))

train$Embarked[train$Embarked == ""] <- "S"
train$Embarked <- factor(train$Embarked)

### Create "fare-distance" variable
# fare-distance = fare - mean(fare of pclass)
# Are those who pay less than the average for a ticket less likely to survive?

# Find the mean fare for each pclass
class1 <- subset(full, Pclass == 1)
class2 <- subset(full, Pclass == 2)
class3 <- subset(full, Pclass == 3)
fare1 <- mean(class1$Fare, na.rm = TRUE)
fare2 <- mean(class2$Fare, na.rm = TRUE)
fare3 <- mean(class3$Fare, na.rm = TRUE)

# Create fare_avg column
train$fare_avg[train$Pclass == 1] <- fare1
train$fare_avg[train$Pclass == 2] <- fare2
train$fare_avg[train$Pclass == 3] <- fare3
test$fare_avg[test$Pclass == 1] <- fare1
test$fare_avg[test$Pclass == 2] <- fare2
test$fare_avg[test$Pclass == 3] <- fare3

# Create fare-distance metric for Train
train <- transform(train, fare.distance = Fare - fare_avg)
train <- train[, !names(train) %in% c("fare_avg")]
test <- transform(test, fare.distance = Fare - fare_avg)
test <- test[, !names(test) %in% c("fare_avg")]

### Add family column
train$family <- NA
test$family <- NA
train$family[which(train$SibSp != 0 | train$Parch != 0)] <- 1
train$family[which(train$SibSp == 0 & train$Parch == 0)] <- 0
test$family[which(test$SibSp != 0 | test$Parch != 0)] <- 1
test$family[which(test$SibSp == 0 & test$Parch == 0)] <- 0
test$family <- factor(test$family)
train$family <- factor(train$family)
test$familia <- test$SibSp + test$Parch
train$familia <- train$SibSp + train$Parch

###  Scale the non factors
###
train$age_scale <- (train$Age-min(train$Age))/(max(train$Age-min(train$Age)))
train$fare_scale <- (train$Fare-min(train$Fare))/(max(train$Fare-min(train$Fare)))

test$age_scale <- (test$Age-min(test$Age))/(max(test$Age-min(test$Age)))
test$fare_scale <- (test$Fare-min(test$Fare))/(max(test$Fare-min(test$Fare)))

## Change Cabin as Factors


# Classifying with stacking

newtrain <- train[,c(2,3,5,6,13,16,10)]
newtest <- test[,c(2,4,5,9,13,16)]
library(mlr)

names(newtrain)

task = makeClassifTask(id = "titanic",data=newtrain, target="Survived")
rdesc = makeResampleDesc(method = "Holdout")

#learner = makeLearner("classif.blackboost")

learner1 = makeLearner("classif.ksvm")
learner2 = makeLearner("classif.kknn")
#learner3 = makeLearner("classif.naiveBayes")
learner3 = makeLearner("classif.J48")

## We stack learners with makeStackedLearner

stacked = makeStackedLearner(list(learner1, learner2, learner3),
                             super.learner = "classif.logreg")


res = resample(learner = stacked, task = task, resampling = rdesc,models=TRUE)
mod = train(stacked, task)

newdata.pred = predict(mod,newdata=newtest)
Surv <- newdata.pred$data

submission <- cbind(test[,1],Surv)
colnames(submission) <- c("PassengerId","Survived")
write.table(submission,file="/Users/alvarobrandon/Dropbox/Master/Data Mining/Project/submission.csv",sep=",",row.names=FALSE)


###### GOING TO TRY WITH BAGGING

base.lrn = makeLearner("classif.J48")
base.res = resample(learner = base.lrn, task = task, resampling = rdesc, models = TRUE)

wrapped.lrn = makeBaggingWrapper(base.lrn, bw.iters = 100)
wrapped.res = resample(learner = wrapped.lrn, task = task, resampling = rdesc, models = TRUE)

mod = train(wrapped.lrn, task)


## We also try to classify with a generalised linear model
## I change the cabin to a factor
train[,11] <- sapply( train.df[,11], function(x) ifelse(x=='',1,2))
library(MASS)

titanic.glm <- glm(factor(Survived)~factor(Pclass)+factor(Sex)+factor(family)+familia+factor(Cabin)+
                     age_scale+factor(Sex):factor(Pclass)+familia:factor(Sex)+
                     factor(Cabin):factor(Sex)+factor(Sex):age_scale+factor(family):age_scale+familia:age_scale+factor(family):factor(Pclass)
                   + fare_scale,data=train, family=binomial(link=logit))


train$survived_pred <- predict(glm2, train, type = "response")
train$survived_pred[train$survived_pred >= 0.5] <- 1
train$survived_pred[train$survived_pred < 0.5] <- 0

# Make a prediction with our probit on TEST
test$survived <- predict(glm1, test, type = "response")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "logit-03.csv")
hatt<-cbind(test$PassengerId,test$survived)
write.csv(hatt, "glm1.csv",row.names=FALSE)


test$survived <- predict(glm2, test, type = "response")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "logit-02.csv")







