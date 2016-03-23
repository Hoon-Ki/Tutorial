Titanic.train<- read.csv("C:\\Users\\Administrator\\Desktop\\DataScience\\Tutorial\\Titanic\\train.csv",header = T)
Titanic.test<- read.csv("C:\\Users\\Administrator\\Desktop\\DataScience\\Tutorial\\Titanic\\test.csv",header = T)

head(Titanic.train)
summary(Titanic.train)
summary(Titanic.train$Sex)
prop.table(table(Titanic.train$Sex,Titanic.train$Survived))#total proprotion, this is not that i want.
prop.table(table(Titanic.train$Sex,Titanic.train$Survived),1) #1 : row 2: column

# This is test set, put the pattern that i found in train set.
#Notice! in here we have no Survived Attribute, instead they(kaggle) have ! cause it's competition.
# think about it, if we know the answer, we can fix it whenever we want. in the end, almost everyone will get the best result, so Kaggle limits the submission to 3. 

Titanic.test$Survived<-0
Titanic.test$Survived[Titanic.test$Sex == 'female']<-1 # apply what we found on train data
# we will submit this test set that i put my model(pattern) in , so we can see what the result is.

summary(Titanic.train$Age)

# 'age' variable only had a few values.
# Now we have a continuous variable which makes drawing proportion tables almost useless, as there may only be one or two passengers for each age!
# So, let’s create a new variable, Child, to indicate whether the passenger is below the age of 18

Titanic.train$Child<-0
Titanic.train$Child[Titanic.train$Age<18] <-1
aggregate(Survived ~ Child + Sex, data=Titanic.train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=Titanic.train, FUN=length)
aggregate(Survived ~ Child + Sex, data=Titanic.train, FUN=function(x) {sum(x)/length(x)})
# Well, it still appears that if a passenger is female most survive, and if they were male most don’t, regardless of whether they were a child or not.

# While the class variable is limited to a manageable 3 values, the fare is again a continuous variable that needs to be reduced to something that can be easily tabulated.
# Let’s bin the fares into less than $10, between $10 and $20, $20 to $30 and more than $30 and store it to a new variable:
Titanic.train$Fare2 <- '30+'
Titanic.train$Fare2[Titanic.train$Fare <30 & Titanic.train$Fare2 >= 20] <- '20-30'
Titanic.train$Fare2[Titanic.train$Fare <20 & Titanic.train$Fare2 >= 10] <- '10-20'
Titanic.train$Fare2[Titanic.train$Fare <10 ] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex , data=Titanic.train, FUN=function(x) {sum(x)/length(x)})

Titanic.test$Survived <-0
Titanic.test$Survived[Titanic.test$Sex == 'female'] <-1
Titanic.test$Survived[Titanic.test$Sex == 'female' & Titanic.test$Pclass == 3 & Titanic.test$Fare >= 20] <-0

# decision tree
library(rpart)
#constructing decision tree

library(rpart)
fit<- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = Titanic.train, method = "class")
plot(fit)
text(fit)

#for better graphics

#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, Titanic.test, type = "class")
submit <- data.frame(Passengerld = Titanic.test$paasengerld, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

fit<- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = Titanic.train, method = "class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)



fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=Titanic.train,
             method="class", control=rpart.control( yourcontrols ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


# Feature engineering
# finding more feature among attributes that we didn't touch, such as ticket number, cabin and name
# Maybe, Name field could give us a new preditive attribute.

Titanic.train$Name[1] 

# we need to combine trainset and testset, to extract titles to make new variables
# if we didn't do this, we can't apply what we've done in train set to test set.
# cause test set didn't have the new variable that there is in train set.

Titanic.test$Survived <-NA
combi <- rbind(Titanic.train, Titanic.test)

combi$Name <- as.character(combi$Name) # To chop this name string, it needs to be trasformed to string.
combi$Name[1]

# now we split the name string by ",."
strsplit(combi$Name[1], split='[,.]')

# we need that surname but right in front of it , there is a space. so first we need to get to that surname by using index [[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# apply this fuction to all values of the name
# puts it to the new variable "Title"
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

# get rid of the space from the begining of the titles.
combi$Title <- sub(' ', '', combi$Title)

# This is a nice new attribute.
table(combi$Title)

# there are a few very rare titles in here that won’t give our model much to work with, so let’s combine a few of the most unusual ones
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# change the variable type back to a factor, as these are essentially categories that we have created.
combi$Title <- factor(combi$Title)


# making another new variable, FamilySize
combi$FamilySize <- combi$SibSp + combi$Parch + 1 #including me, there are spouse, brother and sister, children and parents.
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# we just thought about a large family having issues getting to lifeboats together,
# but maybe specific families had more trouble than others?
# We could try to extract the Surname of the passengers and group them to find families.
# No two family-Johnson’s should have the same FamilySize variable on such a small ship. So let’s first extract the passengers’ last names.
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# let’s knock out any family size of two or less and call it a “small” family. This would fix the Johnson problem too.
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]


# Given we were originally hypothesising that large families might have trouble sticking together in the panic
# We then need to overwrite any family IDs in our dataset for groups that were not correctly identified and finally convert it to a factor
# let’s subset this dataframe to show only those unexpectedly small FamilyID groups.

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


# new variables are ready, so divide again.

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
# 0.79426


# building RF

summary(combi$Age)

# We now also want to use the method=”anova” version of our decision tree, as we are not trying to predict a category any more, but a continuous variable.
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova") 
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),]) # Imputate missing values with predited values.
summary(combi)
summary(combi$Embarked)
which(combi$Embarked == '') # to find out which index is the missing value.
combi$Embarked[c(62,830)] = "S" # imputate the missings with most frequent value, S(South hampton).
combi$Embarked <- factor(combi$Embarked)

# play the imputation
# Random forest can't digest factors with more than 32 levels.
# so we need to norrow it down.
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#install.packages('randomForest')
library(randomForest)
set.seed(415) #you need to fix the seed number. unless you do, you can't compare the results. and then we don't know if this is going wrong or what.
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit) # To see what variables were important.

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#0.79426

# Let’s try a forest of conditional inference trees. 
# They make their decisions in slightly different ways, using a statistical test rather than a purity measure,
# but the basic construction of each tree is fairly similar.

#install.packages('party')
library(party)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")

#0.81340
