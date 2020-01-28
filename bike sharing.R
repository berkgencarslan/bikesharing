setwd("C:\\Users\\USER\\Desktop")
training_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
str(training_data_imp)
summary(training_data)
str(training_data)
dim(training_data)
dim(test_data)
glimpse(training_data)
glimpse(test_data)

install.packages("dplyr")
install.packages("caret")
install.packages("caretEnsemble")
install.packages("mice")
install.packages("doParallel")
install.packages("car")
install.packages("CatEncoders")
library(CatEncoders)
library(dplyr)
library(caret)
library(caretEnsemble)
library(mice)
library(doParallel)
library(car)
library(plyr)

training_data[training_data==""]<-NA
test_data[test_data==""]<-NA
#replicate our sets
training_data_bind <- training_data
test_data_bind <- test_data
#create a new column "set" to label the observations
training_data_bind$set <- "TRAIN"
test_data_bind$set <- "TEST"
#merge them into 1 single set

a <- read.table("rowsnotneeded.txt")
class(a)
a.list <- as.list(as.data.frame(t(a)))

training_data_bind$count -> count
training_data_bind$count = NULL
full_dataset <- rbind(training_data_bind, test_data_bind)



(na_count_full_imputed <-data.frame(sapply(training_data, function(y) sum(length(which(is.na(y)))))))
summary(training_data)
summary(training_data_imp)
summary(full_imputed)

full_dataset$windspeed <- NULL
full_dataset$temp <- NULL
full_dataset$season <- NULL
full_dataset$holiday <- NULL
full_dataset$workingday <- NULL
full_dataset$weather <- NULL
full_dataset$humidity <- NULL
full_dataset$atemp <- NULL


#subset the full_imputed_filtered dataset
training_data_imp <- subset(full_dataset, set == "TRAIN")
test_data_imp <- subset(full_dataset, set == "TEST")

#drop the "set" column, we don't need it anymore
training_data_imp$set <- NULL
test_data_imp$set <- NULL

test_data_imp$datetime -> testdate
training_data_imp$datetime -> traindate

hour <- format(strptime(testdate,"%Y-%m-%d %H:%M:%S"),'%H:%M')
hour2 <- format(strptime(traindate,"%Y-%m-%d %H:%M:%S"),'%H:%M')

test_data_imp$datetime <- NULL
training_data_imp$datetime <- NULL

test_data_imp$hour <- hour
training_data_imp$hour <- hour2

full_imputed_2 <- full_imputed


install.packages("fastDummies")
library(fastDummies)

test_data_imp$hour <- stri_sub(test_data_imp$hour, 1, -4)
training_data_imp$hour <- stri_sub(training_data_imp$hour, 1, -4)

#check dimensions
dim(training_data_imp)
dim(test_data_imp)
summary(training_data_imp)
training_data_imp

test_data_imp$hour <- as.numeric(as.character(test_data_imp$hour))
training_data_imp$hour <- as.numeric(as.character(training_data_imp$hour))

training_data_imp["count"] <- count
training_data_imp$hour <- training_data_imp$hour +1


install.packages("randomForest")
install.packages("party")
library(randomForest)
library(party)
install.packages("gbm")
library(gbm)


genmod<-gbm(count~.
            ,data=training_data_imp ## registered,casual,count columns
            ,var.monotone=NULL # which vars go up or down with target
            ,distribution="gaussian"
            ,n.trees=1200
            ,shrinkage=0.05
            ,interaction.depth=3
            ,bag.fraction = 0.5
            ,train.fraction = 1
            ,n.minobsinnode = 10
            ,cv.folds = 10
            ,keep.data=TRUE
            ,verbose=TRUE)

best.iter <- gbm.perf(genmod,method="cv")
print(pretty.gbm.tree(genmod, best.iter))
summary(genmod, n.trees=best.iter)

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(count ~., data = training_data_imp, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

options(digits=10)

pred.test <- predict(genmod, test_data_imp, best.iter, type="response")
summary(pred.test)

pred.test

pred.test[pred.test<0] <- 0
# create output file
output <- data.frame(datetime=testdate, count=yeni)
write.csv(output, file="bikeend.csv", quote=FALSE, row.names=FALSE)
write.table(output, file="bikeend.txt", sep = ",",quote=FALSE, row.names=FALSE)

test_data_imp$casual + test_data_imp$registered -> test_data_imp$yeni
test_data_imp$casual <- NULL
test_data_imp$registered <- NULL
test_data_imp$yeni -> yeni
yeni

lm1=lm(count ~ .,data=training_data_imp)
summary(lm1)



pred.test <- predict(lm1, test_data_imp)


plot(count ~ hour, training_data_imp)


rf <- randomForest(count~ .,train)
predicted = predict(rf, newdata=test)





testcount = test$count
rmsle <- function(testcount , predicted)
  sqrt(mean((log1p(testcount) - log1p(predicted))^2))

rmsle(testcount,predicted)



