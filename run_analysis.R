## Getting and Cleaning Data - Course Project
## 
## 
library(dplyr)
library(reshape2)

## read in activity labels
act_labels <- read.csv("activity_labels.txt", header = FALSE,sep=" ")
## create better column names
colnames(act_labels) <- c("activity_id", "activity")

## read in feature labels
feature_labels <- read.csv("features.txt", header = FALSE,sep=" ")
## create better column names
colnames(feature_labels) <- c("feature_column", "feature")

## create boolean vector to indicate columns/variables that have mean or std 
keep <- (grepl("mean", feature_labels$feature, ignore.case = TRUE) | 
             grepl("std", feature_labels$feature, ignore.case = TRUE))
##      multiply against vector of integers to subset 
##      (gives us columns numbers of data to keep)
keep_indices <- keep * c(1:nrow(feature_labels))
kept <- as.vector(feature_labels$feature)[keep]

## test data
## read in test subject data
test_subjects <- read.csv("test/subject_test.txt", header=FALSE)
## recast as factor
test_subjects$V1 <- as.factor(test_subjects$V1)
## read in test activity data
test_activities <- read.csv("test/y_test.txt", header=FALSE)
## recast as factor
test_activities$V1 <- as.factor(test_activities$V1)
## give better column names
colnames(test_activities) <- "activity_id"
colnames(test_subjects) <- "subject_id"
## read in test data 
test_data <- read.table("test/X_test.txt", header=FALSE, sep="")
## give columns the right names
colnames(test_data) <- feature_labels$feature
## subset data for only columns we want
test_data <- subset(test_data, select=keep_indices)


## training data
## read in training subject data
train_subjects <- read.csv("train/subject_train.txt", header=FALSE)
## recast as factor
train_subjects$V1 <- as.factor(train_subjects$V1)
## read in training activity data
train_activities <- read.csv("train/y_train.txt", header=FALSE)
## recast as factor
train_activities$V1 <- as.factor(train_activities$V1)
## give better column names
colnames(train_activities) <- "activity_id"
colnames(train_subjects) <- "subject_id"
## read in training data
train_data <- read.table("train/X_train.txt", header=FALSE, sep="")
## give columns the right names
colnames(train_data) <- feature_labels$feature
## subset data for only columns we want
train_data <- subset(train_data, select=keep_indices)


## get rid of keep_indices since we are done with it
remove(keep_indices)

## pull together data sets
test_combined <- cbind(test_subjects, test_activities, test_data)
train_combined <- cbind(train_subjects, train_activities, train_data)
## clean up component data frames
remove(test_data)
remove(train_data)
remove(test_activities)
remove(train_activities)
remove(test_subjects)
remove(train_subjects)

## pull together complete data set from test and train data and clean up
total_data <- rbind(test_combined, train_combined)
remove(test_combined)
remove(train_combined)

## merge data to get activity names in from labels
merged_data <- merge(total_data, act_labels, all=TRUE, sort=FALSE)
## get rid of total data
remove(total_data)

## subset out activity id column since we have activity name in table now
merged_data <- subset(merged_data, select=-(activity_id))
## reorder to tidy up data by moving activity to be second column
fitness_data <- merged_data[c(1,ncol(merged_data),2:(ncol(merged_data)-1))]
## remove merged data as we are done with it
remove(merged_data)

## melt data to allow for summary by mean by subject_id and activity
tidy_data_melt <- melt(fitness_data, id=c("subject_id", "activity"), measure.vars=kept)
## summarize with means of measurements by subject_id and activity
tidy_set_wide <- dcast(tidy_data_melt, subject_id + activity ~ variable, mean)

## Extra - could make narrow tidy data set, but I feel that this isn't as easily digested by a human
##tidy_set_narrow <- melt(tidy_set_wide, id=c("subject_id", "activity") 

write.table(tidy_set_wide, file="fitness_tidy_data.txt", row.names=FALSE)
