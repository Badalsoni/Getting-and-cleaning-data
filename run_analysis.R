title: "Getting and cleaning dataset"
author: "Badal Soni"
date: "11/22/2020"

rm(list=ls())
library(dplyr)

###Downloading the dataset using the url

if(!file.exists("data")){
  dir.create("data")
}
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "./data/getting_cleaning_data.zip")


# Checking if folder exists

if (!file.exists("data/getting_cleaning_data.zip")){ 
  unzip(getting_cleaning_data.zip) 
}

###reading all file of the folder

features <- read.table("data/getting_cleaning_data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
x_test <- read.table("data/getting_cleaning_data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("data/getting_cleaning_data/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("data/getting_cleaning_data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("data/getting_cleaning_data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("data/getting_cleaning_data/UCI HAR Dataset/train/y_train.txt", col.names = "code")
activities <- read.table("data/getting_cleaning_data/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("data/getting_cleaning_data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")


#1. Merges the training and the test sets to create one data set.

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.

summary(Merged_Data)
mean_sd <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
mean_sd

#3. Uses descriptive activity names to name the activities in the data set.
mean_sd$code <- activities[mean_sd$code, 2]
mean_sd$code


#4. Appropriately labels the data set with descriptive variable names.

names(mean_sd)[2] = "activity"
names(mean_sd)<-gsub("Acc", "Accelerometer", names(mean_sd))
names(mean_sd)<-gsub("Gyro", "Gyroscope", names(mean_sd))
names(mean_sd)<-gsub("BodyBody", "Body", names(mean_sd))
names(mean_sd)<-gsub("Mag", "Magnitude", names(mean_sd))
names(mean_sd)<-gsub("^t", "Time", names(mean_sd))
names(mean_sd)<-gsub("^f", "Frequency", names(mean_sd))
names(mean_sd)<-gsub("tBody", "TimeBody", names(mean_sd))
names(mean_sd)<-gsub("-mean()", "Mean", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("-std()", "STD", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("-freq()", "Frequency", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("angle", "Angle", names(mean_sd))
names(mean_sd)<-gsub("gravity", "Gravity", names(mean_sd))


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- mean_sd %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
tidydata

str(tidydata)

write.table(tidydata, "tidyData.txt", row.name=FALSE)
