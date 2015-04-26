  # Data Science Course
  # Getting and Cleaning Data - Homework Project
  
  library(dplyr)
  
  # download zip file and unzip into working directory
  file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  zipfile = "UCI_HAR_Data.zip"
  download.file(file, zipfile, method="curl")
  unzip(zipfile, exdir="data")

  
  # load in test data
  s_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

  s_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
    
  features <- read.table("UCI HAR Dataset/features.txt")
  activities <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  #
  # Meeting the requirements of the assignment
  # 
  
  # Part 4 (out of order) - apply column names to data
  colnames(x_test) <- t(features[,2])  # apply matrix transpose
  colnames(x_train) <- t(features[,2])  # apply matrix transpose
  
  # Part 2 (out of order) - extract only mean and std deviation columns
  x_test <- x_test[, !duplicated(colnames(x_test))]  
  x_test <- select(x_test, contains("std"), contains("mean"))
  x_train <- x_train[, !duplicated(colnames(x_train))]    
  x_train <- select(x_train, contains("std"), contains("mean"))
  
  # merge the test data frames together, and add col names
  x_test$activity <- y_test[,1]
  x_test$subject <- s_test[,1]
  
  # merge the train data frame together, and add col names
  x_train$activity <- y_train[,1]
  x_train$subject <- s_train[,1]
  
  # Part 1 - create one data set, eliminate duplicate columns
  data <- rbind( x_test, x_train )
  data <- data[, !duplicated(colnames(data))]
    
  # Part 2 - extract mean and standard deviation
  # see above
  
  # Part 3 - apply names from activity_labels.txt to data
  data[ data$activity == 1, ]$activity <- as.character( activities[1,2] )
  data[ data$activity == 2, ]$activity <- as.character( activities[2,2] )
  data[ data$activity == 3, ]$activity <- as.character( activities[3,2] )  
  data[ data$activity == 4, ]$activity <- as.character( activities[4,2] )
  data[ data$activity == 5, ]$activity <- as.character( activities[5,2] ) 
  data[ data$activity == 6, ]$activity <- as.character( activities[6,2] ) 
  
  # Part 4 - see above
  
  # Part 5 - create independent tidy data set
  data <- aggregate(. ~subject + activity, data, mean)
  data <- data[ order(data$subject, data$activity),]
  write.table(data, file = "tidy_data_set.txt", row.name = FALSE)
