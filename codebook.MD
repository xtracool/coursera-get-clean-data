# Introduction for Project for Getting and Cleaning Data course (run_analysis.R):
We are to take data from this site:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

and create R script called run_analysis.R that does the following:
  1.	Merges the training and the test sets to create one data set.
  2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
  3.	Uses descriptive activity names to name the activities in the data set
  4.	Appropriately labels the data set with descriptive variable names. 
5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Methodology:
1.	Familiarize with the data set by visiting this page:
  o	http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
2.	Next create a new project in R Studio
3.	Download project data from here:
  o	https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
4.	Unzip the files into the R studio working directory, the directory should be called “UCI HAR Dataset”
o	In this directory you will find 3 txt files that describe in human readable form the attributes of the data and two directories:
  -test – there are 3 txt files that will be used: subject_test.txt, X_test.txt, y_test.txt
  -train – there are 3 txt files that will be used: subject_train.txt, X_train.txt, y_train.txt
  -Inertial Signals folders in both directories will not be used.
  -subject txt files has the id of the individual
  -X txt files has data measurements from the phone
  - y txt files has the activity type
5.	Based on the project requirements, identify what R libraries would make programming easier.  Dplyr would be useful.
6.	In the UCI HAR Dataset, read the “README.txt” to get description of files.
7.	From the test directory load: subject_test, X_test, y_test files into varibles: s_test,  X_test, y_test  respectively
8.	From the train directory load: subject_ train, X_test, y_ train files into varibles: s_ train,  X_ train, y_ train respectively
9.	The “subject” txt files each have 2947 observations and 1 variable
10.	The “X” files each have 2947 observations and 561 variables
11.	The “y” files each have 2947 observations and 1 variable.
12.	Load in features.txt and activities.txt so numerical values can be replaced with descriptions.
13.	Since we have to remove columns that do have “std” or “mean”, assign columns to the data  for both X_test and X_train data frames.
14.	Remove duplicate columns so the select() will work.
15.	From X_test and X_train select columns that are related to “std” and “mean”
16.	Merge ALL the test data frames together: s_test, X_test, y_test and add column names
17.	Merge ALL the train data frames together: s_train, X_train, y_train and add column names
18.	“Part 1” of project
  o	Combine the test and train data frames to create a single data frame.
  o	Remove any possible duplicate columns
19.	“Part 3” of project
  o	Apply replace numerical values with text labels for activities
20.	“Part 5” of project
  o	For each subject and subject’s activity, calculate the mean value for each of the measurements types.
  o	Save the data to file called: tidy_data_set.txt

