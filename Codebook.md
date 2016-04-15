
Varma Dantuluri

Getting and Cleaning Data Course Project
Instructions for project The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Download the Data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")
Load required packages

###Load required packages
library(dplyr)
library(data.table)
library(tidyr)
Files in folder ‘UCI HAR Dataset’ that will be used are:

SUBJECT FILES
test/subject_test.txt
train/subject_train.txt

ACTIVITY FILES
test/X_test.txt
train/X_train.txt

DATA FILES
test/y_test.txt
train/y_train.txt




Read the above files and create data tables.
path <- "C:/Users/vdantulu/Downloads/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"
# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(path, "test" , "subject_test.txt" )))


# Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(path, "test" , "Y_test.txt" )))



#Read data files.
dataTrain <- tbl_df(read.table(file.path(path, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(path, "test" , "X_test.txt" )))


1. Merges the training and the test sets to create one data set.
# we need to merge traing and test using rbind function and rename the variables
mergedatasubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(mergedatasubject, "V1", "subject")
mergedataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(mergedataActivity, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature 
dataFeatures <- tbl_df(read.table(file.path(path, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
mergedataSubjAct<- cbind(mergedatasubject, mergedataActivity)
dataTable <- cbind(mergedataSubjAct, dataTable)


2. Extracts only the measurements on the mean and standard deviation for each measurement.
# The measurnment datais loaded in the features.txt file.
# Beed ti read features.txt and extracting the mean and SD
dataMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

dataMeanStd <- union(c("subject","activityNum"), dataMeanStd)
dataTable<- subset(dataTable,select=dataMeanStd) 


3. Uses descriptive activity names to name the activities in the data set

dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# create dataTable with variable means sorted by subject and Activity

dataTable$activityName <- as.character(dataTable$activityName)
aggregateData<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(aggregateData,subject,activityName))


4. Appropriately labels the data set with descriptive variable names.
head(str(dataTable),2)
#  $ subject                    : int  1 1 1 1 1 1 2 2 2 2 ...
#  $ activityName               : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
#  $ activityNum                : num  6 4 5 1 3 2 6 4 5 1 ...
#  $ tBodyAcc-mean()-X          : num  0.222 0.261 0.279 0.277 0.289 ...
#  $ tBodyAcc-mean()-Y          : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
#  $ tBodyAcc-mean()-Z          : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
#  $ tBodyAcc-std()-X           : num  -0.928 -0.977 -0.996 -0.284 0.03 ...

## change the names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# Names after rename
head(str(dataTable),6)
#  $ subject                                       : int  1 1 1 1 1 1 2 2 2 2 ...
#  $ activityName                                  : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
#  $ activityNum                                   : num  6 4 5 1 3 2 6 4 5 1 ...
#  $ timeBodyAccelerometer-MEAN()-X                : num  0.222 0.261 0.279 0.277 0.289 ...
#  $ timeBodyAccelerometer-MEAN()-Y                : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
#  $ timeBodyAccelerometer-MEAN()-Z                : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
#  $ timeBodyAccelerometer-SD()-X                  : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
#  $ timeBodyAccelerometer-SD()-Y                  : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
#  $ timeBodyAccelerometer-SD()-Z                  : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##write to text file on disk
write.table(dataTable, "CleanData.txt", row.name=FALSE)
The data has 180 rows
