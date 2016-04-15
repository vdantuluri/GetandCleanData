#Create a script and download it to the data directory

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

#unzip the file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Change the characterset
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

##Load Required packages
library(dplyr)
library(data.table)
library(tidyr)


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


# The measurnment datais loaded in the features.txt file.
dataMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

dataMeanStd <- union(c("subject","activityNum"), dataMeanStd)
dataTable<- subset(dataTable,select=dataMeanStd) 



dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# create dataTable with variable means sorted by subject and Activity

dataTable$activityName <- as.character(dataTable$activityName)
aggregateData<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(aggregateData,subject,activityName))

#Get the first 2 hours
head(str(dataTable),2)

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
##write to text file on disk
write.table(dataTable, "CleanData.txt", row.name=FALSE)


