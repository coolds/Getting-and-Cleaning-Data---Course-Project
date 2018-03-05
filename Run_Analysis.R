##1 Merges the training and the test sets to create one data set.
##2 Extracts only the measurements on the mean and standard deviation for each measurement.
##3 Uses descriptive activity names to name the activities in the data set
##4 Appropriately labels the data set with descriptive variable names.
##5 From the data set in step 4, creates a second, independent tidy data 
##set with the average of each variable for each activity and each subject.



####################################################################
setwd("H:/tmp/2017_2018courses/2018DataScience/Course3_project")
getwd()
library(plyr)
library(data.table)

####################################################################
## Step1. Merge the training and the test sets to create one data set



subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt",header=FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)


subject_test = read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt",header=FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)


## combine datasets into one
subjectdata <- rbind(subject_train, subject_test)
xdata <- rbind(x_train, x_test)
ydata <- rbind(y_train, y_test)




####################################################################
## Step2 Extracts only the measurements on the mean and standard deviation 
## for each measurement.

features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)

featurenames <- grep(".*mean.*|.*std.*", features[,2])


xdata_mean_std <- xdata[,grep(".*mean.*|.*std.*", features[,2])]


names(xdata_mean_std) <- features[featurenames,2] 

## name all the column names of xdata_mena_std (xdata subset) for mean and std



####################################################################
## Step3 Uses descriptive activity names to name the activities in the data set
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

ydata[,1] <- activities[ydata[,1],2]

names(ydata) <- "activity"  ## name ydata column name to activity
##View(ydata)


####################################################################
##Step4 Appropriately labels the data set with descriptive variable names.


names(subjectdata) <- "subject"
summary(subjectdata)


alldata <- cbind(xdata_mean_std, ydata, subjectdata)

## Appropriately label column names

fullfeaturenames <- features[featurenames,2]

fullfeaturenames <- gsub('-mean', 'Mean', fullfeaturenames)
fullfeaturenames <- gsub('-std', 'StandardDeviation', fullfeaturenames)
fullfeaturenames <- gsub('Freq', 'Frequency', fullfeaturenames)
fullfeaturenames <- gsub('[-()]', '', fullfeaturenames)




####################################################################
## Step5  From the data set in step 4, creates a second, independent tidy dataset
## with the average of each variable for each activity and each subject.

colnames(alldata) <- c(fullfeaturenames, "activity", "subject")

names(alldata)
avgdata <- aggregate(. ~subject + activity, alldata, mean)
write.table(avgdata, file = "tidy.txt", row.name = FALSE)








