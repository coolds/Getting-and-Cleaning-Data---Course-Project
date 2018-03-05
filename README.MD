---
#title: "Getting and Cleaning Data - Course Project"
---

This is a course project for Coursera course Getting and Cleaning Data. 


The dataset besing used for this project is available

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>


The repository contains the following files:
* README.md - explain the analysis file
* Run_Analysis.R - R script that was used to create the dataset
* tidy.txt - the final dataset for this project created by running Run_Analysis.R
* CodeBook.md - describes the contents of the dataset


## Creating the Dataset
The Run_Analysis.R is used to create the final dataset using the following steps:
1. Download the dataset if it does not exist.
2. Load data.
3. Merge the training and test datasets into one. 
4. Extract only the measurements on the mean and standard deviation for each measurement.
5. Use descriptive activity names to name the activities in the data set
6. Appropriately label the data set with descriptive variable names.
7. Create a second, independent tidy dataset with the average of each variable for each activity and each subject
8. Write the dataset to tidy.txt file.




























