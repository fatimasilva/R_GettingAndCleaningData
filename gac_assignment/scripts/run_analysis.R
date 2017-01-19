# R script for Getting and Cleaning Data Week 4 assignment
# It downloads the dataset needed for the assignment and does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(tidyr)
library(dplyr)

# Clean environment
rm(list = ls())

# Create data folder
if(!file.exists("../data")) { dir.create("../data/")}
if(!file.exists("../tidy")) { dir.create("../tidy/")}

# Download the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl, "../data/getdata_projectfiles_UCI HAR Dataset.zip", method = "curl")

# Unzip file
unzip("../data/getdata_projectfiles_UCI HAR Dataset.zip", exdir = "../data/", list = FALSE)

### 1. Merge the training and the test sets to create one data set
# Get features (they are needed to name the variables later)
featuresdf <- read.table("../data/UCI HAR Dataset/features.txt")
features <- featuresdf$V2

# Get activities mapping (needed to name the activities later)
activities <- read.table("../data/UCI HAR Dataset/activity_labels.txt", sep = "")

# keeping the underscores for easy reading, but transform to lowercase
activities <- transmute(activities, activityid = V1, activity = as.factor(tolower(V2)))

# Prepare the test dataset 
testdf <- read.table("../data/UCI HAR Dataset/test/X_test.txt", sep = "")

## The separator needs to be the default one for read.table ('white space',
## that is one or more spaces, tabs, newlines or carriage returns). There are
## one or more spaces dividing the columns, if sep = " " is used we get more 
## than 561 columns, different number in each row

## Add features as variable names of the test dataset
names(testdf) <- features

## Add subject ids to the test dataset
testsubject <- read.table("../data/UCI HAR Dataset/test/subject_test.txt")
testdf$subject <- testsubject$V1

## Add activity ids to the test dataset
testactivities <- read.table("../data/UCI HAR Dataset/test/y_test.txt")
testdf$activityid <- testactivities$V1

# Prepare train dataset

traindf <- read.table("../data/UCI HAR Dataset/train/X_train.txt", sep = "")

## Add features as column names
names(traindf) <- features

## Add subject ids to the train dataset
trainsubject <- read.table("../data/UCI HAR Dataset/train/subject_train.txt")
traindf$subject <- trainsubject$V1

## Add activity ids to the train dataset
trainactivities <- read.table("../data/UCI HAR Dataset/train/y_train.txt")
traindf$activityid <- trainactivities$V1

# Combine train and test datasets
fulldf <- rbind(testdf, traindf)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

# Extract the mean and std variables. They have tags: '-mean()-', '-std()-', '-meanFreq()-'. 
meanvariables <- grep("-mean()-", names(fulldf), fixed = TRUE)
stdvariables <- grep("-std()-", names(fulldf), fixed = TRUE)
meanfreqvariables <- grep("-meanFreq()-", names(fulldf), fixed = TRUE)

## get the indexes of the variables we want to keep 
## (mean and std plus activity and subject)
workingvariables <- c(meanvariables,stdvariables, meanfreqvariables,562:563)

## subset the dataset to keep only those variables
workingdf <- fulldf[, workingvariables]

# 3. Use descriptive activity names to name the activities in the data set
# The activity labels in the activity_labels.txt file already describe well 
# the type of activity, just map to the activity identifiers
# add activity labels
workingdf <- merge(workingdf, activities, all.x = TRUE, 
                   by.x = "activityid", by.y = "activityid")

# Remove the activity ids
workingdf <- workingdf[,-1]

# 4. Appropriately label the data set with descriptive variable names
# get current variable names
labels <- names(workingdf)

# Transform for easy reading/descriptive
labels <- gsub('^(.*)-std\\(\\)(.*)$', '\\1\\2-std', labels)
labels <- gsub('^(.*)-mean\\(\\)(.*)$', '\\1\\2-mean', labels)
labels <- gsub('^(.*)-meanFreq\\(\\)(.*)$', '\\1\\2-meanfreq', labels)
labels <- gsub('tBodyAcc', 'bodyacceltime', labels)
labels <- gsub('tGravityAcc', 'gravityacceltime', labels)
labels <- gsub('tBodyGyro', 'angveloctime', labels)
labels <- gsub('Jerk', 'jerk', labels)
labels <- gsub('BodyAcc', 'bodyaccel', labels)
labels <- gsub('GravityAcc', 'gravityaccel', labels)
labels <- gsub('BodyGyro', 'angveloc', labels)

names(workingdf) <- labels

# 5. From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.

# Group by activity and subject
bysubjectactivity <- group_by(workingdf, subject, activity)
# Get the average of each variable for each activity and subject
resultsdf <- summarise_all(bysubjectactivity, funs(avg = mean))

# rename the variables 
names(resultsdf) <- gsub('(.+)_avg','mean-\\1', names(resultsdf))

# Dump to file
write.table(resultsdf, file = "../tidy/means_by_subject_and_activity.tsv",
            quote = FALSE, row.names = FALSE, sep = "\t")