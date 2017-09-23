
library(dplyr)

# 1. Merges the training and the test sets to create one data set.

# setup: Read files, create dataframes that can be combined into tidy dataframes

# read activity labels and feature names
activityLabels_df <- read.table("activity_labels.txt")  # to be used later when replacing activity codes (1-6) with descriptive names
featuresNames <- read.table("features.txt")
# store all 152 feature names in a vector to be used to create column names
featureHeader <- as.vector(featuresNames$V2)

# read test group files
test_subjectCode <- read.table("subject_test.txt") 
test_activityCode <- read.table("y_test.txt") 
test_testset <- read.table("x_test.txt")
# create a source column that contains the value "test" (for when the test and train sets are merged)
testsourceVector <- vector(mode = "character", length = nrow(test_activityCode)) # must match number of rows in other dataframes
testsourceVector[1:nrow(test_activityCode)] <- "test" 
testsourceSet <- data.frame(testsourceVector)
# name columns with descriptive variable names
colnames(testsourceSet) <- "group"
colnames(test_subjectCode) <- "subject"
colnames(test_activityCode) <- "activity"
colnames(test_testset) <- featureHeader # names read from features.txt
# combine columns into a tidy dataframe
testTable <- cbind(testsourceSet, test_subjectCode, test_activityCode, test_testset)

# read training group files
train_subjectCode <- read.table("subject_train.txt")
train_activityCode <- read.table("y_train.txt") 
train_testSet <- read.table("x_train.txt") 
# create a source column that contains the value "train" (for when the test and train sets are merged)
trainsourceVector <- vector(mode = "character", length = nrow(train_activityCode)) # must match number of rows in other dataframes
trainsourceVector[1:nrow(train_activityCode)] <- "train"
trainsourceSet <- data.frame(trainsourceVector)
# name columns with descriptive variable names
colnames(trainsourceSet) <- "group"
colnames(train_subjectCode) <- "subject"
colnames(train_activityCode) <- "activity"
colnames(train_testSet) <- featureHeader
# combine columns into a tidy dataframe
trainTable <- cbind(trainsourceSet, train_subjectCode, train_activityCode, train_testSet)

# merge the training and test sets
allTable <- rbind(testTable, trainTable)

# --------------------------------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement

# create a feature table to extract only columns with mean and standard deviation for each measurement
# features table should have 66 feature variables
featureTable <- allTable[ , grepl("*std\\Q()\\E*", names(allTable)) | grepl("*mean\\Q()\\E*", names(allTable))]
# -------------------------------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
# 
# select first 3 columns containing subject info (group, subject, activity) and store them in infoTable
infoTable <- allTable[, 1:3]
# replace activity code values (1-6) with activity labels (walking, standing, sitting, etc) from activityLabels_df 
infoTable$activity<-activityLabels_df[match(infoTable$activity, activityLabels_df$V1),2]
# --------------------------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.
#   
# combine the info and activity tables to recreate allTable
allTable <- cbind(infoTable, featureTable)
# remove dashes and parens
names(allTable) <- gsub("-", "", names(allTable))
names(allTable) <- gsub("\\Q()\\E","", names(allTable))
# capitalize mean and std so colnames are more readable
names(allTable) <- gsub("mean", "Mean", names(allTable))
names(allTable) <- gsub("std", "Std", names(allTable))

# -------------------------------------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

aveTable <- allTable %>%
        group_by(subject, group, activity) %>%
        summarise_all(mean)

write.table(aveTable, "C:/Users/Eric Nelson/Documents/GithubFiles/aveTable.txt", row.name = FALSE, sep = ",")






