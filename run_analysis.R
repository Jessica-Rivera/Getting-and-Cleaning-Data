#  OVERVIEW
##  The data linked to from the course website represent data collected 
##  from the accelerometers from the Samsung Galaxy S smartphone.This script will
##  perform the data analysis.
##  See README.md for details.


library(dplyr)

# STEP 0.1 - Download data
# Download zip file containing data if it hasn't already been downloaded
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipFile <- "UCI HAR Dataset.zip"

        if (!file.exists(zipFile)) {
        download.file(fileUrl, zipFile, mode = "wb")
}

# Unzip file
        path_rf <- "UCI HAR Dataset"
        if (!file.exists(path_rf)) {
        unzip(zipFile)
}
        
# STEP 0.2 - Read data
## Read training data
        trainingSubjects <- read.table(file.path(path_rf, "train", "subject_train.txt"))
        trainingValues <- read.table(file.path(path_rf, "train", "X_train.txt"))
        trainingActivity <- read.table(file.path(path_rf, "train", "y_train.txt"))
        
## Read test data
        testSubjects <- read.table(file.path(path_rf, "test", "subject_test.txt"))
        testValues <- read.table(file.path(path_rf, "test", "X_test.txt"))
        testActivity <- read.table(file.path(path_rf, "test", "y_test.txt"))
        
## Read features, don't convert text labels to factors
        features <- read.table(file.path(path_rf, "features.txt"), as.is = TRUE)
### note: feature names (in features[, 2]) are not unique
###       e.g. fBodyAcc-bandsEnergy()-1,8
        
## Read activity labels
        activities <- read.table(file.path(path_rf, "activity_labels.txt"))
        colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the training and the test sets to create one data set
## Concatenate individual data tables to make single data table
        humanActivity <- rbind(
                cbind(trainingSubjects, trainingValues, trainingActivity),
                cbind(testSubjects, testValues, testActivity)
        )
        
## Remove individual data tables to save space
        rm(trainingSubjects, trainingValues, trainingActivity, 
           testSubjects, testValues, testActivity)
        
## Assign column names
        colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
## Determine columns of data set to keep based on names
        columnsIncluded <- grepl("subject|activity|mean|std", colnames(humanActivity))
        
## and store data in these columns
        humanActivity <- humanActivity[, columnsIncluded]
        
# Step 3 - Use descriptive activity names to name the activities in the data set
## Replace activity values with named factor levels
        humanActivity$activity <- factor(humanActivity$activity, 
                                         levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names
## Get column names
        humanActivityColumns <- colnames(humanActivity)
        
## Remove special characters
        humanActivityColumns <- gsub("[\\(\\)-]", "", humanActivityColumns)
        
## Expand abbreviations and clean up names
        humanActivityColumns <- gsub("^f", "FreqDomain", humanActivityColumns)
        humanActivityColumns <- gsub("^t", "TimeDomain", humanActivityColumns)
        humanActivityColumns <- gsub("Acc", "Accelerometer", humanActivityColumns)
        humanActivityColumns <- gsub("Gyro", "Gyroscope", humanActivityColumns)
        humanActivityColumns <- gsub("Mag", "Magnitude", humanActivityColumns)
        humanActivityColumns <- gsub("Freq", "Frequency", humanActivityColumns)
        humanActivityColumns <- gsub("mean", "Mean", humanActivityColumns)
        humanActivityColumns <- gsub("std", "StandardDeviation", humanActivityColumns)
        humanActivityColumns <- gsub("BodyBody", "Body", humanActivityColumns)
        
## Use new labels as column names
        colnames(humanActivity) <- humanActivityColumns

# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
## Group by subject and activity and summarise using mean
        humanActivityMeans <- humanActivity %>% 
                group_by(subject, activity) %>%
                summarise_each(funs(mean))
        
## Output to file "tidy_data.txt"
        write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
                    quote = FALSE)