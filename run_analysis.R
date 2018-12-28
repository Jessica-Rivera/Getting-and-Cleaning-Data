
# The data linked to from the course website represent data collected 
# from the accelerometers from the Samsung Galaxy S smartphone.This script will
# perform the data analysis.

library(dplyr)

# Download data
# Download zip file containing data if it hasn't already been downloaded
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipF <- "UCI HAR Dataset.zip"

        if (!file.exists(zipF)) {
        download.file(fileUrl, zipF, mode = "wb")
}

# Unzip file
        path_rf <- "UCI HAR Dataset"
        if (!file.exists(path_rf)) {
        unzip(zipF)
}
        
# Read data
## Read training data
        Training_Subject <- read.table(file.path(path_rf, "train", "subject_train.txt"))
        Training_Values <- read.table(file.path(path_rf, "train", "X_train.txt"))
        Training_Activity <- read.table(file.path(path_rf, "train", "y_train.txt"))
        
## Read test data
        Test_Subject <- read.table(file.path(path_rf, "test", "subject_test.txt"))
        Test_Values <- read.table(file.path(path_rf, "test", "X_test.txt"))
        Test_Activity <- read.table(file.path(path_rf, "test", "y_test.txt"))
        
## Read features, don't convert text labels to factors
        features <- read.table(file.path(path_rf, "features.txt"), as.is = TRUE)
### note: feature names (in features[, 2]) are not unique
###       e.g. fBodyAcc-bandsEnergy()-1,8
        
## Read activity labels
        activities <- read.table(file.path(path_rf, "activity_labels.txt"))
        colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the training and the test sets to create one data set
        Human_Activity <- rbind(
                cbind(Training_Subject, Training_Values, Training_Activity),
                cbind(Test_Subject, Test_Values, Test_Activity)
        )
        
## Remove individual data tables
        rm(Training_Subject, Training_Values, Training_Activity, 
           Test_Subject, Test_Values, Test_Activity)
        
## Column names
        colnames(Human_Activity) <- c("subject", features[, 2], "activity")

# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
## Determine columns of data set to keep based on names
        columnsIncluded <- grepl("subject|activity|mean|std", colnames(Human_Activity))
        
## and store data in these columns
        Human_Activity <- Human_Activity[, columnsIncluded]
        
# Step 3 - Use descriptive activity names to name the activities in the data set
## Replace activity values with named factor levels
        Human_Activity$activity <- factor(Human_Activity$activity, 
                                         levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names
## Get column names
        Human_ActivityColumns <- colnames(Human_Activity)
        
## Remove special characters
        Human_ActivityColumns <- gsub("[\\(\\)-]", "", Human_ActivityColumns)
        
## Expand abbreviations and clean up names
        Human_ActivityColumns <- gsub("^f", "FreqDomain", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("^t", "TimeDomain", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("Acc", "Accelerometer", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("Gyro", "Gyroscope", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("Mag", "Magnitude", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("Freq", "Frequency", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("mean", "Mean", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("std", "StandardDeviation", Human_ActivityColumns)
        Human_ActivityColumns <- gsub("BodyBody", "Body", Human_ActivityColumns)
        
## Use new labels as column names
        colnames(Human_Activity) <- Human_ActivityColumns

# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
## Group by subject and activity and summarise using mean
        Human_Activity_Means <- Human_Activity %>% 
                group_by(subject, activity) %>%
                summarise_each(funs(mean))
        
## Output to file "tidy_data.txt"
        write.table(Human_Activity_Means, "tidy_data.txt", row.names = FALSE, 
                    quote = FALSE)