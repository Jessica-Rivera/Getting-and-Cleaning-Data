# Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate how to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.

This repository contains the following files:

- `README.md`, provides an overview of the data set and how it was created.
- `tidy_data.txt`, contains the output data set.
- `CodeBook.md`,describes the contents of the data set (data, variables, and transformations used to generate the data).
- `run_analysis.R`, the R script to generate the data set

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

The data set was obtained from the [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#).

## Data Set Information:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

## Attribute Information:

For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## The R script, 'run_analysis.R', does the following:

1. Download and unzip the dataset if it is not yet available in the working directory.
2. Read the data.
3. Merges the training and the test sets to create one data set.
4. Extracts only the measurements on the mean and standard deviation for each measurement.
5. Uses descriptive activity names to name the activities in the data set.
6. Appropriately labels the data set with descriptive variable names.
7. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
8. Write the data set to `tidy_data.txt` file.

The `tidy_data.txt` in this repository was created by running the `run_analysis.R` script using R version 3.5.1 (2018-07-02) and requires the `dplyr` package (version 0.7.8).
