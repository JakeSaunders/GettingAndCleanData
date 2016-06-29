# Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project.

You will be required to submit:

- 1) a tidy data set as described below,
- 2) a link to a Github repository with yourscript for performing the analysis, and
- 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md.
- 4) You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

A full description is available at the site where the data was obtained:
```
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
```

Here are the data for the project:
```
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
```

You should create one R script called run_analysis.R that does the following:

- 1) Merges the training and the test sets to create one data set.
- 2) Extracts only the measurements on the mean and standard deviation for each measurement.
- 3) Uses descriptive activity names to name the activities in the data set
- 4) Appropriately labels the data set with descriptive variable names.
- 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Start My code

### install packages, download zip file and uzip
    install.packages("dplyr")
    install.packages("reshape2")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="data.zip")
    unzip("data.zip")
    library("dplyr")
    library("reshape2")
    
### read required data into data frames
    features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
    test.subject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
    test.y <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
    test.x <- read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE)
    train.subject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
    train.y <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
    train.x <- read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)

### combine test and train data into columns
    subject <- bind_rows(train.subject, test.subject)
    y <- bind_rows(train.y, test.y)
    x <- bind_rows(train.x, test.x)

### build column names list, make boolean list to determine which columns are kept
    sub <- data.frame("V1" = -1, "V2" = "Subject")
    act <- data.frame("V1" = 0, "V2" = "Activity")
    name <- bind_rows(sub, act)
    name <- bind_rows(name, features)
    names <- as.list(name$V2)
    keep.these <- grepl("Subject|Activity|.*mean.*|.*std.*", names)

### convert number code to activity names
    y$V1[y$V1 == 1] <- "Walking"
    y$V1[y$V1 == 2] <- "WalkingUpstairs"
    y$V1[y$V1 == 3] <- "WalkingDownstairs"
    y$V1[y$V1 == 4] <- "Sitting"
    y$V1[y$V1 == 5] <- "Standing"
    y$V1[y$V1 == 6] <- "Laying"

### build combined data frame and name columns
    raw.data <- bind_cols(subject, y, x)
    colnames(raw.data) <- names

### select columns for subject, activity, any for mean or std
    data <- raw.data[ , keep.these]


### produce averages for each Subject - Activity pair
    data$Activity <- as.factor(data$Activity)
    data$Subject <- as.factor(data$Subject)
    data.all <- data
    data <- melt(data, id = c("Subject", "Activity"))
    data <- dcast(data, Subject + Activity ~ variable, mean)

### clean up symbols in column names
    names <- names(data)
    names <- gsub('[-()]', '', names)
    names <- gsub("mean", "Mean", names)
    names <- gsub('std', 'Std', names)
    names <- gsub('Std', 'StandardDeviation', names)
    names <- gsub("^t", 'Time', names)
    names <- gsub("^f", "Frequency", names)
    names <- gsub("Acc", "Accelerometer", names)
    names <- gsub("Gyro", "Gyroscope", names)
    names <- gsub("Mag", "Magnitude", names)
    names <- gsub("BodyBody", "Body", names)
    names(data) <- names
    names(data.all) <- names

### Save file as comma seperated value files, remove unneeded objects
    write.table(data, file = "datameans.txt")
    write.table(data.all, file = "dataall.txt")
    rm(features,subject,x,y,keep.these,train.subject, test.subject, train.y, test.y, train.x, test.x,sub, act, name, raw.data)
    
# Variable Names and information


 - **Subject**: Numeric ID the subject who performed the activity for each window sample
 - **Activity**: Laying, Sitting, Standing, Walking, WalkingDownstairs, WalkingUpstairs

Elements of other vaiable names discriptors:

 - **Time** or **Frequency** : Type of measurement
 - **Body** or **Gravity** : Force causing signal
 - **Accelerometer** or **Gyroscope** : Instrument makeing measurement 
 - **Jerk** : Jerk signal
 - **Magnitude** : Signals calculated using the Euclidean norm
 - **Mean** or **StandardDeviation** : The value reported is the average or standard deviation
 - **X** , **Y** or **Z** : Direction of force being measured

List of other variable names:

 - TimeBodyAccelerometerMeanX
 - TimeBodyAccelerometerMeanY
 - TimeBodyAccelerometerMeanZ
 - TimeBodyAccelerometerStandardDeviationX
 - TimeBodyAccelerometerStandardDeviationY
 - TimeBodyAccelerometerStandardDeviationZ
 - TimeGravityAccelerometerMeanX
 - TimeGravityAccelerometerMeanY
 - TimeGravityAccelerometerMeanZ
 - TimeGravityAccelerometerStandardDeviationX
 - TimeGravityAccelerometerStandardDeviationY
 - TimeGravityAccelerometerStandardDeviationZ
 - TimeBodyAccelerometerJerkMeanX
 - TimeBodyAccelerometerJerkMeanY
 - TimeBodyAccelerometerJerkMeanZ
 - TimeBodyAccelerometerJerkStandardDeviationX
 - TimeBodyAccelerometerJerkStandardDeviationY
 - TimeBodyAccelerometerJerkStandardDeviationZ
 - TimeBodyGyroscopeMeanX
 - TimeBodyGyroscopeMeanY
 - TimeBodyGyroscopeMeanZ
 - TimeBodyGyroscopeStandardDeviationX
 - TimeBodyGyroscopeStandardDeviationY
 - TimeBodyGyroscopeStandardDeviationZ
 - TimeBodyGyroscopeJerkMeanX
 - TimeBodyGyroscopeJerkMeanY
 - TimeBodyGyroscopeJerkMeanZ
 - TimeBodyGyroscopeJerkStandardDeviationX
 - TimeBodyGyroscopeJerkStandardDeviationY
 - TimeBodyGyroscopeJerkStandardDeviationZ
 - TimeBodyAccelerometerMagnitudeMean
 - TimeBodyAccelerometerMagnitudeStandardDeviation
 - TimeGravityAccelerometerMagnitudeMean
 - TimeGravityAccelerometerMagnitudeStandardDeviation
 - TimeBodyAccelerometerJerkMagnitudeMean
 - TimeBodyAccelerometerJerkMagnitudeStandardDeviation
 - TimeBodyGyroscopeMagnitudeMean
 - TimeBodyGyroscopeMagnitudeStandardDeviation
 - TimeBodyGyroscopeJerkMagnitudeMean
 - TimeBodyGyroscopeJerkMagnitudeStandardDeviation
 - FrequencyBodyAccelerometerMeanX
 - FrequencyBodyAccelerometerMeanY
 - FrequencyBodyAccelerometerMeanZ
 - FrequencyBodyAccelerometerStandardDeviationX
 - FrequencyBodyAccelerometerStandardDeviationY
 - FrequencyBodyAccelerometerStandardDeviationZ
 - FrequencyBodyAccelerometerMeanFreqX
 - FrequencyBodyAccelerometerMeanFreqY
 - FrequencyBodyAccelerometerMeanFreqZ
 - FrequencyBodyAccelerometerJerkMeanX
 - FrequencyBodyAccelerometerJerkMeanY
 - FrequencyBodyAccelerometerJerkMeanZ
 - FrequencyBodyAccelerometerJerkStandardDeviationX
 - FrequencyBodyAccelerometerJerkStandardDeviationY
 - FrequencyBodyAccelerometerJerkStandardDeviationZ
 - FrequencyBodyAccelerometerJerkMeanFreqX
 - FrequencyBodyAccelerometerJerkMeanFreqY
 - FrequencyBodyAccelerometerJerkMeanFreqZ
 - FrequencyBodyGyroscopeMeanX
 - FrequencyBodyGyroscopeMeanY
 - FrequencyBodyGyroscopeMeanZ
 - FrequencyBodyGyroscopeStandardDeviationX
 - FrequencyBodyGyroscopeStandardDeviationY
 - FrequencyBodyGyroscopeStandardDeviationZ
 - FrequencyBodyGyroscopeMeanFreqX
 - FrequencyBodyGyroscopeMeanFreqY
 - FrequencyBodyGyroscopeMeanFreqZ
 - FrequencyBodyAccelerometerMagnitudeMean
 - FrequencyBodyAccelerometerMagnitudeStandardDeviation
 - FrequencyBodyAccelerometerMagnitudeMeanFreq
 - FrequencyBodyAccelerometerJerkMagnitudeMean
 - FrequencyBodyAccelerometerJerkMagnitudeStandardDeviation
 - FrequencyBodyAccelerometerJerkMagnitudeMeanFreq
 - FrequencyBodyGyroscopeMagnitudeMean
 - FrequencyBodyGyroscopeMagnitudeStandardDeviation
 - FrequencyBodyGyroscopeMagnitudeMeanFreq
 - FrequencyBodyGyroscopeJerkMagnitudeMean
 - FrequencyBodyGyroscopeJerkMagnitudeStandardDeviation
 - FrequencyBodyGyroscopeJerkMagnitudeMeanFreq
