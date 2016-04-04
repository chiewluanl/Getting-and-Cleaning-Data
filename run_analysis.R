run_analysis <- function() {
  setwd("./RProject/GettingAndCleaningData/Week4")
  library(data.table)
  
  if(!file.exists("./data")){
  	dir.create("./data")
  
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
    unzip(zipfile="./data/Dataset.zip",exdir="./data")
    path_rf <- file.path("./data" , "UCI HAR Dataset")
    files<-list.files(path_rf, recursive=TRUE)
    files
  }
  
  featureNames <- read.table(file.path(path_rf,"features.txt"),header=FALSE)
  activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"), header = FALSE)
  subjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"), header = FALSE)
  activityTrain <- read.table(file.path(path_rf, "train", "y_train.txt"), header = FALSE)
  featuresTrain <- read.table(file.path(path_rf, "train", "X_train.txt"), header = FALSE)
  subjectTest <- read.table(file.path(path_rf, "test", "subject_test.txt"), header = FALSE)
  activityTest <- read.table(file.path(path_rf, "test", "y_test.txt"), header = FALSE)
  featuresTest <- read.table(file.path(path_rf, "test", "X_test.txt"), header = FALSE)
  
  
  
  ##	1.	Merges the training and the test sets to create one data set.
  subject <- rbind(subjectTrain, subjectTest)
  activity <- rbind(activityTrain, activityTest)
  features <- rbind(featuresTrain, featuresTest)
  colnames(features) <- t(featureNames[2])
  
  colnames(activity) <- "Activity"
  colnames(subject) <- "Subject"
  completeData <- cbind(features,activity,subject)
  
  ## 	2.	Extracts only the measurements on the mean and standard deviation for each measurement.
  columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
  
  requiredColumns <- c(columnsWithMeanSTD, 562, 563)
  dim(completeData)
  
  extractedData <- completeData[,requiredColumns]
  dim(extractedData)
  
  ## 	3.	Uses descriptive activity names to name the activities in the data set
  extractedData$Activity <- as.character(extractedData$Activity)
  for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
  }
  
  extractedData$Activity <- as.factor(extractedData$Activity)
  
  ## 4. Appropriately labels the data set with descriptive variable names.
  names(extractedData)
  names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
  names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
  names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
  names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
  names(extractedData)<-gsub("^t", "Time", names(extractedData))
  names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
  names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
  names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("angle", "Angle", names(extractedData))
  names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
  
  names(extractedData)
  
  
  ## 	5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  extractedData$Subject <- as.factor(extractedData$Subject)
  extractedData <- data.table(extractedData)
  
  tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
  tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
  write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
}