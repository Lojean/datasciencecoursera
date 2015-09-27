# Name: Gelonia Dent
# run_analysis R Script: 
# 1) merges the datasets together; 
# 2) extracts measurements on the mean and standard deviation for each measurement; 
# 3) renames any variables;
# and 4) outputs a second dataset with the average for each activity for each subject.

# File descriptions:  x data contains the measurement data, y data files contains the labels (numeric)
#  and subject contains the participant (subject) who performed the activity

# Part 1: 
# Read in feature data: this corresponds to the column information of the data sets

# Load the libraries needed to clean up the data file
library(plyr)
library(dplyr)
library(tidyr)

#Step 1 Merge the training and test sets:

features <- read.table("features.txt", stringsAsFactors=TRUE)
activityLabels <-read.table("activity_labels.txt")

# File paths

trainFilepath <- paste(getwd(),"/train",sep="")
trainFiles=list.files(trainFilepath,pattern="*_train.txt",full.names=TRUE)

testFilepath <- paste(getwd(),"/test",sep="")
testFiles=list.files(testFilepath,pattern="*_test.txt",full.names=TRUE)

# Read in data sets: training data is under ~./train directory and test data is under ~./test directory
# [1] subject, [2] x data, [3] y data

  subjectTest <-read.table(testFiles[1])
  xTest <- read.table(testFiles[2])
  yTest <- read.fwf(testFiles[3], 6)
  
  #Add measurement labels to data outcome file:
  
  names(xTest) <-features$V2
 
# Combining files based on dimension: subject and y* files will match the subject (participant) to the activity
# These are categorical so load as 'factors'
  
  subjectTestActivity <-data.frame(subjectTest, yTest)
  names(subjectTestActivity) <-c("V1", "V2")

# Match the subject with activity performed
  
  for(i in 1:nrow(subjectTestActivity)){
     for(j in 1:nrow(activityLabels)){
        subjectTestActivity$V2[subjectTestActivity$V2 == j]<-as.character(activityLabels$V2[j])
      }
  }
 
  names(subjectTestActivity) <-c("Subject", "Activity.Label")
  testData_raw<-cbind(subjectTestActivity,xTest)
  
  
  # Repeat above steps for same for training data sets
  
  subjectTrain <-read.table(trainFiles[1])
  xTrain <- read.table(trainFiles[2])
  yTrain <- read.table(trainFiles[3])
  
  #Add measurement labels to data outcome file:
  
  names(xTrain) <-features$V2
  
  subjectTrainActivity <-data.frame(subjectTrain, yTrain)
  names(subjectTrainActivity) <-c("V1", "V2")
  
  for(i in 1:nrow(subjectTrainActivity)){
    for(j in 1:nrow(activityLabels)){
      subjectTrainActivity$V2[subjectTrainActivity$V2 == j]<-as.character(activityLabels$V2[j])
    }
  }
  names(subjectTrainActivity) <-c("Subject", "Activity.Label")
  
  trainData_raw<-cbind(subjectTrainActivity,xTrain)
  
  #Combine the training and test data sets 
  
  tidyData_raw <-rbind(trainData_raw,testData_raw)
  
 # meandata <-select(tidyData_raw,contains("mean()", "std()"))
  
 # Gather the means and standard deviations columns
  mean_std_Columns <- grep("mean|std", features$V2, value = FALSE)
  mean_std_colNames <- grep("mean|std", features$V2, value = TRUE)
  
  # Select the means and standard deviations columns from the data frame
  mean_std_Data <- tidyData_raw[ ,mean_std_colNames]
  tidyData<-cbind(tidyData_raw[1:2],mean_std_Data)
  
  names(tidyData)=sub('()-', '_' ,names(tidyData))
  
  #Finally output the complete tidy data frame

  #Averages
  avg_tidyData <-tidyData %>% gather(Measures, Values, -Subject,-Activity.Label) %>% 
     group_by(Activity.Label,Measures) 
  
  #Output tidy data files: 
  
  file1 <-paste(getwd(),"tidyData.txt",sep="/")
  file2 <-paste(getwd(),"avg_tidyData.txt",sep="/")
  
  write.table(tidyData, file1, row.names=FALSE)
  write.table(avg_tidyData, file2, row.names=FALSE)
