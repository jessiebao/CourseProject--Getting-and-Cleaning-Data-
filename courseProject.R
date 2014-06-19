# Class Project for Getting and Cleaning Data
#
# This script processes several files to create one data file as prescribed
# by the Class Project instructions

# Step 1
# In this section, we import activity labels, subject identifier file for the test data set,
# measurement data, and the variable (or feature) names file

aLabels <- read.table(file="activity_labels.txt",sep=" ")

# Since the activity label file does not contain headers, we give meaningful
# header information
colnames(aLabels) <- c("activityID","activityName")

subTest <- read.table(file="subject_test.txt",sep= " ")
colnames(subTest) <- "subID" # adding subID to have a meaningfule column name
xTest <- read.table(file="X_test.txt")
yTest <- read.table(file="y_test.txt")
colnames(yTest) <- "activityID" # adding meaningful column name
features <- read.table(file="features.txt")
featureNames <- features$V2 
colnames(xTest) <- featureNames # adding variable names to the measurement data


xTest$subID <- subTest$subID # adding subject ID information to the test dataset
xTest$activityID <- yTest$activityID # adding activity ID to the test dataset
xTest <- xTest[,c(562:563,1:561)] # reordering the columns so that subID and activityID appear to the left
testData <- merge(xTest,aLabels, by.x="activityID") # adding activity names to the test data set
testData <- testData[,c(564,1:563)] # reordering the added column to the left

# The regular expression used below essentially eliminates all columns that do not
# contain "mean" or "std" so we need to preserve the columns with labels
# so that we can concatenate label columns and the numerical columns
testPreColNames <- c("activityName","activityID","subID")
testDataIDCols <- testData[,testPreColNames]

# Regular expression is used to extract columns with "mean" and "std" words
# The "\\" helps to elimate other words that follow "mean" or "std"
# page 380 in "Mastering Regular Expressions" was the inspiration
meanstdColNamesTest <- grep("-mean\\(\\)|std", names(testData))
testDataNumeric <- testData[,meanstdColNamesTest]

# The statement below brings the label columns with numeric colmns together
testDataFinal <- cbind(testDataIDCols,testDataNumeric)

# The steps used to process test data is applied to the train data
subTrain <- read.table(file="subject_train.txt",sep = " ")
colnames(subTrain) <- "subID"
xTrain <- read.table(file="X_train.txt")
yTrain <- read.table(file="y_train.txt")
colnames(yTrain) <- "activityID"
colnames(xTrain) <- featureNames

xTrain$subID <- subTrain$subID
xTrain$activityID <- yTrain$activityID
xTrain <- xTrain[,c(562:563,1:561)]
trainData <- merge(xTrain,aLabels, by.x="activityID")
trainData <- trainData[,c(564,1:563)]
trainPreColNames <- c("activityName","activityID","subID")
trainDataIDCols <- trainData[,trainPreColNames]
meanstdColNamesTrain <- grep("-mean\\(\\)|std", names(trainData))
trainDataNumeric <- trainData[,meanstdColNamesTrain]
trainDataFinal <- cbind(trainDataIDCols,trainDataNumeric)


# Test data and train data are combined to create a single dataset
fullData <- rbind(testDataFinal,trainDataFinal)

# Creates a CSV file of the resulting dataset
write.csv(fullData, file="fullData.csv",row.names=FALSE)

# The step below computes the mean of the variables by Activity Name and Subject ID
# and creates another dataset 
sum_fullData <- aggregate(fullData[,4:69],by=fullData[c("activityName","subID")], FUN=mean)

# Creates a CSV file of the resulting dataset
write.csv(sum_fullData,file="sumFullData.csv",row.names=FALSE)
