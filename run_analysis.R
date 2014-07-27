
setwd("E:/Kaeggle/Coursera/Cleaning")
# 1. Merges the training and the test sets to create one data set.

#Read data
features<-read.table('./UCI HAR Dataset/features.txt',header=FALSE);
activityLabels<-read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);
subjectTrain<-read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);
xTrain<-read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE); 
yTrain<-read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);
subjectTest<-read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE);
xTest<-read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE); 
yTest<-read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE);

#Add collumn names
colnames(activityLabels)<-c('activityId','activityType');
colnames(subjectTrain)<-"subjectId";
colnames(xTrain)<-features[,2]; 
colnames(yTrain)<-"activityId";
colnames(subjectTest)<-"subjectId";
colnames(xTest)<-features[,2]; 
colnames(yTest)<-"activityId"

#Merge tables
tidyData<-rbind(cbind(yTrain,subjectTrain,xTrain),cbind(yTest,subjectTest,xTest))


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#Get good collumns and select only them
colNames<-colnames(tidyData)
goodCollumns <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
tidyData <- tidyData[,goodCollumns==TRUE];

# 3. Uses descriptive activity names to name the activities in the data set

tidyData = merge(tidyData,activityLabels,by='activityId',all.x=TRUE);

# 4. Appropriately label the data set with descriptive activity names. 

colNames<-colnames(tidyData)
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(tidyData) <- colNames;

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

tidyDataMean    <- aggregate(tidyData[,names(tidyData) != c('activityId','subjectId')],by=list(activityId=tidyData$activityId,subjectId=tidyData$subjectId),mean);

tidyDataMean[,"activityType"] <- NULL

write.table(tidyDataMean, "tidyDataMean.txt", sep="\t")