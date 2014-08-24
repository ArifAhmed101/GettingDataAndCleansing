#set working directory 
#setwd('C:\Users\arif_mohamed\Desktop\Data Science Class\Data\UCI HAR Dataset\');

#1 Merge data
# Read data from files
vfeatures     = read.table('./UCI HAR Dataset/features.txt',header=FALSE); #loads features.txt
vactivityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE); #loads activity_labels.txt
vsubjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE); #loads subject_train.txt
vxTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE); #loads x_train.txt
vyTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE); #loads y_train.txt

# Assigin column names to the data loaded
colnames(vactivityType)  = c('activityId','activityType');
colnames(vsubjectTrain)  = "subjectId";
colnames(vxTrain)        = vfeatures[,2]; 
colnames(vyTrain)        = "activityId";

# Load test data
vsubjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); #loads subject_test.txt
vxTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE); #loads x_test.txt
vyTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); #loads y_test.txt

# Assign column names to test data
colnames(vsubjectTest) = "subjectId";
colnames(vxTest)       = vfeatures[,2]; 
colnames(vyTest)       = "activityId";

# Final training dataset
vtrainData = cbind(vyTrain,vsubjectTrain,vxTrain);
# Final test dataset merge - xTest, yTest and subjectTest data
vtestData = cbind(vyTest,vsubjectTest,vxTest);


# Combine training and test data
vfinalData = rbind(vtrainData,vtestData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(vfinalData); 

# 2. Extract only the measurements on the mean and standard deviation for 
#    each measurement. 

# Create a logicalVector that contains TRUE 
# values for the ID, mean() & stddev() columns and FALSE for others
lV_datset_val = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the lV_datset_val to keep only desired columns
vfinalData = vfinalData[lV_datset_val==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
vfinalData = merge(vfinalData,vactivityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(vfinalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassign new descriptive column names to the finalData set
colnames(vfinalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Final data without the activityType column
finalDataNoActivityType  = vfinalData[,names(vfinalData) != 'activityType'];

# Summarize finalDataNoActivityType table to include just the mean
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,vactivityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');