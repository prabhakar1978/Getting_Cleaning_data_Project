# Course Project for 'Getting and Cleaing Data' class by Prabhakar Thanikasalam
# Assumes that all data is placed in a folder titled 'Course Project'

#Required packages
library(dplyr)
library(tidyr)
library(reshape2)


#Read in labels
features = read.table("UCI HAR Dataset/features.txt",sep=" ")            #Features: these are column labels to be used
names(features) = c("feature#", "feature")

activity.labels = read.table("UCI HAR Dataset/activity_labels.txt",sep=" ")  #Activties: there are activity # to description matching, to be used as acticity identifier in each row of traiing and test data
names(activity.labels) = c("activity#", "activity")

#Read in test, training datasets
test = read.table("UCI HAR Dataset/test/X_test.txt",sep="",col.names=features$feature)

subject_test = read.table("UCI HAR Dataset/test/subject_test.txt",sep="",col.names="subjectID")

test.labels.number =read.table("UCI HAR Dataset/test/Y_test.txt",sep=" ")
names(test.labels.number) = "activity#"

test.labels.number= left_join(test.labels.number, activity.labels, by="activity#") 
test.labels = select(test.labels.number, activity) #get rid of activity label number since we have activity name (description)

test = cbind(test.labels, test) #Step 2 in Course Project requirements
test = cbind(subject_test, test) # the script so far creates 

#training data - following same logic as test data set
train = read.table("UCI HAR Dataset/train/X_train.txt",sep="",col.names=features$feature)
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt",sep="",col.names="subjectID")
train.labels.number =read.table("UCI HAR Dataset/train/Y_train.txt",sep=" ")
names(train.labels.number) = "activity#"

train.labels.number= left_join(train.labels.number, activity.labels, by="activity#") 
train.labels = select(train.labels.number, activity) #get rid of activity label number since we have activity name (description)

train = cbind(train.labels, train) #Step 3 in Course Project requirements
train = cbind(subject_train, train)

#create merged 'full' data set
full_data = rbind(test, train) #Step 1 in Course Project requirements

#Extract only measurements of mean, std deviation for each measuerment
strings = c("mean", "std", "subjectID", "activity")
MatchExpression = paste(strings, collapse="|") 

full_data = select(full_data, matches(MatchExpression)) 

full_data = select(full_data, 
                -contains("tBodyAccMean"),
                -contains("meanFreq"),
                -contains("gravityMean"),
                -contains("tBodyAccJerkMean"),
                -contains("tBodyGyroMean"),
                -contains("tBodyGyroJerkMean")) #removing extended mataches to "mean" in variables - Step 2 in Course Project requirements


full_data = melt(full_data, id.vars = c("subjectID", "activity"), 
                 
                 variable.name ="Measurement", 
                 value.name ="Value"
                 
                 ) #tidy data

write.table(full_data,"tidy_data.txt",sep="\t",row.name=FALSE) #tidy data for bigger set

full_data= group_by(full_data, subjectID, activity, Measurement) #grouping by subject, measurement, activity

tidy_data_avg = summarise(full_data, 
                          
                          Avg_Value = mean(Value, na.rm=T)
                          ) #Step 5 in Course Project requirement

write.table(tidy_data_avg,"tidy_data_avg.txt",sep="\t",row.name=FALSE) #tidy data set of averages

#this completes the requirments in the Course Project

