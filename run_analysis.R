#Coursera - Getting and Cleaning Data Course Project

##0 : Download and Unzip into "data" directory
if (!file.exists("data")) {
    dir.create("data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "data/CleaningData.zip")
unzip("data/CleaningData.zip",exdir="data")


##1 : Create the whole data set "data_tot", containing train and test informations

library(data.table)
###Read and merge all parts of train data
train_base <- fread("data/UCI HAR Dataset/train/X_train.txt")
train_activity <- fread("data/UCI HAR Dataset/train/y_train.txt")
train_subject <- fread("data/UCI HAR Dataset/train/subject_train.txt")
train <- cbind(train_base,train_activity,train_subject)

###Read and merge all parts of test data
test_base <- fread("data/UCI HAR Dataset/test/X_test.txt")
test_activity <- fread("data/UCI HAR Dataset/test/y_test.txt")
test_subject <- fread("data/UCI HAR Dataset/test/subject_test.txt")
test <- cbind(test_base,test_activity,test_subject)

###Merge train and test
data_tot <- rbind(train,test)


##2 : Extract mean and std measurement features from data_tot

###Attach Features Names, and Name data_tot
names_features <- fread("data/UCI HAR Dataset/features.txt")$V2
names(data_tot) <- c(names_features,"activity","subject")
features_select <- grep("mean|std|activity|subject",names(data_tot))
###Keep mean and std measurements
data_mean_std <- data_tot[,features_select, with=FALSE]


##3 : Merge Activity Labels

names_activity <- fread("data/UCI HAR Dataset/activity_labels.txt")
names(names_activity) <- c("activity","activity_label")
setkey(data_mean_std,"activity")
setkey(names_activity,"activity")
data_mean_std <- merge(data_mean_std,names_activity, all.x=TRUE)


##4 : Create understandable names for measurements features

explicite_names <- names(data_mean_std)
explicite_names<- gsub("Acc","Acceleration",explicite_names)
explicite_names<- gsub("Gyro","AngularVelocity",explicite_names)
explicite_names<- gsub("Jerk","Derived",explicite_names)
explicite_names<- gsub("Mag","Magnitude",explicite_names)
explicite_names<- gsub("tB","TimedB",explicite_names)
explicite_names<- gsub("tG","TimedG",explicite_names)
explicite_names<- gsub("fB","FourierTransformedB",explicite_names)
explicite_names<- gsub("fG","FourierTransformedG",explicite_names)
explicite_names<- gsub("\\()-","-along",explicite_names)
explicite_names<- gsub("\\()","",explicite_names)
names(data_mean_std)<- explicite_names


##5 : Aggregate data by activity x subject and calculate average of all measurement features  
measurement_features<- setdiff(names(data_mean_std),c("activity","activity_label","subject"))
data_aggregate <- data_mean_std[,lapply(.SD,function(x) mean(x,na.rm = T)),by=c("activity_label","subject"),.SDcols= measurement_features ]