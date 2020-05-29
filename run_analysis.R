library(dplyr)

### step 1. create the data set of training and test data along with its lables

#Reading training tables - xtrain / ytrain, subject train
xtrain <- read.table(file.path("UCI HAR Dataset","train","X_train.txt"),header = FALSE)
ytrain <- read.table(file.path("UCI HAR Dataset","train","y_train.txt"),header = FALSE)
subject_train <-read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"),header = FALSE)

#Reading the testing tables
xtest <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"),header = FALSE)
ytest <-read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"),header = FALSE)
subject_test <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"),header = FALSE)

#Read the features data
features <- read.table(file.path("UCI HAR Dataset","features.txt"),header = FALSE)

#Read activity labels data
activitylabels<- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"),header = FALSE)

### Step 2. tagging the data sets

#assigning names to the training dataset
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityid"
colnames(subject_train) <- "subjectid"

#assigning names to the test dataset
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityid"
colnames(subject_test) <- "subjectid"

#nameing activity labels
colnames(activitylabels) <- c("activityid","activitytype")

### step 3: merging the data

merge_train <- cbind(ytrain, subject_train, xtrain)
merge_test <- cbind(ytest, subject_test, xtest)
#main data table merging both tables
setall <- rbind(merge_train, merge_test)


## extraing mean & std .deviation

valid_column_names <- make.names(names=names(setall), unique=TRUE, allow_ = TRUE)
names(setall) <- valid_column_names

#creating the required dataset
dataset_with_mean_std <- setall %>% select(subjectid,activityid,contains("mean"),contains("std"))


###Use descriptive activity names to name the activities in the data set
dataset_withactivitynames <- dataset_with_mean_std%>% mutate(activityid=activitylabels[activityid,2])

#reaname to be more descriptive
#####Rename the columns of the large dataset using more descriptive activity names
names(dataset_withactivitynames)<-gsub("^t", "time", names(dataset_withactivitynames))
names(dataset_withactivitynames)<-gsub("^f", "frequency", names(dataset_withactivitynames))
names(dataset_withactivitynames)<-gsub("Acc", "Accelerometer", names(dataset_withactivitynames))
names(dataset_withactivitynames)<-gsub("Gyro", "Gyroscope", names(dataset_withactivitynames))
names(dataset_withactivitynames)<-gsub("Mag", "Magnitude", names(dataset_withactivitynames))
names(dataset_withactivitynames)<-gsub("BodyBody", "Body", names(dataset_withactivitynames))
names(dataset_withactivitynames)[2] <- "activity"
### final step create tidy data set
tidyset <- dataset_withactivitynames %>% group_by(subjectid,activity) %>% summarise_all(mean)
write.table(tidyset, "tidyset.txt", row.name=FALSE)