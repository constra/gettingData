### loading packages
library(dplyr)

### download and load data
if(!file.exists("wearble.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url,destfile = "wearble.zip",method = "curl")
}

unzip("wearble.zip")

# read in feature table
feature <- read.table("UCI HAR Dataset/features.txt")
# read in the activity table
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activity) <- c("ID","NAME")

# read in the activity lable
label <- read.table("UCI HAR Dataset/activity_labels.txt")

# read in test sets
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("UCI HAR Dataset/test/Y_test.txt")
Subtest <- read.table("UCI HAR Dataset/test/subject_test.txt")

# combine subject name, activity and measurment
testset <- cbind(Subtest,Ytest,Xtest)

# change column name to comprehensive description
colnames(testset) <- append(c("Subject","Activity"),as.character(feature[[2]]))
valid_column_names <- make.names(names=names(testset), unique=TRUE, allow_ = TRUE)
names(testset) <- valid_column_names

# add group label
testset <- mutate(testset,group = "test")

# read in training set
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
Subtrain <- read.table("UCI HAR Dataset/train/subject_train.txt")

# combine subject name, activity and measurment
trainset <- cbind(Subtrain,Ytrain,Xtrain)

# change column name to comprehensive description
colnames(trainset) <- append(c("Subject","Activity"),as.character(feature[[2]]))
valid_column_names <- make.names(names=names(trainset), unique=TRUE, allow_ = TRUE)
names(trainset) <- valid_column_names

# add group label
trainset <- mutate(trainset,group = "train")

# combine two dataset head to tail
merged_data <- rbind(testset,trainset)

# convert group variable to factor
merged_data$group <- factor(merged_data$group)

# select mean and sd
selected_data <- select(merged_data, Subject, Activity, group, contains("mean"),contains("std"))

# change activity to descriptive names
desp_activity <- merge(selected_data,activity,x.by="Activity",y.by="NAME")

# create final dataset from new dataframe
final_data <- desp_activity %>%
        select(Subject,NAME,group:ID) %>%
        select(-ID) %>%
        rename(Activity=NAME) %>%
        group_by(Activity,Subject,group)

# summarize the mean of each variable
cols <- names(final_data)[-(1:3)]
# construct a function vactor
dots <- sapply(cols ,function(x) substitute(mean(x), list(x=as.name(x))))
# run the summarize function for each variable
final_mean <- do.call(summarise, c(list(.data=final_data), dots))

# write data
write.csv(final_mean,file="data.csv")
