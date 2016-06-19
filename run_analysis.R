#Author: Siamak Talebi
library(dplyr)

# Step 1 - Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
#reading the variable names from features.txt
features <- fread("features.txt")
features <- features$V2

#reading all required data with names assigned
x_test <- fread("X_test.txt",col.names = features)
y_test <- fread("y_test.txt",col.names = activity)
subject_test <- fread("subject_test.txt",col.names = subject)
x_train <- fread("X_train.txt",col.names = features)
y_train <- fread("y_train.txt",col.names = activity)
subject_test <- fread("subject_test.txt",col.names = subject)

#mergin data
test <- cbind(subject_test,y_test,x_test)
train <- cbind(subject_train,y_train,x_train)
data <- rbind(test,train)

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------------------------

a <- grepl("mean|std",names(data))
b<-names(data)[a]
x <- subset(data,select = b)


# Step 3 - Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------

#renaming activity

data$activity <- gsub(1,"walking",data$activity)
data$activity <- gsub(2,"walking_upstairs",data$activity)
data$activity <- gsub(3,"walking_downstairs",data$activity)
data$activity <- gsub(4,"sitting",data$activity)
data$activity <- gsub(5,"standing",data$activity)
data$activity <- gsub(6,"laying",data$activity)

# step 4 - Appropriately labels the data set with descriptive variable names.
# ------------------------------------------------------------------------------
names <- names(data)
names <- gsub("\\(\\)", "", names)
names <- gsub("Acc", "-acceleration", names)
names <- gsub("Mag", "-Magnitude", names)
names <- gsub("^t(.*)$", "\\1-time", names)
names <- gsub("^f(.*)$", "\\1-frequency", names)
names <- gsub("(Jerk|Gyro)", "-\\1", names)
names <- gsub("BodyBody", "Body", names)
names <- tolower(names)

#reassign names
names(data) <- names

# step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# ------------------------------------------------------------------------------
n <- data %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))


write.table(n, file="tidy_data.txt", row.name=FALSE)
