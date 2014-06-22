# 1. Merges the training and the test sets to create one data set.
x_train <- read.table("./data/train/X_train.txt")
x_test <- read.table("./data/test/X_test.txt")
x <- rbind(x_train, x_test)
y_train <- read.table("./data/train/y_train.txt")
y_test <- read.table("./data/test/y_test.txt")
y <- rbind(y_train, y_test)
subj_train <- read.table("./data/train/subject_train.txt")
subj_test <- read.table("./data/test/subject_test.txt")
subj <- rbind(subj_train, subj_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./data/features.txt")
indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])  # find only %mean()% and %std()% columns
x <- x[, indices]
names(x) <- features[indices, 2]
names(x) <- gsub("\\(|\\)", "", names(x))  # remove ()

# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] = tolower(as.character(activity[, 2]))
y[,1] <- activity[y[, 1], 2]
names(y) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names. 
names(subj) <- "subject"
result <- cbind(subj, y, x)
write.table(result, "result.txt")

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subj_len <- length(table(subj))
activity_len <- length(activity[, 1])
cols <- dim(result)[2]
result_avg <- result[1:(subj_len*activity_len), ]
row <- 1
for(i in 1:subj_len) {
  for(j in 1:activity_len) {
    result_avg[row, 1] <- unique(subj)[, 1][i]
    result_avg[row, 2] <- activity[j, 2]
    tmp <- result[result$subject == i & result$activity == activity[j, 2], ]
    result_avg[row, 3:cols] <- colMeans(tmp[, 3:numCols])
    row <- row + 1
  }
}
write.table(result_avg, "result_avg.txt")