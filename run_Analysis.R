library(dplyr)
#1. Merging data

sub_test <- read.table("test/subject_test.txt")
y_test <- read.table("test/y_test.txt")
x_test <- read.table("test/X_test.txt")

sub_train <- read.table("train/subject_train.txt")
y_train <- read.table("train/y_train.txt")
x_train <- read.table("train/X_train.txt")

test <- cbind(sub_test, y_test, x_test)
train <- cbind(sub_train, y_train, x_train)
data <- rbind(test, train)

remove(sub_test, y_test, x_test, sub_train, y_train, x_train, test, train)

#2. Rename column name with "features.txt"
colname <- as.vector(read.table("features.txt")$V2)
colnames(data)[3:563] <- colname

#3. Extract measurements on the mean and SD
cond <- grepl("[Mm]ean", colnames(data)) | grepl("[Ss]td", colnames(data)) | grepl("V", colnames(data))
data_fil <- data[,cond]
colnames(data_fil)[1:2] <- c("subject", "activity")

#4. label the activity
act <- as.vector(read.table("activity_labels.txt", stringsAsFactors = FALSE))$V2
for (i in seq_along(data_fil[,2])) {
        data_fil[i,2] <- act[strtoi(data_fil[i,2])]
}

#5. Rename the colnames
colnames(data_fil) <- gsub("^t", "time.", colnames(data_fil))
colnames(data_fil) <- gsub("^f", "freq.", colnames(data_fil))
colnames(data_fil) <- gsub("-mean()-", ".mean.", colnames(data_fil), fixed = TRUE)
colnames(data_fil) <- gsub("-mean()", ".mean", colnames(data_fil), fixed = TRUE)
colnames(data_fil) <- gsub("-std()-", ".std.", colnames(data_fil), fixed = TRUE)
colnames(data_fil) <- gsub("-std()", ".std", colnames(data_fil), fixed = TRUE)
colnames(data_fil) <- gsub("-meanFreq()-", ".meanFreq.", colnames(data_fil), fixed = TRUE)
colnames(data_fil) <- gsub("-meanFreq()", ".meanFreq", colnames(data_fil), fixed = TRUE)

#6. Sum the average of each variable for each activity and each subject
data_sum <- summarise_all(group_by(data_fil, subject, activity), mean)