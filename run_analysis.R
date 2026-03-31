library(dplyr)

# Step 1 - Merge the training and the test sets to create one data set.

features <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/features.txt")
feature_names <- c("subject.id", "activity", as.vector(features$V2))

test_data_subject <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
train_data_subject <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")

test_data_y <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt")
train_data_y <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt")

test_data_x <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt")
train_data_x <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt")

train <- cbind(train_data_subject, train_data_y, train_data_x)
test <- cbind(test_data_subject, test_data_y, test_data_x)

combined_data <- rbind(train, test)

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement.

names(combined_data) <- feature_names

extracted <- feature_names[c(1,2, grep("[Mm][e][a][n]|[s][t][d]", feature_names))]
get_mean_std <- combined_data[, extracted]

# Step 3 - Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt")
get_mean_std$activity <- factor(get_mean_std$activity, labels=activity_labels$V2)

# Step 4 - Appropriately label the data set with descriptive variable names. 

names(get_mean_std) <- gsub("Acc", "Acceleration", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-arCoeff()", "Auto_Regression_Coefficient", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-energy()", "Energy_Measure", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("Gyro", "Angular_Velocity", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-correlation()", "Correlation_Coefficient", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-bandsEnergy()", "Energy_of_Frequency_Interval", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("tBody", "Time_Domain_Body", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("tGravity", "Time_Domain_Gravity", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-XYZ", "Three_Axial_Signals", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-X", "X_Axis", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-Y", "Y_Axis", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-Z", "Z_Axis", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-kurtosis()", "Kurtosis", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("Mag", "Magnitude_Signals", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-mean()", "Mean", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-mad()", "Median_Absolute_Deviation", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-max()", "Maximum", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-min()", "Minimum", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-sma()", "Signal_Magnitude_Area", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-iqr()", "Interquartile_Range", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-entropy()", "Signal_Entropy", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-std()", "Standard_Deviation", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-maxlnds()", "Index_of_Frequency_Component_With_Largest_Magnitude", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-meanFreq()", "Weighted_Average_of_Frequency_Components_For_Mean Frequency", names(get_mean_std), fixed = TRUE)
names(get_mean_std) <- gsub("-skewness()", "Skewness", names(get_mean_std), fixed = TRUE)


# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- get_mean_std %>% 
  group_by(subject.id, activity) %>%
  summarise_each(funs(mean))
write.table(tidy_data, file = "C:/Users/yazubair/Documents/Coursera/R/Data Science  Foundations Using R/3 - Getting and Cleaning Data/UCI HAR Dataset/TidyData.txt", row.name = FALSE, col.names = TRUE)
