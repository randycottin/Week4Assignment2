require(plyr)
# ---------------------------------------------------------------
# 1. Merge the training and the test sets to create one data set.

# assign subject ID for test and training data sets
subjectTest <- read.table('test/subject_test.txt', col.names=c('Subject'))
subjectTrain <- read.table('train/subject_train.txt', col.names=c('Subject'))

# combine subject records from test and training data sets
subjects <- rbind(subjectTest,subjectTrain)

# read in features measurements from test and training data sets
featuresTest <- read.table('test/X_test.txt')
featuresTrain <- read.table('train/X_train.txt')

# combine test and training sets for features
features <- rbind(featuresTest,featuresTrain)

# copy feature labels
feature_labels <- read.table('features.txt', col.names=c('index', 'labels'))

# make feature_labels$labels the column name of features
names(feature_labels)
labels <- feature_labels$labels
colnames(features) <- labels

# -----------------------------------------------------------------------------
# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# subset the features table by selecting variables with labels that contain mean and standard deviation in their names
feature_selected <- as.character(feature_labels$labels[grepl('mean()|std()', feature_labels$labels)])

# do not include meanFreq measurements
feature_selected <- as.character(feature_selected[!grepl('meanFreq()', feature_selected)]
features_means_stds <- features[,c(feature_selected)]

# read in activities from test and training sets
activitiesTest <- read.table('test/y_test.txt')
colnames(activitiesTest) <- 'Activity'

activitiesTrain <- read.table('train/y_train.txt')
colnames(activitiesTrain) <- 'Activity'

# combine test and training sets for activities
activities <- rbind(activitiesTest, activitiesTrain)

# ------------------------------------------------------------------------
# 3. Use descriptive activity names to name the activities in the data set

# descriptive names according to activity_labels.txt
activities$Activity[activities$Activity=='1'] <- 'WALKING'
activities$Activity[activities$Activity=='2'] <- 'WALKING_UPSTAIRS'
activities$Activity[activities$Activity=='3'] <- 'WALKING_DOWNSTAIRS'
activities$Activity[activities$Activity=='4'] <- 'SITTING'
activities$Activity[activities$Activity=='5'] <- 'STANDING'
activities$Activity[activities$Activity=='6'] <- 'LAYING'

# ------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive names.

# make variable names more descriptive by using full words and removing parentheses
names(features_means_stds) <- gsub('Acc','Acceleration', names(features_means_stds))
names(features_means_stds) <- gsub('Mag','Magnitude', names(features_means_stds))
names(features_means_stds) <- gsub('Freq','Frequency', names(features_means_stds))
names(features_means_stds) <- gsub('-mean','Mean', names(features_means_stds))
names(features_means_stds) <- gsub('-std','StandardDeviation', names(features_means_stds))
names(features_means_stds) <- gsub('\\(|\\)','', names(features_means_stds), perl=TRUE)


# --------------------------------------------------------------------------------------
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

# combine all data sets
all_data <- cbind(subjects,activities,features_means_stds)

# get the average of each variable for each activity and subject (uses plyr)
average_variables <- ddply(all_data, c("Subject","Activity"), numcolwise(mean))

# save final dataset as average_variables.txt
write.table(average_variables, file = "average_variables.txt")
