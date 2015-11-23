# This script takes movement data and produces a tidier, 
# well labeled and appropriately summarized pair of data sets
# 
# It does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity 
#    and each subject.
# 

require(dplyr)
require(gsubfn)

#################################################################
#################################################################
## A. Merge the training and the test sets to create one data set.
#################################################################
#################################################################

# 1. Load the training and test subject identifiers
strain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/subject_train.txt")
stest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/subject_test.txt")

# Check their dimensions if desired
# length(strain)
# length(stest)

# Combine them into one data frame
s <- rbind.data.frame(strain, stest)

# Remove the original two separate training/test data frames to free up memory
rm(strain)
rm(stest)

# 2. Load only the summarized data collected from the device (since we only need means and SDs)
xtrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/X_test.txt")

# Check their dimensions if desired
# dim(xtrain)
# dim(xtest)

# Combine them into one data frame
x <- rbind.data.frame(xtrain, xtest)

# Remove the original two separate training/test data frames to free up memory
rm(xtrain)
rm(xtest)


# 3. Load the codes for the activity labels for each row of data currently in our "x" data frame
ytrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/y_train.txt")
ytest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/y_test.txt")

# Check their dimensions if desired
# dim(ytest)
# dim(ytrain)

# Combine them into one data frame
y <- rbind.data.frame(ytrain, ytest)

# Check dimensions if desired
# dim(y)

# Remove the original two separate training/test data frames to free up memory
rm(ytrain)
rm(ytest)

## Step A complete ##

########################################################################
########################################################################
## B. Appropriately label the data set with descriptive variable names
########################################################################
########################################################################

# 1. Load the features and activity labels
ftrs <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/features.txt")
actlabs <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/activity_labels.txt")


# 2. Put our data into a new data frame so that we can refer back to the original 
# data if any label/feature misassignment takes place in subsequent steps
data <- x

# 3. Assign features as column headers to the data acquired from the device ("x")
colnames(data) <- ftrs$V2

# Check this all makes sense, if desired
# str(data)
# str(tbl_df(data))

## Step B complete ##

############################################################################
############################################################################
## C. Use descriptive activity names to name the activities in the data set
############################################################################
############################################################################

# 1. Create an empty vector to take the character label  
# names corresponding to the activity codes in "y".
labels <- vector(mode="character", length=dim(y)[1])

# 2. Use a loop within a loop for the following purpose:
#      Loop 1: go through each activity code, starting with 1 and 
# select each of the character ativity labels in "actlabs", one by one,
#      Loop 2: paste the character label of the current activity 
# into those rows of our empty vector "labels" that correspond to
# the rows of the activity code vector "y" that contain the current
# activity code.
#      This allows us to use cbind  to add the labels vector as an additional
# column of the "data" dataframe. 
#

for (i in (1:length(actlabs$V1))) {
# #      optionally print the current activity label name to the console
#        print(paste(actlabs$V2[i]))
#        flush.console()
        for (j in dim(y)[1]) {
                a <- which(y$V1 == i)
                labels[a] <- paste(actlabs$V2[i])
        }
}

# 3. Add the activity labels and then the subject IDs, 
# each time to the left edge of the data frame
data <- cbind(labels, data)
data <- cbind(s$V1, data)

# 4. Since our first two columns now have no names, let's take all the column
# names, tidy them up and then reassign the tidy version to the dataframe
cn <- colnames(data)
cn[1] <- "subject"
cn[2] <- "actlab"
names(data) <- cn

## Step C complete ##

############################################################
############################################################
# D. Get the names of the mean and std variables to be kept
############################################################
############################################################

# 1. Use grep to find all variable names (colnames) that have 
# the word "mean" before an open parenthesis. This exlcudes 
# variables that have been calculated on the basis of a mean, 
# but are not in and of themselves mean values. 
mncsp <- cn[grep("[Mm]ean[(]", cn)]

# 2. Find the corresponding standard deviations 
stdncs <- cn[grep("std", cn)]

# Check we have a corresponding set of measurements, if desired
# length(mncsp) == length(stdncs)

# 3. Combine the mean and SD variable names for ease of reference
mstdm <- cbind(mncsp, stdncs)

#4. Assign appropriate colnames for ease of reference
colnames(mstdm) <- c("means", "stdvs")

# 5. Create a vector to identify only those columns of our merged dataframe whose colnames 
# correspond to those we have just selected and saved.
keep <- which(colnames(data) %in% mstdm)

# 6. Be sure to add back in the all-important subject ID and activity label columns
keep <- c(1,2,keep)

# 7. Create our completed, merged data set of means and SDs, by selecting only
# those columns of it that we have decided to keep.
datar <- data[,keep]

# 8a. Make all colnames and activity labels lowercase for ease of reference
# Better to do it at this late stage rather than run the risk of creating confusingly
# close variable names in the larger original data set.
names(datar) <- tolower(names(datar))

# 8b. Additionally, remove underscores from activity labels so for ease of reference
# N.B. Although three of the factors contain the word "walking", it did not seem
# appropriate to combine these as three levels of a single variable because
# they are categorically different activities. In some languages, such as Spanish, for example,
# these activities are not referred to as "walking" rather as "going up"/"ascender"
# or "going down"/"bajar". Additionally, they are listed as separate factors in the 
# original data set, which suggests that they are intended to be analyzed as 
# independent factors rather than as levels of a single factor.
datar$actlab <- as.factor(tolower(gsub("_", "", datar$actlab)))


# 9. Sort the data by subject and activity label with arrange
datars <- arrange(datar, subject, actlab)


## Step D complete ##


###############################################################
###############################################################
## E. Create a new data set of averages of the above data set 
###############################################################
###############################################################

# From the data set in the previous step, use summarise_each and chaining 
# to create a second, independent tidy data set with the average of each 
# variable for each activity and each subject.

datars %>% group_by(subject, actlab) %>% summarise_each(funs(mean)) -> datarsAVG

write.table(datarsAVG, "table5.txt", row.names = FALSE)











# ##############################################################
# ## These intertial movement data were ultimately not attached
# ## because the X data already had everything necessary.
# ##############################################################
# #First the training data
# #Body acceleration
# bodaccxtrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
# bodaccytrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
# bodaccztrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")
# 
# #Total acceleration
# totaccxtrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
# totaccytrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
# totaccztrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")
# 
# #Body gyroscopic movement
# bodgyrxtrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
# bodgyrytrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
# bodgyrztrain <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
# 
# 
# #Then the test data
# #Body acceleration
# bodaccxtest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
# bodaccytest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
# bodaccztest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")
# 
# #Total acceleration
# totaccxtest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
# totaccytest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
# totaccztest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")
# 
# #Body gyroscopic movement
# bodgyrxtest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
# bodgyrytest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
# bodgyrztest <- read.table("C:/Users/Stephen/Documents/Coursera/Data_Science_Track/3_Getting_&_Cleaning_Data/201511/Assignment/UCIHAR/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")

