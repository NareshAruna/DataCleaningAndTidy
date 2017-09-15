dataTidyFunction <- function() {

  #loading the dplyr package
  library(dplyr)
  library(reshape2)
  
  activity_list <- read.table("./activity_labels.txt")
  features_list <- read.table("./features.txt")
  
  #Loading data related to Training set
  train_x_list <- read.table("./train/X_train.txt")
  train_y_list <- read.table("./train/y_train.txt")
  train_subject_list <- read.table("./train/subject_train.txt")
  train_activity_set <- data.frame(Activity = activity_list$V2[train_y_list$V1])
  
  #Loading data related to Test Set
  test_x_list <- read.table("./test/X_test.txt")
  test_y_list <- read.table("./test/y_test.txt")
  test_subject_list <- read.table("./test/subject_test.txt")
  test_activity_set <- data.frame(Activity = activity_list$V2[test_y_list$V1])
  
  # combining the train list and test list for the features alone
  combine_list_x <- rbind(train_x_list,test_x_list)
  combine_subject_list <- rbind(train_subject_list,test_subject_list)
  combine_activity_list <- rbind(train_activity_set,test_activity_set)
  
  #modifying column name of subject column from V1 to Subject
  names(combine_subject_list) <- c("Subject")
  
  # fetching the feature name from feature list dataframe and assigning it to col name for x_list
  x_col_name <- as.character(features_list$V2)
  names(combine_list_x) <- x_col_name
  
  # selecting the columns having mean and standard deviation values
  select_col_list <- subset(combine_list_x, select=grep("*mean*|*std*" ,names(combine_list_x)))
  
  # extracting the first data set containing the mean and std along with the subject No and Activity 
  tidy_data_1 <- cbind(combine_subject_list,combine_activity_list,select_col_list)
  
  # writing the first tidy data set to file
  write.table(tidy_data_1, file="tidy_data_1.txt", row.names=FALSE)
  
  #Creating second tidy dataset by getting mena of the values, grouping based on activity and subject
  tidy_data_1_melted <- melt(tidy_data_1, id=c("Subject","Activity"))
  tidy_data_2 <- dcast(tidy_data_1_melted, Subject+Activity ~ variable, mean)
  
  # Writin the second dataset to the file
  write.table(tidy_data_2, file="tidy_data_2.txt", row.names=FALSE)
  
}