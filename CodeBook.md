The purpose of this project is to demonstrate ability to collect, work with, and clean a data set.  
The goal is to prepare tidy data that can be used for later analysis.  

One of the most exciting areas in all of data science right now is wearable computing.  
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users.  
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.  
A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Sctipt run_analysis.R perform the following steps:  
1. Merges the training and the test sets to create one data set.  
    Variables x, y, subj  
2. Extracts only the measurements on the mean and standard deviation for each measurement.   
    features, name x columns  
3. Uses descriptive activity names to name the activities in the data set.  
    activity, name y columns  
4. Appropriately labels the data set with descriptive variable names.  
    name subj columns, write data to file result.txt  
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
    write average data to result_avg.txt
