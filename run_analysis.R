library(base)
library(utils)
library(data.table)

# This function downloads the datafile and unzips it
downloadDataFile <- function () {
  zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zipUrl, destfile = "dataset.zip")
  unzip(zipfile = "dataset.zip", exdir = "data")
}


# Returning a dataset from training or test file
# @set: train or test
# @features: tblFeatures
# @labels: the column labels
getDataset <- function (set, features, labels) {
  
  # Creates the path to common, test or training files
  prefix <- paste(set, '/', sep = '')
  tblData <- paste(prefix, 'X_', set, '.txt', sep = '')
  tblLabel <- paste(prefix, 'y_', set, '.txt', sep = '')
  tblSubject <- paste(prefix, 'subject_', set, '.txt', sep = '')
  dataTable <- read.table(tblData)[, features$index]
  names(dataTable) <- features$name
  
  label <- read.table(tblLabel)[, 1]
  dataTable$label <- factor(label, levels=labels$level, labels=labels$label)
  
  subject <- read.table(tblSubject)[, 1]
  dataTable$subject <- factor(subject)
  
  # convert to data table
  data.table(dataTable)
}

# Creates the tinyData.csv
runAnalysis <- function () {
  #Set my working directory. Please change if needed
  setwd('~/Coursera_WorkingDirectory/Coursera/UCI HAR Dataset/')
  
  # Getting the features from txt file
  tblFeature <- read.table('features.txt', col.names = c('index', 'name'))
  features <- subset(tblFeature, grepl('-(mean|std)[(]', tblFeature$name))
  
  # Getting the activity labels from txt file and assign column names
  tblLabel <- read.table('activity_labels.txt', col.names = c('level', 'label'))
  
  # Reading train and test data sets
  tblTrain <- getDataset('train', features, tblLabel)
  tblTest <- getDataset('test', features, tblLabel)
  
  # Combine the train and test dataset by rbind
  rawData <- rbind(tblTrain, tblTest)
  
  # Generate the tidy data set
  dataTidy <- rawData[, lapply(.SD, mean), by=list(label, subject)]
  
  # replacing a couple of characters and clearing some variable names
  names <- names(dataTidy)
  names <- gsub('-mean', 'Mean', names) 
  names <- gsub('-std', 'Std', names) 
  names <- gsub('BodyBody', 'Body', names)
  names <- gsub('[()-]', '', names)   
  setnames(dataTidy, names)
  
  # Writing the result file in working directory
  setwd('..')
  write.csv(dataTidy, file = 'tidydata.csv', row.names = FALSE)

}