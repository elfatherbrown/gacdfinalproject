
# Got this handy thing from:
# http://www.salemmarafi.com/code/install-r-package-automatically/
# If a package is installed, it will require it. If not, 
# it will install, then require. Nice!
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
#install.packages(c("readr","stringr","tidyr","dplyr","janitor"),dependencies = TRUE)
usePackage("readr")
usePackage("stringr")
usePackage("tidyr")
usePackage("dplyr")
usePackage("janitor")
#####
#This script should comply with the follwing:
#####
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation
#   for each measurement.
# 3) Uses descriptive activity names to name the activities in the 
#   data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and each subject.


#This variables configures the name of the directory of the unzipped dataset
DATADIR = "UCI HAR Dataset"

#We check if the data directory is in the current working directory
checkData <- function() {
  if (DATADIR %in% dir()) {
    print("UCI HAR Dataset directory found")
    return(TRUE)
  }
  else
  {
    print(
      "The unziped data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    )
    print("Must be in the current working directory")
    return (FALSE)
  }
}
####
# Solution functions
###

#1.- Merge The Datasets
#3.- Uses descriprive activity_names (featureVector provides this)
mergeTheDatasets <- function() {
  return(bind_rows(featureVector("test"),featureVector("train")))
}

#2.- Extract only mean and standard deviation meassurments
meanAndSv<-function(m=FALSE) {
  if(is.data.frame(m)){
    s<-m
  }
  else
  {
    s<-mergeTheDatasets()
    
  }
  return(s%>%select(1:5,contains("mean"),contains("std")))
}

#5.- Get a dataset with the average of each activity and each subject
byActivitySubject <- function (m=FALSE) {
  if(is.data.frame(m)){
    s<-m
  }
  else
  {
    s<-meanAndSv()
  }
  return(s%>%group_by(activity_name,subject) %>% summarise_at(.vars = -(1:5),.funs=mean))
}

####
# Main dataset loading function
###
#The documentation specifies:
# For each record it is provided:
#   ======================================
#
# - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
# - Triaxial Angular velocity from the gyroscope.
# - A 561-feature vector with time and frequency domain variables.
# - Its activity label.
# - An identifier of the subject who carried out the experiment.

#featureVector gets all the variables of the features, which are
#variables calculated from functions of the acceleration,body acceleration and gyroscope
#readings and others subsequently calculated. It will also load the subject
# and the activity that was being performed, translated to text

featureVector <- function (dname) {
  fname<- paste(c("X_", dname, ".txt"), collapse  = "")
  split <- getFileFeatureVector(dname,fname)
  #Transpose the resulting matrix
  t <- t(split)
  #Give her the colnames!
  v <- featureVarnames()
  v <- as.vector(v$name)
  colnames(t) <- v
  t <- as_tibble(t,.name_repair="universal")
  #Add the subjects, they are listed in the same order of the main observations
  #file
  s <- subjects(dname)
  t$subject <- s
  a <- activities(dname)
  t$activity_id <- a$activity_id
  t$activity_name <- a$activity_name
  t$obs_id <- seq_along(t$activity_id)
  #Return with the subject as first column, everything else at last
  t$set_name<-dname
  t<- t %>% select(set_name,obs_id, subject, activity_id, activity_name, everything())
  
  return(t%>%clean_names())
}


####
# Helper functions
###

#Designed for the features in the dirname/X_dirname.txt files
#where dirname == test|train.
#dname == "test"|"train"
#fname == "filename"
#It will return a matrix of as many rows as lines
#and 561 values, which is what the files have.
getFileFeatureVector <- function(dname, fname) {
  con <- file(file.path(getwd(),DATADIR, dname, fname))
  test_obs_byline <- readLines(con)
  #The feature files have exactly one extra space at the begining of each line
  # and exactly 561 values per observation(line).
  # Thus the hardcoded indexing [2:562] at the end of the parse_number
  split <-
    sapply(test_obs_byline, function(line) {
      return(parse_number(strsplit(line, split = "\\s+")[[1]][2:562]))
    })
  close.connection(con)
  return (split)
}


#Load the subject list, which awe assume to be listed in the same
#order as the main observations file
subjects <- function (dname) {
  fname <- paste(c("subject_", dname, ".txt"), collapse  = "")
  con <- file(file.path(getwd(),DATADIR, dname, fname))
  subjects <- readLines(con)
  close.connection(con)
  return(subjects)
}
#Loads the activities and the activity_labels.txt files
#It uses the second as a translation table to create
#an activity_id,activity_name tibble corresponding neatly
#to the featureVector observation (one per line)
activities <- function (dname) {
  #Parse the activity number to name file:
  actrans <-
    read.delim(file.path(getwd(),DATADIR, "activity_labels.txt"),
               sep = " ",
               header = FALSE)
  #Make it a tibble just for better syntax
  #We now have a translation table activity_id,activity_name
  actrans <-
    as_tibble(actrans) %>% mutate(activity_id = V1, activity_name = V2) %>% select(activity_id, activity_name)
  #Parse the activities for this directory
  fname <- paste(c("y_", dname, ".txt"), collapse  = "")
  con <- file(file.path(getwd(),DATADIR, dname, fname))
  activities <- parse_number(readLines(con))
  close.connection(con)
  #The follwing will make a tibble with a seq number on the activity column
  #and a value column, which holds the actual id of the activity for that observation
  activities <- tibble::enframe(activities, name = "activity")
  #This will put the translated name of an activity into activity_name
  activities$activity_name <-
    actrans$activity_name[match(activities$value, actrans$activity_id)]
  #Cleanup and return
  activities <-
    activities %>% mutate(activity_id = value) %>% select(activity_id, activity_name)
  return(activities)
}

#Loads all the variable names available in the features.txt file
featureVarnames <- function () {
  v <-
    read.delim(file.path(getwd(),DATADIR, "features.txt"),
               header = FALSE,
               sep = " ") %>% select(pos = V1, name = V2)
}

########
# The program itself
########

#We shall stop if the checkData function does not return TRUE
stopifnot(checkData())
print("This script will comply with a set of things")
print("1.- Merges the training and the test sets to create one data set.")
print("This is done by the mergeTheDatasets function which I will call now")
print("Please ignore the New names warnings. They are ok")
print("")
print("It can take a while...")
merged_ds<-mergeTheDatasets()
print("Examine the merged_ds variable at the end of this run...")
print("View(merged_ds) will show the full thing. ")
print("But here is a sample of 10 rows")
print(sample_n(merged_ds,10))
print("Scroll up to see the sample")
print("Remember you can see the whole thing in the merged_ds variable")
print("2.- Extracts only the measurements on the mean and standard deviation for each measurement.")
print("This is accomplished by my meanAndSv function")
msv_ds<-meanAndSv(merged_ds)
print("Check the sample now:")
print(sample_n(msv_ds,10))
print("Scroll up to see the sample")
print("Remember you can see the whole thing in the msv_ds variable")

print("3.- Uses descriptive activity names to name the activities in the data set")
print("You can see the activity_name variable in either dataset")
print("has descriptive activity names. I extracted them from msv_ds for you:")
print(unique(msv_ds$activity_name))

print("4.- Appropriately labels the data set with descriptive variable names.")
print("As I understand it, no camel case, no funny characters, and descriptive")
print("info in each variable name should be provided. You can see my variables")
print("comply with this")
print(head(names(msv_ds),10))
print("5.- From the data set in step 4, creates a second, independent tidy")
print("data set with the average of each variable for each activity and each subject.")
print("This is accomplished by the byActivitySubject function")
act_subj<-byActivitySubject(msv_ds)
print("You can see the result in the act_subj dataframe")
print("here is a sample")
print(head(act_subj,10))
write_csv(act_subj,file.path(getwd(),"by_activity_and_subject.csv"))
print("Hi, I executed in four steps all of the required stuff")
print("Scroll up to see what I did")
