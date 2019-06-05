# Checks for existance of the data directory,
# outputs instructions otherwise
library("here")
library("readr")
library("stringr")
library("dplyr")

DATADIR = "UCI HAR Dataset"
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
##Line by line reader. All files here are a line per record,
#  values therefor space delimited

processFile = function(filepath) {
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    print(line)
  }
  
  close(con)
}
getFileFeatureVector <- function(dname, fname) {
  con <- file(file.path(getwd(),DATADIR, dname, fname))
  test_obs_byline <- readLines(con)
  #The feature files have exactly one extra space at the begining of each line
  # and exactly 561 variables. Thus the indexing at the end of the parse_number
  split <-
    sapply(test_obs_byline, function(line) {
      return(parse_number(strsplit(line, split = "\\s+")[[1]][2:562]))
    })
  close.connection(con)
  return (split)
}
getFileSignalVector <- function(dname, fname) {
  con <- file(file.path(getwd(),DATADIR, dname, fname))
  test_obs_byline <- readLines(con)
  split <-
    sapply(test_obs_byline, function(line) {
      ls<-strsplit(line, split = "\\s+")
      len<-length(ls[[1]])
      #This is the time domain signal of 128 samples
      curvec<-ls[[1]][2:(len)]
      return(curvec)
    })
  close.connection(con)
  return (split)
}
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
  t <- as_tibble(t)
  #Add the subjects, they are listed in the same order of the main observations
  #file
  s <- subjects(dname)
  t$subject <- s
  a <- activities(dname)
  t$activity_id <- a$activity_id
  t$activity_name <- a$activity_name
  t$obs_id <- seq_along(t$activity_id)
  #Return with the subject as first column, everything else at last
  t<- t %>% select(obs_id, subject, activity_id, activity_name, everything())
  return(t)
}

#signalVector will return the values, one per column, for:
#  x, y and z axis in:
#   ls test/Inertial\ Signals/
# body_acc_x_test.txt	body_acc_z_test.txt	body_gyro_y_test.txt	total_acc_x_test.txt	total_acc_z_test.txt
# body_acc_y_test.txt	body_gyro_x_test.txt	body_gyro_z_test.txt	total_acc_y_test.txt

#

signalVector <- function (dname) {
  fnames <- dir(file.path(getwd(),DATADIR, dname, "Inertial Signals"))
  # split <- sapply(
  #   here(DATADIR, dname, "Inertial Signals", fnames),
  #   FUN = read.delim,
  #   stringsAsFactors = FALSE,
  #   row.names = NULL,
  #   sep="\t"
  # )
  split <-
    sapply(
      fnames,
      FUN = function(fn) {
        return (getFileSignalVector(paste(
          dname, "/", "Inertial Signals/",sep = "", collapse = ""), fn
        ))}
        )
  #The sapply strategy gives us columns and values inverted. We traspose:
  
  split<-as_tibble(split)
  #Remove .txt from the column names
  
  
  names(split) <-
    sapply(str_replace_all(names(split), pattern = "^(.*)\\.txt$", "/\\1"),
           basename)
  #Parse the numbers
  split <-
    split %>% 
    mutate_all(parse_number) %>% 
      mutate(s_id=seq_len(length.out = nrow(split)),
             time_id = rep(seq(1, 128, by=1),nrow(split)/128)) %>% 
                group_by(time_id) %>% 
                  mutate(obs_id=seq_along(time_id))%>% 
                  ungroup() %>%
                    select(s_id,obs_id,time_id,everything())
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
#Load the subject list, which we assume to be listed in the same
#order as the main observations file
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
  activities$activity_name <-
    actrans$activity_name[match(activities$value, actrans$activity_id)]
  activities <-
    activities %>% mutate(activity_id = value) %>% select(activity_id, activity_name)
  return(activities)
}


featureVarnames <- function () {
  v <-
    read.delim(file.path(getwd(),DATADIR, "features.txt"),
               header = FALSE,
               sep = " ") %>% select(pos = V1, name = V2)
}

#Given a directory name, looks for the appropriate files
# and builds a non tidy tibble that we will use to clean this
#stuff
aRecord <- function (dir) {
  #The root directory has a features.txt file, with the features and
  # an activity_labels.txt file, which labels the activities being
  #done for each observation in the rest of the files.
  # a features.txt file wich lists the name of each variable meassured
  # per observation. This has the variable names.
  
  #We are provided with two directories test|train, which have:
  #- an subject_dirname.txt file, with a line per meassure and
  # the subject number (a subject identifier), is repeated in each line
  # for each observation in the X_dirname file
  #- an X_dirname.txt file, with a line per observation (and 561 read variables). Each line corresponds
  # to each subject in the subject_dirname.txt file
  #- an y_test.txt file, with a number identifying the activity being done for each observation
  # in the previous file
  
  #First part of the record are the observed values on the X_dirname.txt. file
  
  
  #OUTPUT:
  #We will deliver a clean tibble with all variable names clearly stated, one full observation
  # per row, identified by subject-activity pair
  
}

#1. Merges the training and the test sets to create one data set.
mergeSamsungData <- function () {
  ###
  #Do the nasty stuff here
  ###
}
checkData()

