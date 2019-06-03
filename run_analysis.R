# Checks for existance of the data directory, 
# outputs instructions otherwise
DATADIR="UCI HAR Dataset"
checkData <- function() {
  if (DATADIR %in% dir()) {
    print("UCI HAR Dataset directory found")
    return(TRUE)
  }
  else
  {
    print("The unziped data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
    print("Must be in the current working directory")
    return (FALSE)
  }
}
##Line by line reader. All files here are a line per record, 
#  values therefor space delimited

processFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

#Given a directory name, looks for the appropriate files
# and builds a non tidy tibble that we will use to clean this
#stuff
fromDirToTibble <- function (dir) {
  
}

#1. Merges the training and the test sets to create one data set.
mergeSamsungData <- function () {
  t1 <- fromDirToTibble("./test/")
  t2 <- fromDirToTibble("./train/")
  ###
  #Do the nasty stuff here
  ###
}