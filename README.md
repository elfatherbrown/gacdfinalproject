# Final project
## Getting and Cleaning Data course

#### Files in this project:
- README.md
  - This file
- README.Rmd (also in html and pdf)
  - PLEASE DO READ: **README.pdf**, directly on github. It is [the actual readme file](https://github.com/elfatherbrown/gacdfinalproject/blob/master/README.pdf).
  - The full documentation of the project itself
  - You can run it using the knitr package in Rstudio
- codebook.txt
  - A discussion on the resulting dataset of this project and its variables
- new_variable_names.txt
  - An equivalence '=' separated table of old variable names = new variable name
- run_analysis.R
  - The script you should run to see if I did my work.
  - While its not interactive, it will output what it does at each step, along with 
  dataset samples. It can be anoying, but if you scroll to the top and slowly go down,
  you'll see why I thought it was a good idea.

### Most important reminder:

#### 1. git clone this repo
#### 2. cd to that directory
### Get and unzip the dataset unto the UCI HAR Dataset subdirectory of this cloned directory
#### 3. If on mac/linux:
###### wget [this file](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
##### 3. If on Windows, using Rstudio's console:
###### a) setwd("directory where you cloned this")
###### b) download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="Dataset.zip")
####  c) go unzip the Dataset.zip file.
