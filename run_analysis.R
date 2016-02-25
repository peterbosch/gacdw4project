run_analysis<- function(){
  #   ------------------------------------------------------------------
  #  |FUNCTION NAME: run_analysis()
  #  |DATE: February 23rd, 2016
  #  |CREATED BY: Pete Bosch
  #  |PROJECT FILE: run_analysis.R            
  #  |----------------------------------------------------------------
  #  |USAGE: run_analysis()       
  #  |RETURNS : After running, the files all and summary will exist in
  #  |          the directory [user's documents]/gacdw4projdata 
  #  |------------------------------------------------------------------
  #  |DATA USED: The URL "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  #  |          must exist, and its form must be as expected.
  #  |------------------------------------------------------------------
  #  |COMMENTS: Ineligible state or condition will result in an error.             
  #   ------------------------------------------------------------------
  
  # Set up environment.
  library(dplyr)
  library(readr)
  initialDir <- getwd()
  
  rawdata <- obtainRawData()
  test <- loadDataSet("test")
  training <- loadDataSet("train")
  all <- rbind(test,training)
  summary <- createSummary(all)
  
  # Save in a text format that can be easily loaded in R
  dump(c("all", "summary"), "dataset.Rdmpd")
  write.table(all,"all.txt",row.names = FALSE)
  write.table(summary,"summary.txt",row.names = FALSE)
  cat("Writing data tables to \"dataset.Rdmpd\", \"all.txt\" and \"summary.txt\"...\n")
  
  # Restore working directory to initial directory.
  setwd(initialDir)
}



obtainRawData <- function(){
  #   ------------------------------------------------------------------
  #  |FUNCTION NAME: obtainRawData()
  #  |DATE: February 23rd, 2016
  #  |CREATED BY: Pete Bosch
  #  |PROJECT FILE: run_analysis.R            
  #  |----------------------------------------------------------------
  #  |USAGE: obtainRawData()
  #  | Creates data directory, obtains and unzips data file.
  #  |RETURNS : If not already retrieved, goes to the embedded URL to
  #  |          retrieve the zipped data file, and if not already 
  #  |          unzipped, unzips it to the current directory.
  #  |------------------------------------------------------------------
  #  |DATA USED: The embedded sourceUrl must exist and contain expected
  #  |           data.
  #  |------------------------------------------------------------------
  #  |COMMENTS: Ineligible state or condition will result in an error.             
  #   ------------------------------------------------------------------  
  createAndChangeToDataDirectory()
  temp <- tempfile()
  sourceUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  localFile <- "getdata_projectfiles_UCI HAR Dataset.zip"
  if ( !file.exists(localFile)) download.file(sourceUrl,localFile)
  ## Comment the following out if experiencing problems with partial unzips...
  if ( !file.exists("README.TXT")) utils::unzip(localFile)
}

createAndChangeToDataDirectory <- function(refreshDirectory = FALSE){
  #   ------------------------------------------------------------------
  #  |FUNCTION NAME: createAndChangeToDataDirectory()
  #  |DATE: February 23rd, 2016
  #  |CREATED BY: Pete Bosch
  #  |PROJECT FILE: run_analysis.R            
  #  |----------------------------------------------------------------
  #  | Creates a data directory under the user's documents directory
  #  | to hold the analysis' data files, then changes the working
  #  | directory to that location.
  #  |USAGE: createAndChangeToDataDirectory(refreshDirectory=FALSE)       
  #  |         - refreshDirectory, if true, starts by removing any
  #  |           files already in the data directory.
  #  |RETURNS : the current directory.
  #  |------------------------------------------------------------------
  #  |DATA USED: None.
  #  |------------------------------------------------------------------
  #  |COMMENTS: Ineligible state or condition will result in an error.             
  #   ------------------------------------------------------------------  
  forClassEval <- TRUE
  if ( !forClassEval ){
    homeDirectory <- path.expand('~') ## The user's home directory for documents.
    targetDirectory <- "./gacdw4projdata"   ## "Getting and collecting data, weeek 4, project data."
    fulldirectory <- file.path(homeDirectory, targetDirectory)
    if ( refreshDirectory ) unlink(targetDirectory, recursive = TRUE)
    dir.create(fulldirectory, showWarnings = FALSE)
    setwd(fulldirectory)
  }
}

loadDataSet <- function(whichDataSet){
  #   ------------------------------------------------------------------
  #  |FUNCTION NAME: loadDataSet()
  #  |DATE: February 23rd, 2016
  #  |CREATED BY: Pete Bosch
  #  |PROJECT FILE: run_analysis.R            
  #  |----------------------------------------------------------------
  #  |USAGE: loadDataSet(whichDataSet)
  #  | Loads the test or training data set, as specified.
  #  |         - whichDataSet is "test" or "training", specifying the
  #  |           data set to load.
  #  |RETURNS : the test or training data set, as specified.
  #  |------------------------------------------------------------------
  #  |DATA USED: files from the downloaded & unzipped overall data set.
  #  |------------------------------------------------------------------
  #  |COMMENTS: Ineligible state or condition will result in an error.             
  #   ------------------------------------------------------------------  
  rootDir <- "UCI HAR Dataset"
  columnnamesSourceFile <- sprintf("%s\\features.txt",rootDir)
  dataSourceFile <- sprintf("%s\\%s\\X_%s.txt", rootDir, whichDataSet, whichDataSet)
  activitiesSourceFile <- sprintf("%s\\%s\\y_%s.txt", rootDir, whichDataSet, whichDataSet)
  subjectsSourceFile <- sprintf("%s\\%s\\subject_%s.txt", rootDir, whichDataSet, whichDataSet)
  
  ##print("Reading column names...")
  columnnames <- read.csv(columnnamesSourceFile, header = FALSE, sep = " ",stringsAsFactors = FALSE)
  columnnames <- c("subject", "activity", as.list(columnnames$V2))

  activities <- read.csv(activitiesSourceFile, header = FALSE, sep = " ")
  
  subjects <- read.csv(subjectsSourceFile, header = FALSE, sep = " ")
  
  cat(sprintf("Reading data for %s...\n", whichDataSet))
  data <- readr::read_fwf(file=dataSourceFile, col_positions = fwf_widths(rep.int(16,561)))
  
  data <- cbind(subjects,activities,data)
  columnnames <- gsub("-","_",columnnames)
  columnnames <- gsub("\\(\\)","",columnnames)
  names(data) <- columnnames
  
  useThisColumn <- grepl("^subject|^activity|_mean_|_std_", columnnames)
  
  data[useThisColumn]
  
}

createSummary <- function(all){
  #   ------------------------------------------------------------------
  #  |FUNCTION NAME: createSummary(all)
  #  |DATE: February 23rd, 2016
  #  |CREATED BY: Pete Bosch
  #  |PROJECT FILE: run_analysis.R            
  #  |----------------------------------------------------------------
  #  |USAGE: createSummary()  
  #  | Creates the summary data set, as specified in the assignment,
  #  | from a detailed data set passed in.
  #  |         - all is the detailed data set passed in.
  #  |RETURNS : the summary data set.
  #  |------------------------------------------------------------------
  #  |DATA USED: None
  #  |------------------------------------------------------------------
  #  |COMMENTS: Ineligible state or condition will result in an error.             
  #   ------------------------------------------------------------------  
  
  
  smry <- ddply(all,subject~activity,colMeans)
  cn <- colnames(smry)
  cn2 <- c(cn[1],cn[2],sprintf("Average_value_for_%s",cn)[3:50])
  colnames(smry) <- cn2
  smry
}

