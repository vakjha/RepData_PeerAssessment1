FileUpload <- function() 
{ 
  
  if (!file.exists("./activity.csv")) { 
    
    # download the data 
    file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
    file_name <- "repdata_data_activity.zip" 
    download.file(file_url, file_name) 
    unzip(file_name) 
  } 
  # read data   
  data <- read.csv("./activity.csv", header = TRUE)
 } 
