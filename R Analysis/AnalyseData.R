library(dplyr)
library(plyr)
library(R.matlab)

load_data <- function(fileloc, filepattern){
  
  files <- list.files(fileloc, full.names = T)
  
  files <- files[grep(filepattern, files)]
  
  data <- data.frame()
  
  for(file in files){
    
    newdata <- read.csv(file)
    
    
    data <- rbind(data, newdata)
    
  }
  
  return(data)
}