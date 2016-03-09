# setwd("d:/Users/fabaldaro/Desktop/datasciencecoursera")
# wd=getwd()
pollutantmean <- function(directory, pollutant, id = 1:332)  {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating the name of the
  ## pollutant for which we will calculate the mean; either 'sulfate' or
  ## 'nitrate'.
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant across all monitors list in the 'id'
  ## vector (ignoring NA values)
  ## NOTE: do not round the results!
  files_list <- list.files(directory, full.names=TRUE)   ##creates a list of files
  data <- data.frame()                             ##creates an empty data frame
  for (i in id) {                                
    ##loops through the files, rbinding them together 
    data <- rbind(data, read.csv(files_list[i]))
  }
  data_subset <- data[which(data[, "ID"] %in% id),]  #subsets the rows that match the argument
  return(mean(data_subset[, pollutant], na.rm=TRUE))      # return the mean of pollutant across all monitors list
  #while stripping out the NAs
}
## pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064128
## pollutantmean("specdata", "nitrate", 23)
## [1] 1.280833
## pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047

