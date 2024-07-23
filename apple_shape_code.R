## Apple metadata code

# import libraries
library(tidyverse)
library(tidyr)

# THIS CODE ONLY REFORMATS landmarks, you need to add in the other information as well
# Create empty table, which has the same number of columns as our final table needs but only 1 row (since it's empty)
results <- matrix(nrow=1, ncol=9)

#load in files with csvs from the folder
files <- list.files(path="C:/Users/User/Desktop/COOP02/apples/measurements_side",pattern="*.csv", full.names=TRUE, recursive=FALSE)

for(i in 1:length(files)) {
  print(i)
  file1 <- files[i]
  
  data <- read.csv(file1) # read in CSV file
  
  # reformat data to fit in meta data format
  data_table <- matrix(nrow=1, ncol=9) # Create an empty table 
  big <- matrix(nrow=1, ncol=9) 
  
  # confirm this based on order of the data in the spreadsheet (base vs tip)
  data_table[1,1] <- data[1,2] # Name of file
  data_table[1,2] <- "apple" # Dataset
  data_table[1,3] <- "81.93" # (px-cm) This was the average of 6 ruler measurements from the color_adjusted photos
  data_table[1,4] <- data[1,2] # Genotype
  data_table[1,6] <- "na" # Node
  data_table[1,6] <- data[1,3] # top x
  data_table[1,7] <- data[1,4] # top y
  data_table[1,8] <- data[2,3] # bottom x
  data_table[1,9] <- data[2,4] # bottom y
  
  #add in file name
  results <- rbind(results,data_table) 
}

# remove extra row at start
results <- results[-c(1),]

# give column names
# might have to change the genotype because i dont have that at the moment
colnames(results) <- c("file", "dataset", "px-cm","genotype", "node", "top_x", "top_y", "bottom_x", "bottom_y")

# set working directory to save file as a csv
setwd("C:/Users/User/Desktop/COOP02/code/apple_shape")
write.csv(results, file = "apple_metadata.csv")

# read in file
meta <- read.csv("apple_metadata.csv")

# separate genotype column to match if the nursery id will be correct
meta <- meta %>% separate(genotype, c('file_type', 'genotype', 'extension'), sep = '_')
meta <- meta %>% select(!c(X, file_type, extension))

# export the csv file
write.csv(meta, file = "apple_metadata.csv")