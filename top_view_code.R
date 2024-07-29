library(tidyverse)
library(readxl)

# read in file
top_meta <- read.csv("top_view_data.csv")
top_apple_info <- read_excel("20200204_abc_pop_info.xlsx")

top_meta <- top_meta %>% 
  separate(Name, into = c("binary", "nursery_id", "picture"), sep = "_", remove = FALSE) %>% 
  select(1, 3)

top_meta$nursery_id <- as.double(top_meta$nursery_id)

# make a list of the nursery ids that I have
# find those ids in the apple_info to get the apple_id
# paste apple_id in genotype column of meta
top_meta <- top_meta %>%
  left_join(top_apple_info) %>% 
  select(1,2,11)

# export the csv file
write.csv(top_meta, file = "top_view_data.csv")
