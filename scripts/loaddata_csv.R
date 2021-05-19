# load packages
library(dplyr)


# list all files in repository
files <- list.files(pattern = "_GA.csv", path = "data/", full.names = T)

# function to read GA file 
read_GAs <- function(file){
  data <- read.csv(file, sep = ";", 
                   stringsAsFactors = F, header = T, fileEncoding = "UTF-16LE") %>%
    # extract filenumber from file name
    mutate(session_number = gsub('.*_(\\d{1,3})_.*', '\\1', file)) %>%
    
    # remove unwanted columns 
    select(-sessionID_Text_Language__SL_, -sessionID_Age__S_,
           -sessionID_Gender__S_, -sessionID_Group__S_, 
           -sessionID_Experience__S_,
           -sessionID_Session__S_, -sessionID_filepath__S_)
}

# read all GA files at once
all_data_list <- lapply(files, read_GAs)

# put them into a dataframe
all_data <- do.call(rbind, all_data_list)

# write data
write.csv(all_data, "data_out/all_data.csv", row.names = F)