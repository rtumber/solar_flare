#Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(data.table)
library(DataExplorer)

#Download data description
names_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/solar-flare/flare.names", names_file)

length(readLines(names_file))
dataset_desc <- readLines(names_file)

rm(names_file)

#Examine and Tidy
print(dataset_desc)

#dataset_explanation
ds_explanation <- dataset_desc[12:18] %>% str_trim()
ds_explanation[1] <- paste(ds_explanation[1], ds_explanation[2], sep= " ")
ds_explanation <- ds_explanation[-2]
ds_explanation[2] <- paste(ds_explanation[2], ds_explanation[3], sep= " ")
ds_explanation <- ds_explanation[-3]
ds_explanation[3] <- paste(ds_explanation[3], ds_explanation[4], ds_explanation[5],  sep= " ")
ds_explanation <- ds_explanation[1:3]

ds_explanation <- str_replace(ds_explanation, "-- ", "")
print(ds_explanation)

#Dataset explanation directs us to use flare.data2 file.

#Attribute information
att_inf <- dataset_desc[25:48] %>% str_trim(side="left")
att_inf <- att_inf[att_inf != ""]
att_inf <- str_split(att_inf, "\\s\\s+", n = 2, simplify = TRUE)
att_inf[5,2] <- paste(att_inf[5,2], att_inf[6,1], sep = " ")
att_inf[7,2] <- paste(att_inf[7,2], att_inf[8,1], att_inf[9,1], sep = " ")
att_inf[11,1] <- paste(att_inf[11,1], att_inf[12,1], sep = " ")
att_inf[15,1] <- paste(att_inf[15,1], att_inf[16,1], sep = " ")
att_inf[17,1] <- paste(att_inf[17,1], att_inf[18,1], sep = " ")
att_inf[19,1] <- paste(att_inf[19,1], att_inf[20,1], sep = " ")
att_inf[21,1] <- paste(att_inf[21,1], att_inf[22,1], sep = " ")
att_inf <- att_inf[c(1:5, 7, 10:11, 13:15, 17, 19, 21), 1:2]
att_inf[,1] <- str_squish(att_inf[,1])
att_inf[,2] <- str_squish(att_inf[,2])
colnames(att_inf) <- c("Attribute", "Explanation")


#Download and import flare.data2 file
data_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/solar-flare/flare.data2", data_file)

length(readLines(data_file)) #number of records matches that described in description
flare_data <- readLines(data_file)
rm(data_file)

#Arrange into something useful  
print(flare_data)
flare_data <- flare_data[-1]
flare_data <- str_split(flare_data, "\\s", simplify = TRUE)
colnames(flare_data) <- c(att_inf[1:10,1], att_inf[12:14,1]) 
#Some column names are a little wordy, abbreviate in att_inf and reassign.
att_inf <- cbind(att_inf, att_abb = c("Modified-Zurich Class Code", "Largest spot size code", "Spot dist code", "Activity", "Evolution", "Flare activity code", "Historically-complex", "Recently historically-complex", "Area", "Largest spot area", "/" ,"C-class flares (common)", "M-class flares (moderate)", "X-class flares (severe)"))
colnames(flare_data) <- c(att_inf[1:10,3], att_inf[12:14,3]) 

