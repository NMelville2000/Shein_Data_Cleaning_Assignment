#01_ingest.r

#Load Libraries

library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(scales)

#Read in the raw dataset
raw_file <- read_csv(raw_data_path)
View(raw_file)

#Validating the data compared to the Source Document
nrow(raw_file)
ncol(raw_file)
names(raw_file)
head(raw_file)
class(raw_file)
str(raw_file)
summary(raw_file)

#Separating by spitting data
