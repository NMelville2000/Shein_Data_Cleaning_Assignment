#01_ingest.r

#Load Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(scales)
library(data.table)
library(openxlsx)
library(stringi)
library(lubridate)
library(purrr)

#Read in the raw dataset
raw_file <- fread(
        raw_file_path,
        encoding = "UTF-8",
        fill = TRUE,
        quote = "\""
)

raw_file <- as.data.frame(raw_file)

cat("Raw data loaded successfully.\n")
cat("Rows:", nrow(raw_file), "\n")
cat("Columns:", ncol(raw_file), "\n")


# View(raw_file)

#Validating the data compared to the Source Document
dim(raw_file)
head(raw_file)
names(raw_file)
class(raw_file)
str(raw_file)
summary(raw_file)


###################Creating a small excel file sample to view data######################
# dir.create("small_sample", showWarnings = FALSE)
# 
# # Take the first 50 rows
# sample_50 <- raw_file[1:min(50, nrow(raw_file)), ]
# 
# # Fix invalid UTF-8 text in all character columns
# sample_50 <- as.data.frame(
#         lapply(sample_50, function(x) {
#                 if (is.character(x)) {
#                         x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
#                         x <- stri_replace_all_regex(x, "\\p{C}", "")
#                 }
#                 return(x)
#         }),
#         stringsAsFactors = FALSE
# )
# 
# 
# # write.xlsx(
# #         sample_50,
# #         file = "shein_sample_50_rows.xlsx",
# #         overwrite = TRUE
# # )
# 
# cat("First 50 rows saved successfully in the Output folder.")




