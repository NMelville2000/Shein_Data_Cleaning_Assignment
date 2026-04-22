#00_config.r 

raw_file_path <- "Raw_data/shein_sample.csv"

if (!file.exists(raw_file_path)) {
        stop("The raw data file was not found. Please check that shein_sample.csv is inside the Raw_data folder.")
}