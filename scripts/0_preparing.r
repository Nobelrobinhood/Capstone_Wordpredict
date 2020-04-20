###
# this script prepares all necessary directories for future work
###

# set working directory to the project root folder
setwd(paste0(dirname(parent.frame(2)$ofile), "/.."))

source("scripts/config.r")

print("creating necessary data directories")
for (curr_dir in c(data_dir, data_raw_dir, data_clean_dir, 
                   model_dir, model_data_dir, 
                   prediction_dir, prediction_data_dir)) {
  if (!file.exists(curr_dir)) {
    dir.create(curr_dir, showWarnings = FALSE)
  }
}

curr_dir <-sprintf("%s/final/%s", data_raw_dir, language_of_interest)
if (!file.exists(curr_dir)) {
  dir.create(curr_dir, showWarnings = FALSE)
}

print("all directories created")

rm(curr_dir)