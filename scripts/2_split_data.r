###
# this script splits the data for train, validation and test cases
###

# set working directory to the project root folder
setwd(paste0(dirname(parent.frame(2)$ofile), "/.."))

source("./scripts/config.r")

library(R.utils)
set.seed(42)

if( (train_part + validation_part + test_part) != 100 
    | train_part < 0  | validation_part < 0 | test_part < 0) {
  stop("total sum can't excede 100 %")
}

print("creating train, test, validation splits")

if (!file.exists(sprintf("%s/data_train.rds", data_raw_dir))
    | !file.exists(sprintf("%s/data_validation.rds", data_raw_dir))
    | !file.exists(sprintf("%s/data_test.rds", data_raw_dir))) {
  
  train_data <- c()
  validation_data <- c()
  test_data <- c()
  
  for (src in c("blogs", "news", "twitter")) {
    # train split
    txt <- readRDS(sprintf("%s/%s.rds", data_raw_dir, src))
    num_lines <- length(txt)
    
    for (x in list(c("train", train_part),
                   c("validation", validation_part),
                   c("test", test_part))) {
      split_type <- as.character(x[1])
      split_percent <- as.numeric(x[2])
      
      sample_size <- ceiling(num_lines * split_percent / 100)
      curr_txt_len <- length(txt)
      
      if (sample_size < curr_txt_len) {
        sampled_ids <- sample(curr_txt_len, sample_size)
        sampled_lines <- txt[sampled_ids]
        txt <- txt[-sampled_ids]
      } else {
        # works only for last part
        sampled_lines <- txt
      }
      
      if (split_type == "train") {
        train_data <- c(train_data, sampled_lines)
      } else if (split_type == "validation") {
        validation_data <- c(validation_data, sampled_lines)
      } else if (split_type == "test") {
        test_data <- c(test_data, sampled_lines)
      }
    }
  }
  
  #save files containing data samples from all corpus
  saveRDS(train_data, sprintf("%s/data_train.rds", data_raw_dir))
  saveRDS(validation_data, sprintf("%s/data_validation.rds", data_raw_dir))
  saveRDS(test_data, sprintf("%s/data_test.rds", data_raw_dir))
  
  print("train, test, validation splits created")
  
  rm(train_data, validation_data, test_data, txt, num_lines,
     split_type, split_percent, sample_size, curr_txt_len,
     sampled_ids, sampled_lines, x, src, rds_file)
} else {
  print("train, test, validation splits already exists")
}
