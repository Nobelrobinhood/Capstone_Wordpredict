###
# this script downloads data, unpacks it to 'data_raw_dir'
# and creates userfull .rds representation of 'language_of_interest' files
###

# set working directory to the project root folder
setwd(paste0(dirname(parent.frame(2)$ofile), "/.."))

source("./scripts/config.r")

# download corpus and unzip if needed
print("downloading Coursera-SwiftKey.zip")
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zip_file <- sprintf("%s/Coursera-SwiftKey.zip", data_raw_dir)

if (!file.exists(zip_file)) {
  download.file(url, method="curl", destfile = zip_file) 
  print("Coursera-SwiftKey.zip downloaded")
} else {
  print("Coursera-SwiftKey.zip already exists")
}

print("unpacking Coursera-SwiftKey.zip")
lang_dir <- sprintf("%s/final/%s", data_raw_dir, language_of_interest)

if (!file.exists(lang_dir)) {
  unzip(zip_file, exdir = data_raw_dir)
  print("Coursera-SwiftKey.zip unpacked")
} else {
  print("Coursera-SwiftKey.zip for 'language of interest' already unpacked")
}

rm(url, zip_file, lang_dir)


# save language of interest text files as RDS, 
# it's compressed and it takes shorter to load the files in R
print("optimising representation data format")
for (src in c('blogs', 'news', 'twitter')) {
  rds_file <- sprintf("%s/%s.rds", data_raw_dir, src)
  if (!file.exists(rds_file)) {
    txt_file <- sprintf("%s/final/%s/%s.%s.txt", data_raw_dir, 
                        language_of_interest, language_of_interest, src)
    txt <- readLines(txt_file, skipNul = T, encoding="UTF-8")
    saveRDS(txt,rds_file) 
    print(sprintf("%s created", rds_file))
  } else{
    print(sprintf("%s already exists", rds_file))
  }
}
print("representation data format optimised")

rm(txt, src, rds_file)
