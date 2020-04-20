language_of_interest <- "en_US" # should be one of [de_DE, en_US, fi_FI, ru_RU]

data_dir <- "data"
data_raw_dir <- "data/raw"
data_clean_dir <- "data/clean"

model_dir <- "shiny"
model_data_dir <- "shiny/data"

prediction_dir <- "data"
prediction_data_dir <- "data/prediction"

# percent of total lines used in train, validation and test stages
# the higher the percentage of data used in train, the higher the accuracy, but
# also the higher the sparsity of data and the memory/time required to process the data
train_part <- 80.0
validation_part <- 10.0
test_part <- 10.0

dict_dir <- "shiny/dicts/cracklib-small.txt"
UNK <- "UNK"

MIN_FREQUENCY <- 4