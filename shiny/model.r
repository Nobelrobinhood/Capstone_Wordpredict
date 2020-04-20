
library(quanteda)
library(stringr)

source("config.r")

dct <- readLines(dict_dir, encoding = "UTF-8")

unigrams <- readRDS(sprintf("%s/%s.rds", data_dir, "data_unigram"))
bigrams <- readRDS(sprintf("%s/%s.rds", data_dir, "data_bigram"))
trigrams <- readRDS(sprintf("%s/%s.rds", data_dir, "data_trigram"))
tetragrams <- readRDS(sprintf("%s/%s.rds", data_dir, "data_tetragram"))

ngrams_names <- c("unigrams", "bigrams", "trigrams", "tetragrams")

preprocess_input <- function(txt) {
  txt <- corpus(tolower(txt))
  tokens <- tokenize_word(txt)
  mask <- tokens[[1]] %in% dct
  tokens[[1]][!mask] <- UNK
  tokens[[1]]
}

ngram_prediction <- function(txt, ngram = 4) {
  ngram <- min(ngram - 1, length(txt))
  
  txt <-tail(txt, ngram)
  txt <- paste(txt, collapse = "_")
  
  res <- list()
  res$bigram <- word(txt, -1, -1, sep = fixed("_"))
  res$trigram <- word(txt, -2, -1, sep = fixed("_"))
  res$tetragram <- word(txt, -3, -1, sep = fixed("_"))
  
  res
}

next_word <- function(txt) {
  txt <- preprocess_input(txt)
  num_words <- length(txt)
  ngrams <- ngram_prediction(txt)
  
  if(num_words >= 3 && (!exists("prediction") || nrow(prediction) < 5)) {
    tetra_prediction <- tetragrams[tetragrams$sentence == ngrams$tetragram, ]
    if (!exists("prediction")) {
      prediction <- head(tetra_prediction, 5)
    } else {
      prediction <- rbind(prediction, head(tetra_prediction, 5))  
    }
    
  }
  
  if(num_words >= 2 && (!exists("prediction") || nrow(prediction) < 5)) {
    tri_prediction <- trigrams[trigrams$sentence == ngrams$trigram, ]
    if (!exists("prediction")) {
      prediction <- head(tri_prediction, 5)
    } else {
      prediction <- rbind(prediction, head(tri_prediction, 5))  
    }
  }
  
  if(num_words >= 1 && (!exists("prediction") || nrow(prediction) < 5)) {
    bi_prediction <- bigrams[bigrams$sentence == ngrams$bigram, ]
    if (!exists("prediction")) {
      prediction <- head(bi_prediction, 5)
    } else {
      prediction <- rbind(prediction, head(bi_prediction, 5))  
    }
  }
  
  if (!exists("prediction")) {
    return(rep("the", 5))
  } else {
    prediction <- prediction[order(prediction$probability, decreasing = TRUE),]
    prediction <- prediction$prediction
    prediction_length <- length(prediction)
    if (prediction_length < 5) {
      prediction <- c(prediction, rep("the", 5 - prediction_length))
    }
    return(prediction)
  }
}

