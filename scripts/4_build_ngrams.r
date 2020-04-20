###
#
###

setwd(paste0(dirname(parent.frame(2)$ofile), "/.."))

library(quanteda)
library(stringr)

source("scripts/config.r")

nrams_freq_df <- function(tokens, ngram) {
  tokens <- ngrams(tokens, n = ngram, concatenator = "_")
  data_ngram <- dfm(tokens, ngrams = ngram, toLower = F, verbose = T)
  
  df_freq <- sort(colSums(data_ngram), decreasing = T)
  
  df_freq  <- data.frame(names(df_freq), df_freq)
  names(df_freq) <- c("words", "frequency")
  
  df_freq <- df_freq[df_freq$frequency >= MIN_FREQUENCY, ]
  
  if (ngram > 1) {
    df_freq$sentence <- word(df_freq$words, -ngram, -2, sep = fixed("_"))
    df_freq$prediction <- word(df_freq$words, -1, sep = fixed("_"))
    df_freq <- df_freq[!df_freq$sentence == "",]
    df_freq <- df_freq[!df_freq$prediction == UNK,]
    df_freq$words <- NULL
    df_freq  <- df_freq[c("sentence", "prediction", "frequency")]
  }
  rownames(df_freq) <- c(1:nrow(df_freq))
  df_freq  <- df_freq[complete.cases(df_freq),]
  df_freq
}

nrams_freq_df_probs <- function(df) {
  df_probs <- data.frame(X=table(df$frequency))
  names(df_probs) <- c("r", "n")
  df_probs$r <- as.numeric(as.character(df_probs$r))
  Xlength <- nrow(df_probs)
  
  # Assign to N the sum of the products of the pairs of integers 
  # in the r and n columns
  # This will be the number of individuals in the sample.
  N <- sum(df_probs$r*df_probs$n)
  
  # Estimate of the total probability of all unseen species
  P_0 <- df_probs$r[1]/N
  
  df_probs$Z <- 0
  
  for (c in 1:Xlength) {
    if (c == 1) {
      i <- 0
    } else {
      i <- i <- df_probs$r[c-1]
    }
    if (c == Xlength) {
      k <- df_probs$r[c]
    } else {
      k <- df_probs$r[c+1]
    }
    df_probs$Z[c] <- 2*df_probs$n[c] / ( k-i )
  }
  
  df_probs$logr <- log(df_probs$r)
  df_probs$logZ <- log(df_probs$Z)
  df_probs$rstar <- 0
  
  # linear regression model
  model1 <- glm(logZ ~ logr, data = df_probs)
  
  c0 <- model1$coefficients[1]
  c1 <- model1$coefficients[2]
  
  ycheck = FALSE
  for (c in 1:(Xlength-1)) {
    rplus1 <- df_probs$r[c] + 1
    
    s_rplus1 <- exp(c0 + (c1 * df_probs$logr[c+1]))
    s_r <- exp(c0 + (c1 * df_probs$logr[c]))
    y <- rplus1 * s_rplus1/s_r
    
    if(ycheck) {
      df_probs$rstar[c] <- y
    } else { 
      n_rplus1 <- df_probs$n[df_probs$r == rplus1]
      n_r <- df_probs$n[c]
      x <- (rplus1) * n_rplus1/n_r
      
      limit <- sqrt(((rplus1)^2) * (n_rplus1/((n_r)^2)) * (1 + (n_rplus1/n_r)))
      
      if (abs(x-y) > 1.96 * limit) {
        df_probs$rstar[c] <- x
      }else {
        df_probs$rstar[c] <- y
        ycheck = TRUE
      }
    }
    if(c==(Xlength-1)) {
      df_probs$rstar[c+1] <- y
    }
    
  }
  
  N_1 <- sum(df_probs$n * df_probs$rstar)
  df_probs$p <- (1-P_0) * df_probs$rstar/N_1
  
  df_probs
}

print("creating ngram data splits")

rds_file <- sprintf("%s/%s.rds", model_data_dir, "data_train_tetragram")
if (!file.exists(rds_file)) {
  for (src in c(#"blogs", 
                #"news", 
                #"twitter", 
                "data")) {
    for (x in list(
                   c("unigram", 1), # cause 'the'/'and' is always most frequent
                   c("bigram", 2),
                   c("trigram", 3),
                   c("tetragram", 4))) {
      ngram_type <- as.character(x[1])
      ngram <- as.numeric(x[2])
      
      txt_file <- sprintf("%s/%s_train.rds", data_clean_dir, src)
      txt <- readRDS(txt_file)
      
      print(sprintf("creating %s for %s", ngram_type, src))
      
      t <- system.time({
        print("creating ngrams")
        data_ngram <- nrams_freq_df(txt, ngram)
        
        print("creating ngrams probabilities")
        data_probs <- nrams_freq_df_probs(data_ngram)
        
        print("merging ngrams with probabilities")
        data_ngram$probability <- data_probs$probability[
          match(data_ngram$freq, data_probs$r)]
        data_ngram <- data_ngram[
          order(data_ngram$probability, decreasing = TRUE),]
        })
      
      rds_file <- sprintf("%s/%s_%s.rds", model_data_dir, src, ngram_type)
      saveRDS(data_ngram, rds_file)
      
      print(sprintf("%s for %s created in %.3f s", ngram_type, src, t[3]))
      
      rm(txt, data_ngram)
      gc()
    }
    print("ngram data splits created")
  }  
} else {
  print("ngram data splits already exists")
}


rm(src, x, ngram_type, ngram, txt_file, txt, data_ngram, t, rds_file)
gc()