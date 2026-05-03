library(tm)
library(tokenizers)
library(data.table)
library(slam)

build_ngram_counts <- function(texts) {
  
  t1gram <- table(unlist(tokenize_ngrams(texts, n=1)))
  t2gram <- table(unlist(tokenize_ngrams(texts, n=2)))
  t3gram <- table(unlist(tokenize_ngrams(texts, n=3)))
  t4gram <- table(unlist(tokenize_ngrams(texts, n=4)))
  
  ## token -> n1gram, bigram -> n2gram etc.
  #uni_dt -> dt1gram, bi_dt -> dt2gram, tri_dt -> dt3gram, four_dt -> dt4gram
  ## needs to be a data.table object
  
  dt1gram  <- data.table(n1gram = names(t1gram), count = as.integer(unlist(t1gram)))
  dt2gram  <- data.table(n2gram = names(t2gram), count = as.integer(unlist(t2gram)))
  dt3gram  <- data.table(n3gram = names(t3gram), count = as.integer(unlist(t3gram)))
  dt4gram  <- data.table(n4gram = names(t4gram), count = as.integer(unlist(t4gram)))
  
  setkey(dt1gram, n1gram)
  setkey(dt2gram, n2gram)
  setkey(dt3gram, n3gram)
  setkey(dt4gram, n4gram)
  
  list(unigram = dt1gram, bigram = dt2gram, trigram = dt3gram, quadrigram = dt4gram)
  
}

compute_kn_components <- function (dtngram){
  
  ## Splitting n-grams into components for n+1
  
  split_4gram_dt_ok <- function(dt4gram) {
    if(nrow(dt4gram) == 0) return(data.table(w1=character(), w2=character(), w3=character(), w4=character(), count=integer()))
    tmp <- tstrsplit(dt4gram$n4gram, " ")
    dt <- data.table(w1 = tmp[[1]], w2 = tmp[[2]], w3 = tmp[[3]], w4 = tmp[[4]], count = dt4gram$count)
    dt
  }
  
  split_3gram_dt_ok <- function(dt3gram) {
    if(nrow(dt3gram) == 0) return(data.table(w1=character(), w2=character(), w3=character(), count=integer()))
    tmp <- tstrsplit(dt3gram$n3gram, " ")
    dt <- data.table(w1 = tmp[[1]], w2 = tmp[[2]], w3 = tmp[[3]], count = dt3gram$count)
    dt
  }
  
  split_2gram_dt_ok <- function(dt2gram) {
    if(nrow(dt2gram) == 0) return(data.table(w1=character(), w2=character(), count=integer()))
    tmp <- tstrsplit(dt2gram$n2gram, " ")
    dt <- data.table(w1 = tmp[[1]], w2 = tmp[[2]], count = dt2gram$count)
    dt
  }
  
  ## cleaning up 1-gram for format consistency
  
  clean_1gram_dt_ok <- function(dt1gram) {
    if(nrow(dt1gram) == 0) return(data.table(w1=character(), count=integer()))
    dt <- data.table(w1 = dt1gram$n1gram , count = dt1gram$count)
    dt
  }
  
  dt4gram <- split_4gram_dt_ok(dtngram$quadrigram)
  dt3gram <- split_3gram_dt_ok(dtngram$trigram)
  dt2gram <- split_2gram_dt_ok(dtngram$bigram)
  dt1gram <- clean_1gram_dt_ok(dtngram$unigram)
  
  ## For 4-grams:
  if(nrow(dt4gram) > 0) {
    Ncont_4gram_by_context <- dt4gram[, .N, by = .(w1, w2, w3)]
    setnames(Ncont_4gram_by_context, "N", "Ncont")
  } else {
    Ncont_4gram_by_context <- data.table(w1=character(), w2=character(), w3=character(), Ncont=integer())
  }
  
  ## For 3-grams:
  if(nrow(dt3gram) > 0) {
    Ncont_3gram_by_context <- dt3gram[, .N, by = .(w1, w2)]
    setnames(Ncont_3gram_by_context, "N", "Ncont")
  } else {
    Ncont_3gram_by_context <- data.table(w1=character(), w2=character(), Ncont=integer())
  }
  
  ## For 2-grams:
  if(nrow(dt2gram) > 0) {
    Ncont_2gram_by_context <- dt2gram[, .N, by = .(w1)]
    setnames(Ncont_2gram_by_context, "N", "Ncont")
  } else {
    Ncont_2gram_by_context <- data.table(w1=character(), Ncont=integer())
  }
  
  ## For 1-grams:
  total_1gram <- sum(dt1gram$count)
  names(total_1gram) <- "N"
  
  list(
    dt1gram = dt1gram,
    dt2gram = dt2gram,
    dt3gram = dt3gram,
    dt4gram = dt4gram,
    Ncont_4gram_by_context = Ncont_4gram_by_context,
    Ncont_3gram_by_context = Ncont_3gram_by_context,
    Ncont_2gram_by_context = Ncont_2gram_by_context,
    total_1gram = total_1gram
  )
}

make_kn_probability_model <- function(kn_comps, D = 0.75) {
  
  ## Here, the function returns dt4gram, dt3gram, dt2gram, dt1gram, 
  ## And Ncont_4gram_by_context , Ncont_3gram_by_context, Ncont_2gram_by_context, and total_1gram 
  ## As kn_components
  
  dt1gram <- kn_comps$dt1gram
  dt2gram <- kn_comps$dt2gram
  dt3gram <- kn_comps$dt3gram
  dt4gram <- kn_comps$dt4gram
  
  Ncont_4gram_by_context<- kn_comps$Ncont_4gram_by_context
  Ncont_3gram_by_context<- kn_comps$Ncont_3gram_by_context
  Ncont_2gram_by_context<- kn_comps$Ncont_2gram_by_context
  total_1gram <- kn_comps$total_1gram
  
  # set keys for fast lookup
  
  setkey(dt4gram, w1, w2, w3, w4)
  setkey(dt3gram, w1, w2, w3)
  setkey(dt2gram, w1, w2)
  setkey(dt1gram, w1)
  
  setkey(Ncont_4gram_by_context, w1, w2, w3)
  setkey(Ncont_3gram_by_context, w1, w2)
  setkey(Ncont_3gram_by_context, w1)
  
  P1 <- function(w1) {
    index_w1 <- which(dt1gram$w1 == w1)
    count_w1 <- dt1gram$count[index_w1]
    return( count_w1/ total_1gram )
  }
  
  P2 <- function(w1, w2) {
    
    index_w1w2 <- which(dt2gram$w1 == w1 & dt2gram$w2 == w2)
    count_w1w2 <- dt2gram$count[index_w1w2]
    
    index_Ncont_w1 <- which(Ncont_2gram_by_context$w1 == w1)
    Ncont_w1 <- Ncont_2gram_by_context$Ncont[index_Ncont_w1]
    
    if (sum(dt2gram$count[dt2gram$w1 == w1]) == 0) {
      p <-0
      backoff_weight2 <- 1
    } 
    else {
      p <- (max(count_w1w2 - D,0) / sum(dt2gram$count[dt2gram$w1 == w1]))
      backoff_weight2 <- D * (Ncont_w1 / sum(dt2gram$count[dt2gram$w1 == w1])) 
    }
    return( p + backoff_weight2 * P1(w2) )
  }
  
  P3 <- function(w1, w2, w3) {
    
    index_w1w2w3 <- which(dt3gram$w1 == w1 & dt3gram$w2 == w2 & dt3gram$w3 == w3)
    count_w1w2w3 <- dt3gram$count[index_w1w2w3]
    
    index_Ncont_w1w2 <- which(Ncont_3gram_by_context$w1 == w1 & Ncont_3gram_by_context$w2 == w2)
    Ncont_w1w2 <- Ncont_3gram_by_context$Ncont[index_Ncont_w1w2]
    
    if (sum(dt3gram$count[dt3gram$w1 == w1 & dt3gram$w2 == w2]) == 0) {
      p <-0
      backoff_weight3 <- 1
    } 
    else {
      p <- max(count_w1w2w3 - D,0) / sum(dt3gram$count[dt3gram$w1 == w1& dt3gram$w2 == w2])
      backoff_weight3 <- D * (Ncont_w1w2 / sum(dt3gram$count[dt3gram$w1 == w1 & dt3gram$w2 == w2]))
    }
    return( p + backoff_weight3 * P2(w2, w3) )
  }
  
  P4 <- function(w1, w2, w3, w4) {
    
    index_w1w2w3w4 <- which(dt4gram$w1 == w1 & dt4gram$w2 == w2 & dt4gram$w3 == w3 & dt4gram$w4 == w4)
    count_w1w2w3w4 <- dt4gram$count[index_w1w2w3w4]
    
    index_Ncont_w1w2w3 <- which(Ncont_4gram_by_context$w1 == w1 & 
                                  Ncont_4gram_by_context$w2 == w2 & Ncont_4gram_by_context$w3 == w3)
    Ncont_w1w2w3 <- Ncont_4gram_by_context$Ncont[index_Ncont_w1w2w3]
    
    if (sum(dt4gram$count[dt4gram$w1 == w1 & dt4gram$w2 == w2 & dt4gram$w3 == w3]) == 0) {
      p <-0
      backoff_weight4 <- 1
    } 
    else {
      p <- max(count_w1w2w3w4 - D,0) / sum(dt4gram$count[dt4gram$w1 == w1& dt4gram$w2 == w2 & dt4gram$w3 == w3])
      backoff_weight4 <- D * (Ncont_w1w2w3 / sum(dt4gram$count[dt4gram$w1 == w1 & dt4gram$w2 == w2 & dt4gram$w3 == w3]))
    }
    return( p + backoff_weight4 * P3(w2, w3, w4) )
  }
  
  list(
    P1 = P1,
    P2 = P2,
    P3 = P3,
    P4 = P4,
    components = kn_comps,
    D = D
  )
}

predict_next_word <- function(model, context_tokens, topk = 5, fallback_top_unigram = 100) {
  
  # context_tokens: character vector of previous tokens (only last two are used)
  comps <- model$components
  dt1gram <- comps$dt1gram
  dt2gram <- comps$dt2gram
  dt3gram <- comps$dt3gram
  dt4gram <- comps$dt4gram
  
  w_prev1 <- ifelse(length(context_tokens) >= 1, tail(context_tokens, 1), "<s>")
  w_prev2 <- ifelse(length(context_tokens) >= 2, tail(context_tokens, 2)[1], "<s>")
  w_prev3 <- ifelse(length(context_tokens) >= 3, tail(context_tokens, 3)[1], "<s>")
  
  # set of possible next words = union of:
  # - words that follow (w_prev3, w_prev2, w_prev1) in quadrigrams,
  # - words that follow (w_prev2, w_prev1) in trigrams,
  # - words that follow w_prev1 in bigrams,
  # - top unigrams (fallback)
  
  nextw_3w <- character(0)
  if(nrow(dt4gram) > 0) {
    nextw_3w <- dt4gram[w1 == w_prev3 & w2 == w_prev2 & w3 == w_prev1, unique(w4)]
  }
  
  nextw_2w <- character(0)
  if(nrow(dt3gram) > 0) {
    nextw_2w <- dt3gram[w1 == w_prev2 & w2 == w_prev1, unique(w3)]
  }
  
  nextw_1w <- character(0)
  if(nrow(dt2gram) > 0) {
    nextw_1w <- dt2gram[w1 == w_prev1, unique(w2)]
  }
  
  top_fallback <- if(nrow(dt1gram) > 0) 
    dt1gram[order(-count)][seq_len(min(nrow(dt1gram), fallback_top_unigram))]$w1 else character(0)
  
  next_words <- unique(c(nextw_3w, nextw_2w, nextw_1w, top_fallback))
  if(length(next_words) == 0) {
    return(character(0))
  }
  
  # compute probabilities for next words
  # return probabilities not just words
  probs <- vapply(next_words, function(w) model$P4(w_prev3, w_prev2, w_prev1, w), numeric(1))
  ord <- order(probs, decreasing = TRUE)[1:min(topk, length(probs))]
  ## next_words[ord]
  probs[ord]
}
