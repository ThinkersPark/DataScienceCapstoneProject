library(tm)
library(tokenizers)
library(data.table)
library(slam)

prep_set <- function(myset){
  
  ## create the <s> filled data table
  ## add 3 x <s> to the beginning each line of the test set
  
  myset <- gsub("^","s s s ", myset)
  myset <- unlist(tokenize_ngrams(myset, n=4))
  
  ## efficient way of removing 1-grams that start with <s> 3 times
  myset <- data.table(gram=myset[-grep("^(s ){3}", myset)])
  ## if there are only 1 grams in the test set, don't bother
  if(nrow(myset) == 0) return (data.table(context_tokens=character(), target_nextword=character()))
  
  ## split function will split it into 1-, 2-, 3-long context tokens and 1 follow up word
  ## returns a 2-column data table of context_tokens and target_nextword
  tmp <- tstrsplit(myset$gram, " ")
  tmptable <- data.table(context_tok1 = tmp[[1]], context_tok2 = tmp[[2]], context_tok3 = tmp[[3]], target_nextword = tmp[[4]])
  ## you may want to keep the padding for perplexity, as it calls P4 directly
  tmptable[, context_tokens := paste(context_tok1, context_tok2, context_tok3)]
  myset <- tmptable[,.(context_tok1 = gsub("^(s)$","",context_tok1), context_tok2 = gsub("^(s)$","", context_tok2),
                       context_tok3 = gsub("^(s)$","",context_tok3), context_tokens = gsub("^(s ){1,2}","", context_tokens),
                       target_nextword)]
  
  ## Removing spaces again
  ## myset <- tmptable[, .(context_tokens, target_nextword)]
  ## return(myset[, .(context_tokens = gsub("^(s ){1,2}","", context_tokens),target_nextword)])
  
  myset
  
}


evaluate_on_dataset <- function(model, myset_prepped, topk = 5, fallback_top_unigram = 100) {
  total_words <- 0L; top1 <- 0L; top5 <- 0L; log_prob_sum <- 0L; pred_time <- 0L;
  
  ##prep testset returns a 2-column data table of context_tokens and target_nextword
  
  for(i in 1:length(myset_prepped$context_tokens)) {
    total_words <- total_words + 1L
    
    s_start <- Sys.time()
    preds <- predict_next_word(kn_model, myset_prepped$context_tokens[i], topk, fallback_top_unigram) 
    s_end <- Sys.time()
    pred_time <- pred_time + (s_end - s_start)
    
    if (length(preds) >= 1 && myset_prepped$target_nextword[i] == names(preds)[1]) top1 <- top1 + 1L
    if (myset_prepped$target_nextword[i] %in% names(preds)) top5 <- top5 + 1L
    
    prob <- max(model$P4(myset_prepped$context_tok1[i], myset_prepped$context_tok2[i],
                         myset_prepped$context_tok3[i], myset_prepped$target_nextword[i]),0)
    if(prob > 0) {
      log_prob_sum <- log_prob_sum + log(prob)
    } else {
      log_prob_sum <- log_prob_sum + log(1e-10)  # small prob for unseen
    }
  }
  
  list(total_words = total_words, top1 = top1 / total_words, top5 = top5 / total_words, 
       pred_time = pred_time/total_words,
       loglikelihood = log_prob_sum,
       perplexity = exp(-log_prob_sum / total_words))
  
}        
