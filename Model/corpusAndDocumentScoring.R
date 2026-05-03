library(tm)
library(tokenizers)
library(data.table)
library(stringr)
library(slam)

setwd("C:/Users/Oliwia Kozlowska/Documents/R/RworkCoursera/Capstone/DataScienceCapstoneProject")

source("./Model/knFunctions.R")

## Corpus reading and pre-processing data

path <- paste0(getwd(),"/Data/final/en_US")

myCorp <- VCorpus(DirSource(path, encoding ="UTF-8"),
                  readerControl = list(reader = readPlain, language="en-US"))

myCorp <- tm_map(myCorp,content_transformer(tolower))
myCorp <- tm_map(myCorp,removePunctuation)
myCorp <- tm_map(myCorp,removeNumbers)
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
myCorp <- tm_map(myCorp,f,"'")
myCorp <- tm_map(myCorp,f,"~")
myCorp <- tm_map(myCorp,f,"@")
myCorp <- tm_map(myCorp,f,"#")
myCorp <- tm_map(myCorp,f,"$")
myCorp <- tm_map(myCorp,f,"%")
myCorp <- tm_map(myCorp,f,"&")
myCorp <- tm_map(myCorp,f,"*")
myCorp <- tm_map(myCorp,f,"+")
myCorp <- tm_map(myCorp,f,"_")
myCorp <- tm_map(myCorp,f,"^")

sCorp <- sapply(c(myCorp[[1]]$content,myCorp[[2]]$content,myCorp[[3]]$content), function(row) iconv(row, "latin1", "ASCII", sub="")) 
myCorp <- Corpus(VectorSource(sCorp))
## save(myCorp, file="./Data/dimReduction/myCorp.RData")

## Initial dimensionality reduction

## load("./Data/dimReduction/myCorp.Rdata") 
myControlTfIdf <- list(tolower=TRUE,
                       removePunctuation = TRUE,
                       removeNumbers = TRUE,
                       ## stopwords=stopwords("en"), 
                       stemming=FALSE,
                       wordLengths=c(1,Inf),
                       weighting = function(x)
                         weightTfIdf(x, normalize = FALSE)
)

MegaDTMTfIdf <- DocumentTermMatrix(c(myCorp[[1]]$content,myCorp[[2]]$content,myCorp[[3]]$content),myControlTfIdf)

qut <- quantile(slam::col_sums(MegaDTM, na.rm = T),0.95)
SampleDTM <- MegaDTM[,which(slam::col_sums(MegaDTM, na.rm = T)>qut)]
SampleDTM <- SampleDTM[which(slam::row_sums(SampleDTM, na.rm = T)>0),]

qud <- quantile(slam::row_sums(SampleDTM, na.rm = T),0.95)
SampleDTM <- SampleDTM[which(slam::row_sums(SampleDTM, na.rm = T)>qud),]
SampleDTM <- SampleDTM[,which(slam::col_sums(SampleDTM, na.rm = T)>0)]
save(SampleDTM, file="SampleDTM.RData")

myCorpTfIdf <- c(myCorp[[1]]$content,myCorp[[2]]$content,myCorp[[3]]$content)[as.numeric(SampleDTM$dimnames$Docs)]
## conversion to VCorpus to enable custom tokeniser
vCorpTfIdf <- VCorpus(VectorSource(myCorpTfIdf))
## save(vCorpTfIdf, file="./Data/dimReduction/vCorpTfIdf.Rdata")

## custom 4-gram tokeniser function
tokenizer_wrapper <- function(x) {
  # Convert document to plain text
  text <- as.character(x)
  # Call tokenize_ngrams from tokenizers
  tokens <- tokenize_ngrams(text, n = 4, simplify = TRUE)
  # Ensure output is a character vector
  return(tokens)
}

## Creating DTM
DTMngramTfIdf <- DocumentTermMatrix(vCorpTfIdf, control = list(tokenize = tokenizer_wrapper, 
                                                               weighting = function(x) 
                                                                 weightTfIdf(x, normalize = FALSE),
                                                               wordLengths = c(1, Inf)))
## save(DTMngramTfIdf, file="./Data/dimReduction/DTMngramTfIdf.Rdata")



## Document scoring & selection

## load("./Data/dimReduction/vCorpTfIdf.Rdata") 
## load("./Data/dimReduction/DTMngramTfIdf.Rdata") 

## helpful function to multiply sparse matrix by a vector

sparse_matvec <- function(A, x) {
  # Input validation
  if (!inherits(A, "simple_triplet_matrix")) {
    stop("A must be a 'simple_triplet_matrix' from slam.")
  }
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  if (length(x) != A$ncol) {
    stop("Length of x must equal the number of columns in A.")
  }
  
  # Initialize result vector
  result <- numeric(A$nrow)
  
  # Efficiently accumulate row sums
  for (k in seq_along(A$v)) {
    result[A$i[k]] <- result[A$i[k]] + A$v[k] * x[A$j[k]]
  }
  
  return(result)
}

## Global term frequency and weights
myTermFreq <- slam::col_sums(DTMngramTfIdf)
myWeights <- log1p(myTermFreq)

# Score docs: dfm %*% weights (VERY fast)
scores_raw <- as.numeric(sparse_matvec(DTMngramTfIdf, myWeights))
doc_len <- slam::row_sums(DTMngramTfIdf)
scores_norm <- scores_raw / (doc_len + 1)

docsNscores <- data.table(docs = myCorpTfIdf, scores = scores_norm )
## save(docsNscores, file="./Data/dimReduction/docsNscores.Rdata")
## load("./Data/dimReduction/docsNscores.Rdata")

data_size <- c()
model_size <- c()
kn_time <- c()

## Choose a sample of texts to test prediction time (from assignment test)

text1 <- "a case of"
text2 <- "make me the"
text3 <- "be on my"
text4 <- "in quite some"
text5 <- "you must be"

## Running the below code for different K values
## Using the model functions to construct the KEN model

K <- 10000; K <- 5000; K <- 2000; K <- 1000; K <- 100

top_idx <- order(docsNscores$scores, decreasing = TRUE)[seq_len(K)]
docsNscores_subset <- data.table(docs = docsNscores$docs[top_idx], scores = docsNscores$scores[top_idx])
dtngram<- build_ngram_counts(docsNscores_subset$docs)
kn_comps <- compute_kn_components(dtngram)  
kn_model <- make_kn_probability_model(kn_comps)

subset_size <- object.size(docsNscores_subset); subset_size; data_size <- c(data_size,subset_size)
kn_size <- object.size(kn_model); kn_size; model_size <- c(model_size,kn_size)

s_start <- Sys.time()
context_tokens <-text1
predict_next_word(kn_model, context_tokens, topk = 5, fallback_top_unigram = 100) 
s_end <- Sys.time()
time1 <- s_end - s_start; time1 

s_start <- Sys.time()
context_tokens <-text2
predict_next_word(kn_model, context_tokens, topk = 5, fallback_top_unigram = 100) 
s_end <- Sys.time()
time2 <- s_end - s_start; time2 

s_start <- Sys.time()
context_tokens <-text3
predict_next_word(kn_model, context_tokens, topk = 5, fallback_top_unigram = 100) 
s_end <- Sys.time()
time3 <- s_end - s_start; time3

s_start <- Sys.time()
context_tokens <-text4
predict_next_word(kn_model, context_tokens, topk = 5, fallback_top_unigram = 100) 
s_end <- Sys.time()
time4 <- s_end - s_start; time4

s_start <- Sys.time()
context_tokens <-text5
predict_next_word(kn_model, context_tokens, topk = 5, fallback_top_unigram = 100) 
s_end <- Sys.time()
time5 <- s_end - s_start; time5

kn_time <- c(kn_time, sum(unclass(time1)[1],unclass(time2)[1],unclass(time3)[1],unclass(time4)[1],unclass(time5)[1])/5)

data_size; model_size; kn_time
## [1] 6361136 3130064 1244168  624672 56552
## [1] 112563544 61515704 27818880  14912296  702616
## [1] 15.71427064  6.21411338  2.35848055  0.88785982  0.01773076

data_size <- c(6361136, 3130064, 1244168, 624672, 56552)
model_size <- c(112563544, 61515704, 27818880, 14912296, 702616)
model_time <- c(15.71427064, 6.21411338, 2.35848055, 0.88785982, 0.01773076)

## Saving down the selected dataset

# Selecting final K=1000 given model size & time to predict the next word below 1s

K_final<- 1000
top_idx <- order(docsNscores$scores, decreasing = TRUE)[seq_len(K_final)]
docsNscores_final <- data.table(docs = docsNscores$docs[top_idx], scores = docsNscoresK1$scores[top_idx])

## Extracting n-grams and their weights to feed into the KEN model

subsetCorp <- VCorpus(VectorSource(docsNscores_final$docs))

## Creating DTM
DTMngramTfIdfSubset <- DocumentTermMatrix(subsetCorp, control = list(tokenize = tokenizer_wrapper, 
                                                                     weighting = function(x)
                                                                       weightTfIdf(x, normalize = FALSE),
                                                                     wordLengths = c(1, Inf)))

## Global term frequency and weights
myTermFreq <- slam::col_sums(DTMngramTfIdfSubset)
myWeights <- log1p(myTermFreq)
myNgrams <- DTMngramTfIdfSubset$dimnames$Terms

save(myNgrams, file="./Data/dimReduction/myNgrams.Rdata")
save(myWeights , file="./Data/dimReduction/myWeights.Rdata")

