library(tm)
library(tokenizers)
library(data.table)
library(stringr)
library(slam)

setwd("C:/Users/Oliwia Kozlowska/Documents/R/RworkCoursera/Capstone/DataScienceCapstoneProject")

source("./Model/knFunctions.R")
source("./Model/evalFunctions.R")

## Calibrating the size of the KEN model by number of top-scoring n-grams
## The set of n-grams to consider, and their weights, were extracted from the top scoring documents in the previous step, 
## And saved down in the "./Data/dimReduction" project directory ("myNgrams.Rdata" and "myWeights.Rdata" files, respectively).

load("./Data/dimReduction/myNgrams.Rdata")
load("./Data/dimReduction/myWeights.Rdata")

## Creating training, validation, and testing sets for model evaluation and selection

seed <- 123
set.seed(seed)

K_train_max <- 50000
K_val <- 5000
partition1 <- sample(1:length(myNgrams), size=K_train_max, replace=FALSE)
partition2 <- sample(1:length(myNgrams[-partition1]), size=K_val, replace=FALSE)
training_set_max <- myNgrams[partition1]
newset <- myNgrams[-partition1]
validation_set <- newset[partition2]
testing_set <- newset[-partition2]
rm(newset)
training_set_weights <- myWeights[partition1]

## Building the KEN model

D <- 0.75           # discount (typical value for MKN)
top_unigram_fallback <- 10   # number of top unigrams to include in fallback candidates

## Running the below code for different Kngram values
## Using the model functions to construct and evaluate the KEN model

Kngrams <- 10000; Kngrams <- 20000; Kngrams <- 30000; Kngrams <- 40000; Kngrams <- 50000

training_set <- names(training_set_weights[order(training_set_weights, decreasing = TRUE)[seq_len(Kngrams)]])
dtngram<- build_ngram_counts(training_set)
kn_comps <- compute_kn_components(dtngram)  
kn_model <- make_kn_probability_model(kn_comps)

trainset_prepped <- prep_set(training_set)
valset_prepped <- prep_set(validation_set)

eval10k_train <- evaluate_on_dataset(kn_model, trainset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval10k_train, file="./Data/dimReduction/eval10k_train.Rdata")

## object.size(kn_model)
## 5035192 bytes
## object.size(training_set)
## 864656 bytes

eval10k_train_full <- c(eval10k_train ,ts_size=864656, model_size=5035192)
eval10k_val <- evaluate_on_dataset(kn_model, valset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval10k_val, file="./Data/dimReduction/eval10k_val.Rdata")


eval20k_train <- evaluate_on_dataset(kn_model, trainset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval20k_train, file="./Data/dimReduction/eval20k_train.Rdata")

## object.size(kn_model)
## 8058168 bytes
## object.size(training_set)
## 1723088 bytes

eval20k_train_full <- c(eval20k_train ,ts_size=1723088, model_size=8058168)
eval20k_val <- evaluate_on_dataset(kn_model, valset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval20k_val, file="./Data/dimReduction/eval20k_val.Rdata")

eval30k_train <- evaluate_on_dataset(kn_model, trainset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval30k_train, file="./Data/dimReduction/eval30k_train.Rdata")

## object.size(kn_model)
## 10389464 bytes
## object.size(training_set)
## 2578928 bytes

eval30k_train_full <- c(eval30k_train ,ts_size=2578928, model_size=10389464)
eval30k_val <- evaluate_on_dataset(kn_model, valset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval30k_val, file="./Data/dimReduction/eval30k_val.Rdata")


eval40k_train <- evaluate_on_dataset(kn_model, trainset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval40k_train, file="./Data/dimReduction/eval40k_train.Rdata")

## object.size(kn_model)
## 12150768 bytes
## object.size(training_set)
## 3434768 bytes

eval40k_train_full <- c(eval40k_train ,ts_size=3434768, model_size=12150768)
eval40k_val <- evaluate_on_dataset(kn_model, valset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval40k_val, file="./Data/dimReduction/eval40k_val.Rdata")


eval50k_train <- evaluate_on_dataset(kn_model, trainset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval50k_train, file="./Data/dimReduction/eval50k_train.Rdata")

## object.size(kn_model)
## 13472864 bytes
## object.size(training_set)
## 4289920 bytes

eval50k_train_full <- c(eval50k_train ,ts_size=4289920, model_size=13472864)

eval50k_val <- evaluate_on_dataset(kn_model, valset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval50k_val, file="./Data/dimReduction/eval50k_val.Rdata")

# Selecting final Kngrams=20k given model size & time to predict the next word below 1s

Kngrams_final <- 20000
top_ngrams <- data.table(text = names(myWeights[order(myWeights, decreasing = TRUE)[seq_len(Kngrams_final)]]))

training_set_final <- names(training_set_weights[order(training_set_weights, decreasing = TRUE)[seq_len(Kngrams_final)]])
dtngram<- build_ngram_counts(training_set_final)
kn_comps <- compute_kn_components(dtngram)  
kn_model_final <- make_kn_probability_model(kn_comps)

save(training_set_final, file = ".Data/dimReduction/training_set_final.RData")
save(kn_model_final, file = "./Data/dimReduction/kn_model_final.RData")

## Evaluation on test set

testset_prepped <- prep_set(testing_set)
eval20k_test <- evaluate_on_dataset(kn_model_final, testset_prepped, topk = 5, fallback_top_unigram = 10)
save(eval20k_test, file="./Data/dimReduction/eval20k_test.Rdata")

## Preparing for app load

training_set_app_load<- training_set_final
kn_model_app_load <- kn_model_final

save(training_set_app_load, file = "./App/data/training_set_app_load.RData")
save(kn_model_app_load, file = "./App/data/kn_model_app_load.RData")


