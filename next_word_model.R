library(dplyr)
library(tidyr)
library(tm)

# load training data words
ngram1 <- readRDS("./ngram_data/ngram1.rds")
ngram2 <- readRDS("./ngram_data/ngram2.rds")
ngram3 <- readRDS("./ngram_data/ngram3.rds")
ngram4 <- readRDS("./ngram_data/ngram4.rds")

# clean input
input_words <- function(x){
        x <- gsub("[^[:alpha:][:space:]]", "", x) %>% tolower()
        x <- data.frame(strsplit(x, " ")) %>% setNames("words")
        return(x)
}

# match ngram
match_ngram2 <- function(y){
        num <- nrow(y)
        nextword <- subset(ngram2, word1 == y[num,1])[1,2]
        ifelse(is.na(nextword), ".", return(nextword))
}

match_ngram3 <- function(y){
        num <- nrow(y)
        nextword <- subset(ngram3, word1 == y[num-1,1] & 
                                   word2 == y[num,1])[1,3]
        ifelse(is.na(nextword), match_ngram2(y), return(nextword))
}

match_ngram4 <- function(y){
        num <- nrow(y)
        nextword <- subset(ngram4, word1 == y[num-2,1] & 
                                    word2 == y[num-1,1] & 
                                    word3 == y[num,1])[1,4]
        ifelse(is.na(nextword), match_ngram3(y), return(nextword))
}

# predict next word
next_word <- function(x){
        y <- input_words(x)
        num <- nrow(y)
        nextword <- ifelse(num == 1, match_ngram2(y), 
                         ifelse(num == 2, match_ngram3(y), 
                                match_ngram4(y)))
        return(nextword)
}