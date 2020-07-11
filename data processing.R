# load all the packages
library(dplyr)
library(tm)
library(RWeka)
library(tidyr)
library(ggplot2)

setwd("~/Desktop/R_files/Git_repo/datasciencecoursera/Capstone")

# read lines of blogs text
blogs <- readLines("./data/en_US/en_US.blogs.txt")

# read lines of news text
news <- readLines("./data/en_US/en_US.news.txt")

# read lines of twitter text
twitter <- readLines("./data/en_US/en_US.twitter.txt")

# Summarize the basic information of the three dataset.

df_datasum<- data.frame("File" = c("blogs", "news", "twitter"),
                        "Number of Lines" = c(length(blogs), length(news), length(twitter)),
                        "Number of Words" = c(wordcount(blogs), wordcount(news), wordcount(twitter)),
                        "File Size in MB" = c(file.size("./final/en_US/en_US.blogs.txt")/1024^2, 
                                              file.size("./final/en_US/en_US.news.txt")/1024^2,
                                              file.size("./final/en_US/en_US.twitter.txt")/1024^2))
df_datasum

# From the above summary, we can see that the twitter dataset has the most lines and the least words while the blogs dataset has the most words and least lines.


# 2. Sampling
# The above summary shows that the datasets are fairly large. I will use a smaller subset of the data to build the algorithms and models. To do that, I will creat separate sub-sample datasets by sampling 10,000 lines of each dataset.

set.seed(177)
blogs_sample <- sample(blogs, size = length(blogs)*0.05, replace = FALSE)
news_sample <- sample(news, size = length(news)*0.05, replace = FALSE)
twitter_sample <- sample(twitter, size = length(twitter)*0.05, replace = FALSE)
sample_data <- c(blogs_sample, news_sample, twitter_sample)
saveRDS(sample_data, file="./data/raw_sample_data.rds")

# clean
sample_data <- gsub("[^[:alpha:][:space:]]", "", sample_data)
# or use this to remove numbers and punctuations
# sample_data <- gsub("[[:digit:][:punct:]']", " ", sample_data)
saveRDS(sample_data, file="./data/sample_data.rds")

# 3. Cleaning the Data
# The text mining functions provided by the tm package will be applied here to perform cleaning of the data. The above combined sample dataset will be transformed into a sample corpus dataset. The sample corpus will be further transformed via the following steps:
# - changing all letters to lower cases
# - remove whitespace
# - remove words of offensive and profane meaning

sample_corpus <- VCorpus(VectorSource(sample_data))
# download the google bad words list
if(!file.exists("./final/badwords.txt")){
        download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", destfile = "./final/badwords.txt")
}
badwords <- readLines("./final/badwords.txt")
sample_corpus <- tm_map(sample_corpus, content_transformer(tolower)) 
sample_corpus <- tm_map(sample_corpus, stripWhitespace)
sample_corpus <- tm_map(sample_corpus, removeWords, badwords)
# sample_corpus <- tm_map(sample_corpus, removeWords, stopwords(kind = "en"))
saveRDS(sample_corpus, file="./data/sample_corpus.rds")

remove(blogs, blogs_sample, news, news_sample, twitter, twitter_sample)
remove(sample_data, badwords)

# 4. Exploratory Data Analysis
# The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The n-gram model will be used. The first step is to get the tokens of the data, namely unigrams (single word), bi-grams (two words), and tri-grams (three words).

sample_corpus <- readRDS(file="./data/sample_corpus.rds")

# create tokenizers
unigram_tokenizer <- function(x){NGramTokenizer(x, Weka_control(min=1, max=1))}
bigram_tokenizer <- function(x){NGramTokenizer(x, Weka_control(min=2, max=2))}
trigram_tokenizer <- function(x){NGramTokenizer(x, Weka_control(min=3, max=3))}
quadgram_tokenizer <- function(x){NGramTokenizer(x, Weka_control(min=4, max=4))}

# get the term document matrix for the tokens 
unigram_tdm <- TermDocumentMatrix(sample_corpus, control = list(tokenize = unigram_tokenizer))
bigram_tdm <- TermDocumentMatrix(sample_corpus, control = list(tokenize = bigram_tokenizer))
trigram_tdm <- TermDocumentMatrix(sample_corpus, control = list(tokenize = trigram_tokenizer))
quadgram_tdm <- TermDocumentMatrix(sample_corpus, control = list(tokenize = quadgram_tokenizer))

saveRDS(bigram_tdm, file="./data/bigram_tdm.rds")
saveRDS(trigram_tdm, file="./data/trigram_tdm.rds")
saveRDS(quadgram_tdm, file="./data/quadgram_tdm.rds")
remove(sample_corpus)

# The next step is to explore the frequencies of the tokens.
# unigram
unigram_freq <- rowSums(as.matrix(removeSparseTerms(unigram_tdm, 0.999)))
unigram_freq <- sort(unigram_freq, decreasing = TRUE)
df_unigram <- data.frame(word=names(unigram_freq), freq = unigram_freq)
saveRDS(df_unigram, "./ngram_data/ngram1.rds")
remove(unigram_tdm, unigram_freq, df_unigram)

# bigram
bigram_freq <- rowSums(as.matrix(removeSparseTerms(bigram_tdm, 0.9999)))
bigram_freq <- sort(bigram_freq, decreasing = TRUE)
df_bigram <- data.frame(word=names(bigram_freq), freq = bigram_freq)
df_bigram <- df_bigram %>% separate(word, c("word1", "word2"), sep=" ")
saveRDS(df_bigram, "./ngram_data/ngram2.rds")
remove(bigram_tdm, bigram_freq, df_bigram)

# trigram
trigram_freq <- rowSums(as.matrix(removeSparseTerms(trigram_tdm, 0.99996)))
trigram_freq <- sort(trigram_freq, decreasing = TRUE)
df_trigram <- data.frame(word=names(trigram_freq), freq = trigram_freq)
df_trigram <- df_trigram %>% separate(word, c("word1", "word2", "word3"), sep=" ")
saveRDS(df_trigram, "./ngram_data/ngram3.rds")
remove(trigram_tdm, trigram_freq, df_trigram)

# quadgram
quadgram_freq <- rowSums(as.matrix(removeSparseTerms(quadgram_tdm, 0.99998)))
quadgram_freq <- sort(quadgram_freq, decreasing = TRUE)
df_quadgram <- data.frame(word=names(quadgram_freq), freq = quadgram_freq)
df_quadgram <- df_quadgram %>% separate(word, c("word1", "word2", "word3", "word4"), sep=" ")
saveRDS(df_quadgram, "./ngram_data/ngram4.rds")
remove(quadgram_tdm, quadgram_freq, df_quadgram)


# Plot the top 10 most frequent tokens to visualize the variation in the frequencies of unigrams, bigrams, and trigrams in the data.
# plot unigram
# plot_unigram <- ggplot(df_unigram[1:10,], aes(reorder(word, -freq), freq)) + 
#         geom_bar(stat = "identity", fill ="red") +
#         theme(axis.text.x = element_text(angle=45, hjust=1)) + 
#         ggtitle("Top 10 Unigrams") +
#         xlab("Unigrams") + ylab("Frequency")
# plot_unigram
# 
# # plot bigram
# plot_bigram <- ggplot(df_bigram[1:10,], aes(reorder(word, -freq), freq)) + 
#         geom_bar(stat = "identity", fill ="blue") +
#         theme(axis.text.x = element_text(angle=45, hjust=1)) + 
#         ggtitle("Top 10 bigrams") +
#         xlab("Bigrams") + ylab("Frequency")
# plot_bigram
# 
# # plot trigram
# plot_trigram <- ggplot(df_trigram[1:10,], aes(reorder(word, -freq), freq)) + 
#         geom_bar(stat = "identity", fill ="blue") +
#         theme(axis.text.x = element_text(angle=45, hjust=1)) + 
#         ggtitle("Top 10 bigrams") +
#         xlab("Trigrams") + ylab("Frequency")
# plot_trigram
# 
# # plot quadgram
# plot_quadgram <- ggplot(df_quadgram[1:10,], aes(reorder(word, -freq), freq)) + 
#         geom_bar(stat = "identity", fill ="blue") +
#         theme(axis.text.x = element_text(angle=45, hjust=1)) + 
#         ggtitle("Top 10 bigrams") +
#         xlab("Quadgrams") + ylab("Frequency")
# plot_quadgram