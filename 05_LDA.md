Topic Models
================
Jessica She
2023-06-01

#### Latent Dirichlet Allocation (LDA) is the popular topic model available to extract the topics from text documents.

- Topic modeling is a probability-based approach to finding clusters
  within documents. It is unsupervised because you do not have
  document-assigned classes like “positive” or “negative” for IMDB movie
  reviews.  

- When working on finding topics within text documents, there are two
  distinct probabilities. If there are “K” topics, “D” documents, and
  “W” words that make all of these “D” documents, then there is a word
  probability that defines a topic (a set of words makes a topic or a
  topic can be explained using a set of words). Word1 is highly likely
  to describe topic 1, but it may be less likely to explain topic 2 than
  it was previously. The second probability is the likelihood of a given
  document (a sentence or set of sentences) being associated with a
  given topic.

- The words describing the topics are hidden, and the topics within the
  documents are hidden (latent), which is why the algorithm is called
  LDA.  

- Gibbs Sampling is the most commonly used sampling method for
  identifying topics and word probabilities.  

- A Markov chain Monte Carlo (MCMC) algorithm is used in the Gibbs
  sampler. It approximates distributions, then generates correlated
  Markov Chain samples for statistical inference.  

- The topic model inference yields two (approximate) posterior
  probability distributions: theta over K topics within each document
  and beta over V terms within each topic, where V represents the
  collection’s vocabulary length.

``` r
library(tidytext)
library(tm)
library(topicmodels)
library(ldatuning)
library(textmineR)
library(tidyverse)
library(readxl)
library(textcat)
library(stringr)
library(modeltools)
library(wordcloud)
```

- Read the dataset consisting of Blutooth speaker reviews from Amazon

``` r
text <- read_xlsx(paste0(getwd(), "/Input/BlutoothSpeaker_B09JB8KPNW.xlsx"))
```

``` r
text_pre_process <- function(corpus) {
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
  return(corpus)
}
```

``` r
text <- text %>%
  mutate(language = textcat(review_text)) %>%
  filter(language == "english") %>%
  mutate(review_text <- str_replace_all(review_text, "[[:punct:]]", ""))

corpus <- Corpus(VectorSource(text$review_text))
corpus <- text_pre_process(corpus)
text_tm_processed <- corpus$content
text_tm_processed <- str_squish(text_tm_processed)
text_tm_processed <- as.data.frame(text_tm_processed)
names(text_tm_processed) <- c("text")
```

``` r
corpus <- Corpus(VectorSource(text_tm_processed$text))
dtm <- DocumentTermMatrix(corpus)
#, 
                  # control = list(bounds = list(global = c(5, Inf) 
                                                #)))
# Include frequency terms that appear in at least 50 documents 
dtm <- dtm[,findFreqTerms(dtm,50)]

# Include the dtm rows that contain non-zero rows
nonzero_row_numbers <- slam::row_sums(dtm) > 0

# Create updated dtm
dtm <- dtm[nonzero_row_numbers, ]
```

``` r
finetune_result <- FindTopicsNumber(# To get the optimal number of topics in our topics)
  dtm,
  topics = seq(2, 20, by = 2),
  control = list(seed = 1234)
)
```

``` r
FindTopicsNumber_plot(finetune_result)
```

![](05_LDA_files/figure-gfm/7.%20Plot%20the%20optimal%20LDA%20topics-1.png)<!-- -->

``` r
K <- 12
topicModel <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, 
                                                       verbose = 50, #Print Inter
                                                       seed = 1234))
```

    ## K = 12; V = 73; M = 766
    ## Sampling 500 iterations!
    ## Iteration 50 ...
    ## Iteration 100 ...
    ## Iteration 150 ...
    ## Iteration 200 ...
    ## Iteration 250 ...
    ## Iteration 300 ...
    ## Iteration 350 ...
    ## Iteration 400 ...
    ## Iteration 450 ...
    ## Iteration 500 ...
    ## Gibbs sampling completed!

- Let’s look at the posterior estimates.

``` r
LDA_Result <- posterior(topicModel)
attributes(LDA_Result)
```

    ## $names
    ## [1] "terms"  "topics"

``` r
# topics are probability distributions over the entire vocabulary
beta <- LDA_Result$terms   # get beta from results
dim(beta)                # K distributions over nTerms(dtm) terms
```

    ## [1] 12 73

``` r
# Row sums by topic should equal to 1
rowSums(beta)
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 
    ##  1  1  1  1  1  1  1  1  1  1  1  1

``` r
nDocs(dtm)
```

    ## [1] 766

``` r
# for every document we have a probability distribution of its contained topics
theta <- LDA_Result$topics # Probability of a document associated with a topic (12 topics. Randomly if I take document 50, what is the probability that it is talking about topic 3?)
dim(theta)
```

    ## [1] 766  12

``` r
# Sum of probabilities of each document 
rowSums(theta)[1:5] 
```

    ## 1 2 3 4 5 
    ## 1 1 1 1 1

``` r
# Terms distribution for the first 5 topics. 
term_distribution <- terms(topicModel, 10)
term_distribution[, 1:5]
```

    ##       Topic 1  Topic 2        Topic 3   Topic 4     Topic 5    
    ##  [1,] "one"    "noise"        "can"     "just"      "great"    
    ##  [2,] "case"   "price"        "use"     "work"      "sound"    
    ##  [3,] "right"  "cancellation" "hear"    "get"       "quality"  
    ##  [4,] "earbud" "cancelling"   "volume"  "don’t"     "buy"      
    ##  [5,] "use"    "earbuds"      "phone"   "app"       "better"   
    ##  [6,] "bud"    "well"         "app"     "well"      "recommend"
    ##  [7,] "left"   "amazing"      "overall" "stay"      "fit"      
    ##  [8,] "got"    "bought"       "set"     "different" "just"     
    ##  [9,] "charge" "like"         "using"   "better"    "much"     
    ## [10,] "easy"   "got"          "also"    "first"     "well"

``` r
# Concatenate the terms to give a good name for each topic. 
topic_names <- apply(terms(topicModel, 3), 2, 
                     FUN = paste, collapse = " ")
```

``` r
topic_sel <- 5 # select topic 5 to visualize 

# topic_sel <- grep('volume', topic_names)[1] # selecting a topic by a term 
# # select to 50 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top50terms <- sort(LDA_Result$terms[topic_sel,], decreasing=TRUE)[1:50]
words <- names(top50terms)

# extract the probabilites of each of the 50 terms
probabilities <- sort(LDA_Result$terms[topic_sel,], decreasing=TRUE)[1:50]
# visualize the terms as wordcloud
wordcloud(words, probabilities, random.order = FALSE)
```

![](05_LDA_files/figure-gfm/10.%20wordcloud%20of%20topics-1.png)<!-- -->

``` r
tidy_lda_output <- tidy(topicModel, matrix = "beta")
```
