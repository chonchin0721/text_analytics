Clustering and Topic Models Homework
================
Jessica She
2023-06-09

#### K-Means Clustering and LDA Topic Models are explained in the file

``` r
library(tidyverse)
library(skmeans) # For running Spherical K-Means clustering 
library(factoextra)
library(fpc) # Plot clusters using a DTM matrix. Base plot style plotting 
library(readxl)
library(tm)
library(stringr) 
library(tidytext)
library(textcat)
library(textmineR)
library(cluster)
library(SnowballC)
library(dplyr)
library(wordcloud)
library(topicmodels)
library(ldatuning)
library(modeltools)
library(clue)
```

#### **Q1.** Plot the exploratory analysis graph using the iris dataset, Sepal.Length and Sepal.Width

**Output should resemble figure the one given in the markdown output**

``` r
# There are three different types of flowers in the iris dataset. Perform K-means clustering to fetch these three clusters clusters based on the rest of the properties. 

# Load the iris dataset
data(iris)

# View the first 5 rows of the iris dataset
head(iris, 5)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa

``` r
# Plotting Iris Dataset 
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Sepal Width", title = "Iris Scatterplot")
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/2.%20Explore%20iris%20dataset-1.png)<!-- -->

``` r
# Use the kmeans function from stats package to create the clusters. 
# Use all the numeric fields

# Select properties for clustering
iris_properties <- iris[, 1:4]

# set seed for reproducibility (generate the same squence of random numbers)
set.seed(1234) 
kmeans_model <- kmeans(iris_properties, center = 3)

# retrive the clusters 
cluster_labels <- kmeans_model$cluster

# plotting to see the count of elements mapped to each cluster
barplot(kmeans_model$size)
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/3.%20Perform%20K-Means%20clustering%20on%20iris%20dataset%20to%20extract%20three%20clusters-1.png)<!-- -->

#### **Q2.** Perform K-Means clustering on iris dataset to extract three clusters. Create a clusters plot using fviz_cluster

**Output should resemble figure the one given in the markdown output**

``` r
fviz_cluster(kmeans_model, data = iris_properties)
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/Q2-1.png)<!-- -->

*Complete the below questions* - Add the clusters to the iris dataset,
capture their frequency, and validate them against the true flower
types. The output should look something like this (the numbers may not
match exactly). Please explain how you interpreted the cluster outputs.
Did the K-Means algorithm properly cluster the output?):

``` r
# Adding clusters to the iris dataset
iris$Cluster <- cluster_labels

# Capturing freqeuncy 
cluster_freq <- table(cluster_labels)

#
validation_table <- table(iris$Species, iris$Cluster)
validation_table
```

    ##             
    ##               1  2  3
    ##   setosa     50  0  0
    ##   versicolor  0 48  2
    ##   virginica   0 14 36

From the validation_table, we can see that there are 50 observations of
“setosa” assigned to cluster one, and no observations from the two other
species, meaning the K-Means algorithm correctly assigned all the setosa
species to this cluster. However, in cluster 2, there are 48
observations of versicolor and 14 virginica, suggesting the algorithim
assigned some virginica species into Cluster 2 incorrectly. Lastly, for
cluster 3, it contains 36 observations of virginica, and 2 observations
of versicolor, meaning the 2 versicolor are in the wrong cluster.

*Complete the below questions using the misinformation dataset* 1. Read
the misinformation dataset. 2. Text pre-processing steps: a. Use only
English tweets b. Remove punctuation characters, special characters,
numbers, strip whitespaces and remove stopwords c. Additionally, stem
words (Use: function stemDocument. Please look at the help for more
details) 2. Use Spherical K-Means clustering to create 10 clusters and
validate a few tweets from one of the cluster and provide
interpretations if the clusters are meaningful. 3. Plot the clusters
using plotcluster() function. 4. Also plot the cluster frequencies as
bar plot.

``` r
# read tweet data
misinformation_data <- read_csv(str_c(getwd(), "/Input/Misinformation_Tweets.csv"))
```

``` r
# Text pre-processing steps
# Use only English tweets
misinformation_data <- subset(misinformation_data, textcat(misinformation_data$TWEET_TEXT) == "english")

# # Select only the TWEET_TEXT C
# misinformation_data <- misinformation_data %>%
#   select(TWEET_TEXT)

# Remove punctuation characters, special characters, numbers, strip whitespaces, and remove stopwords

text_pre_process <- function(corpus) {
  # Remove line breaks
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\n", replacement = " ")
  
  # Remove special characters
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:][:space:]]", replacement = "")
  
  # Stemming
  corpus <- tm_map(corpus, stemDocument)
  
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
  
  
  return(corpus)
}

corpus <- Corpus(VectorSource(misinformation_data$TWEET_TEXT))
corpus <- text_pre_process(corpus)
misinformation_data_processed <- corpus$content
misinformation_data_processed <- data.frame(tweets = corpus$content, harm = misinformation_data$Harm)
misinformation_data_processed$tweets <- str_squish(misinformation_data_processed$tweets)
```

``` r
# Extract corpus
corpus <- Corpus(VectorSource(misinformation_data_processed$tweets))

# Create dtm using the corpus
dtm <- DocumentTermMatrix(corpus,
                          control = list(weighting =
                                         function(x)
                                         weightTfIdf(x, normalize =
                                                     FALSE))
                          )

# Use k-mean clustering to create 10 clusters
set.seed(12345)
skmeans_model <- skmeans(dtm,
                         10,
                         m =1.2,
                         control = list(nruns = 10, verbose = FALSE))

# Validate a few tweets, validating cluster one
cluster_assignments <- skmeans_model$cluster
cluster_no <- 1
cluster_tweets <- misinformation_data_processed$tweets[cluster_assignments == cluster_no]
head(cluster_tweets,5)
```

    ## [1] "one way prevent catch coronavirus"                             
    ## [2] "antibiot effect prevent treat coronavirus"                     
    ## [3] "general tlr use advic behav alreadi infect prevent anyon catch"
    ## [4] "alcohol general help catch coronavirus"                        
    ## [5] "record drive slow prevent catch coronavirus"

``` r
# Validate Cluster two
cluster_no_two <- 2
cluster_tweets_two <- misinformation_data_processed$tweets[cluster_assignments == cluster_no_two]
head(cluster_tweets_two,5)
```

    ## [1] "effect prevent spread novel"                                
    ## [2] "govern stick messag evid worn general public prevent spread"
    ## [3] "telosian prevent catch spread"                              
    ## [4] "prevent catch amp spread"                                   
    ## [5] "habit chang prevent spread catch coronavirus"

``` r
# Generating Word Cloud to look at different cluster and topics
cluster_prototypes <- cl_prototypes(skmeans_model)
cluster_prototypes <- t(cluster_prototypes)
comparison.cloud(cluster_prototypes, max.words = 100)
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/Use%20Spherical%20K-Means%20clustering%20to%20create%2010%20clusters%20and%20validate%20a%20few%20tweets-1.png)<!-- -->

``` r
# Extracting the top words
top_words <- sort(cluster_prototypes[,5], decreasing = T)[1:10]
top_words <- as.data.frame(top_words)
top_words <- data.frame(cluster1_words = row.names(top_words))
top_words
```

    ##    cluster1_words
    ## 1            year
    ## 2             flu
    ## 3             die
    ## 4           death
    ## 5            last
    ## 6        american
    ## 7            noth
    ## 8          averag
    ## 9          common
    ## 10           case

From the cluster and word cloud, we can see that the clustering has
meaning. There’s a category pertains to wash and sanitize. There’s a
different category on wearing mask.

``` r
# # plot clusters using plotclusters()
# plotcluster(dist(dtm, method = "euclidean"),
             # cluster_assignments)

# plot cluster frequencies as bar plot
barplot(table(cluster_assignments))
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/Use%20plotting%20bar%20graph-1.png)<!-- -->

*Complete the below questions using the misinformation dataset* 1. Using
the misinformation data, create a topic model and print the topics
summary. 2. Create a dataframe of tweets and name it as
misinformationTweets. 3. misinformationTweets contains both the highly
harmful and less harmful tweets 4. Use the CreateDtm function of
textmineR package to create a dtm matrix. Make sure you pass the
doc_names to identify each tweet as a single document. For this use,
doc_names = rownames(\<<misinformation dataset>\>). Also, use the
ngram_window = c(1, 2) option while creating DTM 5. Fit the LDA model
model for the High-Harm tweets. Generate 15 topics and use 500
iterations while fitting the LDA topic model. Name the topic model you
create as lda_misinformation_high. If you pass the argument calc_r2 =
TRUE to the FitLdaModel, then you would be able to create the r2 value
for the topic model. Print the r2 of the LDA model. You can print it
usimg
\<<model>\>$r2 6. Plot the coherence histogram. Coherence value would have been calculated and stored in the lda_misinformation_high$coherence  
7. Capture the top 15 terms for each topic into top_terms member of lda
model object. Refer to the help documentation of the GetTopTerms
function for more details. 8. Assigning the labels for each topic using
the set of topic keywords is the most important task of topic modeling.
The textmineR package provides a simple implementation of assigning the
topic lables using the best N-grams with the highest theta value. Phi is
the word probability distribution per topic and theta is the topic-level
probability distribution over the documents.

Here in the below code, if the probability of a topic related for a
particular tweet (one tweet out of all the documents) is less than 5% it
is considered as not so useful and is given a zero probability.
assignments variable/member of the lda_misinformation_high object holds
the probable topic that the tweet is related to (probability using
theta) within the 15 topics and assigns the corresponding labels.

``` r
# Create a dataframe of tweets and name it as misinformationTweets
misinformationTweets <- misinformation_data_processed
```

``` r
data_high_harm <- misinformationTweets %>% filter(harm == 'High_Harm')
data_low_harm <- misinformationTweets %>% filter(harm == 'Low_Harm')
```

``` r
# create dtm
set.seed(123)
dtm_high_harm <- CreateDtm(
  doc_vec = data_high_harm$tweets,
  doc_names = rownames(data_high_harm),
  ngram_window = c(1, 2),
  verbose = FALSE)
```

``` r
set.seed(123)
lda_misinformation_high <- FitLdaModel(dtm = dtm_high_harm,
                         k = 15, # number of topic
                         iterations = 500,
                         calc_r2 = T)

# For overall goodness of fit 
lda_misinformation_high$r2
```

    ## [1] 0.1938533

The r2 value of .19 suggests the LDA model explains 19.4% of the
variance in the text data. Topic generated by the model capture some
patterns and structure in the documents, but still some amount of
unexplained variance, and the model may not fully capture all the
underlying topics in the misinformationTweets data set.

``` r
lda_misinformation_high$top_terms <- GetTopTerms(phi = lda_misinformation_high$phi,M = 15)
data.frame(lda_misinformation_high$top_terms)
```

    ##              t_1         t_2              t_3               t_4
    ## 1           show        nose             hand              hand
    ## 2         infect       cough             wash        hand_sanit
    ## 3          studi       runni        wash_hand             sanit
    ## 4        patient        cold            sanit             fight
    ## 5           rate  runni_nose      coronavirus       coronavirus
    ## 6          covid      common       hand_sanit            effect
    ## 7     studi_show         dri        hand_wash              make
    ## 8    infect_rate   dri_cough          prevent              made
    ## 9         result common_cold           proper              york
    ## 10          test     symptom            peopl           homemad
    ## 11  covid_infect       fever             yall       sanit_fight
    ## 12         posit   pneumonia          protect      homemad_hand
    ## 13        report cough_runni coronavirus_wash             state
    ## 14 covid_patient coronavirus             soap fight_coronavirus
    ## 15         reduc      sputum      hand_proper            prison
    ##                   t_5           t_6            t_7         t_8          t_9
    ## 1              affect          dead           hand        mask       affect
    ## 2         coronavirus          cure          sanit        wear        peopl
    ## 3  affect_coronavirus        pandem     hand_sanit   wear_mask        littl
    ## 4               covid        miracl          virus      infect      countri
    ## 5              pandem         treat       bacteria        care       health
    ## 6        covid_affect   miracl_cure    coronavirus      person        world
    ## 7               place          tank           kill     protect affect_peopl
    ## 8                busi          fish           wash coronavirus       person
    ## 9        affect_covid     fish_tank    antibacteri        sick        brain
    ## 10           industri bleach_miracl      wash_hand     suspect       realli
    ## 11                end          tout           noth   face_mask       corona
    ## 12             accord   dead_pandem           work        face         stay
    ## 13             higher    tout_drink        protect     prevent     outbreak
    ## 14        mortal_rate         solut bacteria_virus     healthi affect_brain
    ## 15             mortal    conspiraci     sanit_kill      public        organ
    ##               t_10         t_11             t_12                t_13       t_14
    ## 1             mask         year             dead               drink        flu
    ## 2           public          die              flu              bleach       dead
    ## 3          prevent          flu            covid        drink_bleach      death
    ## 4          general       common         dead_flu         coronavirus      peopl
    ## 5           effect     american         flu_dead                cure        die
    ## 6   general_public   common_flu       covid_dead               qanon       time
    ## 7            catch         case       dead_covid               peopl       year
    ## 8           spread        death      coronavirus    cure_coronavirus   dead_flu
    ## 9      mask_effect         noth dead_coronavirus              corona       rate
    ## 10  effect_prevent      confirm           season         bleach_cure  peopl_die
    ## 11 prevent_general       averag           vaccin          conspiraci  time_dead
    ## 12    public_catch american_die          contagi                ward death_rate
    ## 13            face         shut             test   coronavirus_drink      covid
    ## 14  prevent_spread  coronavirus             wors conspiraci_theorist    million
    ## 15     coronavirus confirm_case           realli            theorist  flu_death
    ##            t_15
    ## 1         sanit
    ## 2    hand_sanit
    ## 3          hand
    ## 4         vodka
    ## 5          tito
    ## 6    tito_vodka
    ## 7          make
    ## 8         peopl
    ## 9       homemad
    ## 10   vodka_hand
    ## 11 homemad_hand
    ## 12  coronavirus
    ## 13    make_hand
    ## 14      prevent
    ## 15      alcohol

``` r
lda_misinformation_high$coherence
```

    ##          t_1          t_2          t_3          t_4          t_5          t_6 
    ##  0.301609926  0.694398813  0.253958024  0.213309007 -0.011302083  0.018468997 
    ##          t_7          t_8          t_9         t_10         t_11         t_12 
    ##  0.267541624  0.298475448 -0.006459139  0.300401801  0.410767571  0.134065776 
    ##         t_13         t_14         t_15 
    ##  0.372825945  0.150049286  0.340361350

``` r
hist(lda_misinformation_high$coherence, main = "Coherence Histogram", xlab = "Coherence Value")
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/Plotting%20Coherence%20Histogram-1.png)<!-- -->

``` r
lda_misinformation_high$prevalence <- colSums(lda_misinformation_high$theta)/sum(lda_misinformation_high$theta)*100
lda_misinformation_high$prevalence
```

    ##       t_1       t_2       t_3       t_4       t_5       t_6       t_7       t_8 
    ##  4.685687  5.260752  8.699369  5.568841  8.435999  5.426468  7.054163  7.343966 
    ##       t_9      t_10      t_11      t_12      t_13      t_14      t_15 
    ##  6.640477  5.664258  4.780979 10.599817  7.362510  7.164083  5.312633

``` r
tmp <- lda_misinformation_high
tmp$assignments <- tmp$theta 
tmp$assignments[tmp$assignments < 0.05 ] <- 0
tmp$labels <- LabelTopics(
    assignments = tmp$assignments, 
    dtm = dtm_high_harm, 
    M = 2)
tmp$assignments <- 
    tmp$assignments / rowSums(tmp$assignments)
tmp$assignments[ is.na(tmp$assignments) ] <- 0
head(tmp$assignments)
```

    ##   t_1 t_2 t_3 t_4 t_5 t_6 t_7       t_8 t_9      t_10      t_11 t_12 t_13
    ## 1   0   0   0   0   0   0   0 0.0000000   0 1.0000000 0.0000000    0    0
    ## 2   0   0   0   0   0   0   0 0.8804348   0 0.0000000 0.1195652    0    0
    ## 3   0   0   0   0   0   0   0 0.6801802   0 0.3198198 0.0000000    0    0
    ## 4   0   0   0   0   0   0   0 0.8472222   0 0.1527778 0.0000000    0    0
    ## 5   0   0   0   0   0   0   0 0.0000000   0 1.0000000 0.0000000    0    0
    ## 6   0   0   0   0   0   0   0 0.4513274   0 0.4513274 0.0000000    0    0
    ##         t_14 t_15
    ## 1 0.00000000    0
    ## 2 0.00000000    0
    ## 3 0.00000000    0
    ## 4 0.00000000    0
    ## 5 0.00000000    0
    ## 6 0.09734513    0

``` r
tmp$num_docs <- colSums(tmp$assignments > 0)
```

9.  Cluster topics together in a dendrogram. Calculate the Hellinger
    distance using CalcHellingerDist method and using the phi vectors
    (or the word probabilities of the topics). Perform Hierarchical
    clustering using hclust method and the linguistic distance.
    Calculated using the CalcHellingerDist method. Use the “ward.D”
    agglomerative clustering technique. Limit the number of clusters to
    10 instead of 15 based on the hclust you have calculated earlier.

``` r
# Calculate Hellinger distance
lda_misinformation_high$linguistic <- CalcHellingerDist(lda_misinformation_high$phi)

# Perform hierarchical clustering
hclust_result <- hclust(as.dist(lda_misinformation_high$linguistic), method = "ward.D")

# Limit the number of clusters to 10
clusters <- cutree(hclust_result, k = 10)

# Assign labels to the clusters
cluster_labels <- paste(clusters, lda_misinformation_high$labels[, 1])

# Plot dendrogram
plot(hclust_result, labels = cluster_labels)
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/cluster%20topics%20together%20in%20dendrogram-1.png)<!-- -->

10. Create labels for the clusters. The code sample above created two
    set of labels using the LabelTopics function. You could combine
    these two labels into one as shown below code:

``` r
lda_misinformation_high$hclust$labels <- 
    paste(lda_misinformation_high$hclust$labels, lda_misinformation_high$labels[ , 1])
```

11. Plot the hclust of your model using plot function

``` r
# Perform hierarchical clustering
hclust_result <- hclust(as.dist(lda_misinformation_high$linguistic), method = "ward.D")

# Plot dendrogram
plot(hclust_result)
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/plotting%20hclust-1.png)<!-- -->
The Dendrogram above similarity between topics. We can see topic 3 and
topic 7 have greater similarity. Topic 4 and Topic 14 are similar and so
forth.

12. Finally, make a summary table

``` r
lda_misinformation_high$summary <- data.frame(
  topic = rownames(lda_misinformation_high$phi),
  coherence = round(lda_misinformation_high$coherence, 3),
  prevalence = round(lda_misinformation_high$prevalence, 3),
  top_terms = apply(lda_misinformation_high$top_terms, 2, paste, collapse = ", ")
)

mod_high <- lda_misinformation_high$summary %>%
  `rownames<-`(NULL)

mod_high
```

    ##    topic coherence prevalence
    ## 1    t_1     0.302      4.686
    ## 2    t_2     0.694      5.261
    ## 3    t_3     0.254      8.699
    ## 4    t_4     0.213      5.569
    ## 5    t_5    -0.011      8.436
    ## 6    t_6     0.018      5.426
    ## 7    t_7     0.268      7.054
    ## 8    t_8     0.298      7.344
    ## 9    t_9    -0.006      6.640
    ## 10  t_10     0.300      5.664
    ## 11  t_11     0.411      4.781
    ## 12  t_12     0.134     10.600
    ## 13  t_13     0.373      7.363
    ## 14  t_14     0.150      7.164
    ## 15  t_15     0.340      5.313
    ##                                                                                                                                                                  top_terms
    ## 1                                      show, infect, studi, patient, rate, covid, studi_show, infect_rate, result, test, covid_infect, posit, report, covid_patient, reduc
    ## 2                                   nose, cough, runni, cold, runni_nose, common, dri, dri_cough, common_cold, symptom, fever, pneumonia, cough_runni, coronavirus, sputum
    ## 3                             hand, wash, wash_hand, sanit, coronavirus, hand_sanit, hand_wash, prevent, proper, peopl, yall, protect, coronavirus_wash, soap, hand_proper
    ## 4                              hand, hand_sanit, sanit, fight, coronavirus, effect, make, made, york, homemad, sanit_fight, homemad_hand, state, fight_coronavirus, prison
    ## 5                      affect, coronavirus, affect_coronavirus, covid, pandem, covid_affect, place, busi, affect_covid, industri, end, accord, higher, mortal_rate, mortal
    ## 6                                   dead, cure, pandem, miracl, treat, miracl_cure, tank, fish, fish_tank, bleach_miracl, tout, dead_pandem, tout_drink, solut, conspiraci
    ## 7                               hand, sanit, hand_sanit, virus, bacteria, coronavirus, kill, wash, antibacteri, wash_hand, noth, work, protect, bacteria_virus, sanit_kill
    ## 8                                              mask, wear, wear_mask, infect, care, person, protect, coronavirus, sick, suspect, face_mask, face, prevent, healthi, public
    ## 9                                           affect, peopl, littl, countri, health, world, affect_peopl, person, brain, realli, corona, stay, outbreak, affect_brain, organ
    ## 10    mask, public, prevent, general, effect, general_public, catch, spread, mask_effect, effect_prevent, prevent_general, public_catch, face, prevent_spread, coronavirus
    ## 11                                         year, die, flu, common, american, common_flu, case, death, noth, confirm, averag, american_die, shut, coronavirus, confirm_case
    ## 12                                dead, flu, covid, dead_flu, flu_dead, covid_dead, dead_covid, coronavirus, dead_coronavirus, season, vaccin, contagi, test, wors, realli
    ## 13 drink, bleach, drink_bleach, coronavirus, cure, qanon, peopl, cure_coronavirus, corona, bleach_cure, conspiraci, ward, coronavirus_drink, conspiraci_theorist, theorist
    ## 14                                                   flu, dead, death, peopl, die, time, year, dead_flu, rate, peopl_die, time_dead, death_rate, covid, million, flu_death
    ## 15                              sanit, hand_sanit, hand, vodka, tito, tito_vodka, make, peopl, homemad, vodka_hand, homemad_hand, coronavirus, make_hand, prevent, alcohol

From looking at the table summary, we can see that the highest coherence
score (t_2) with 0.694, contains words like nose, cough, cold, fever,
pneuomnia. It seems to be symptoms. It has high coherence score, and
moderate prevalence, meaning they are somewhat used in other review.

``` r
mod_high %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Tweets Reviews",
       x = "Topics", y = "Value")
```

![](05_Clustering_n_LDA_Homework_BoilerPlate_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
