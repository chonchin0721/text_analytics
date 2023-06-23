Classification using GLM models
================
Naga Vemprala
2023-06-19

#### Text Classification using GLM (Generalized Linear Model) models. In this markdown, we will review the below regression models.

1.  Ridge Regression
2.  Lasso Regression
3.  Elastic Net Regression

**Regularization** Linear regression algorithm works by selecting
coefficients for each independent variable that minimizes a loss
function. However, if the coefficients are large, they can lead to
over-fitting on the training dataset, and such a model will not
generalize well on the unseen test data. To overcome this shortcoming,
we’ll do regularization, which penalizes large coefficients.
Regularization algorithms helps to include penalty.

**Ridge Regression** Ridge regression is an extension of linear
regression where the loss function is modified to minimize the
complexity of the model. If used with the GLM, then it can be used for
classification problems. Loss function modification is done by adding a
penalty parameter (lambda) that is equivalent to the square of the
magnitude of the coefficients.

- Loss function = OLS + alpha \* summation (squared coefficient values)
- Ridge regression is also referred to as l2 regularization.
- GLM has a parameter called alpha. For ridge regression using GLM, the
  value of alpha is zero.

**Lasso Regression** Lasso regression, or the Least Absolute Shrinkage
and Selection Operator, is also a modification of linear regression. In
lasso, the loss function is modified to minimize the complexity of the
model by limiting the sum of the absolute values of the model
coefficients (also called the l1-norm).

The loss function for lasso regression can be expressed as below:

Loss function = OLS + alpha \* summation (absolute values of the
magnitude of the coefficients)

In the above function, alpha is the penalty parameter we need to select.
Using an l1-norm constraint forces some weight values to zero to allow
other coefficients to take non-zero values.

A **receiver operating characteristic curve, or ROC curve**, is a
graphical plot that illustrates the diagnostic ability of a binary
classifier system as its discrimination threshold is varied.

The ROC curve is the plot of the true positive rate (TPR) against the
false positive rate (FPR), at various threshold settings.

- Positive rate measures: Sensitivity, recall, hit rate, or true
  positive rate = (True Positive)/(Total Number of Possible Positives).
  Total number of possible positives means positives correctly detected
  as positive + Positive but detected as Negative (False Negatives)
- True negative rate: specificity, selectivity or true negative rate
  (TNR). (True Negative)/(Total Number of Possible Negatives).
  Denominator is True Negative + False Positive.

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.2.3

``` r
library(tm)
```

    ## Warning: package 'tm' was built under R version 4.2.3

    ## Loading required package: NLP

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

``` r
library(glmnet) # To fit GLM models 
```

    ## Warning: package 'glmnet' was built under R version 4.2.3

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-7

``` r
library(Matrix) # To create sparse matrix for GLM 
library(pROC) # to create ROC object for plotting ROC curves 
```

    ## Warning: package 'pROC' was built under R version 4.2.3

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

#### Use read_csv function of readr to read the dataset

``` r
news <- read_csv(paste0(getwd(), "/Input/fake_or_real_news.csv"))
```

    ## New names:
    ## Rows: 6335 Columns: 4
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (3): title, text, label dbl (1): ...1
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
# Takes tm corpus and a customWords character vector as input to 
text_pre_process <- function(corpus, customWords = c()) {
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, customWords)
  corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
  return(corpus)
}
```

``` r
corpus <- Corpus(VectorSource(news$text))
corpus <- text_pre_process(corpus)
```

    ## Warning in tm_map.SimpleCorpus(corpus, tolower): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(corpus, removeNumbers): transformation drops
    ## documents

    ## Warning in tm_map.SimpleCorpus(corpus, stripWhitespace): transformation drops
    ## documents

    ## Warning in tm_map.SimpleCorpus(corpus, removeWords, customWords):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(corpus, removeWords, stopwords(kind = "en")):
    ## transformation drops documents

``` r
news$text <- corpus$content 
```

#### It is better to split the dataset equally among the labels for the training and test datasets.(It might lead to data modeling issues if labels are imbalance)

``` r
set.seed(1234) 
train_test_vector <- sample(c(0,1), nrow(news), replace = T, prob = c(0.7, 0.3))
train_news <- news[train_test_vector == 0, ]
test_news <- news[train_test_vector == 1, ]
```

``` r
train_corpus <- Corpus(VectorSource(train_news$text))
train_dtm <- DocumentTermMatrix(train_corpus)
train_dtm <- as.matrix(train_dtm)
train_dtm <- Matrix(train_dtm, sparse = T)
train_dtm <- train_dtm[, sort(colnames(train_dtm))]
```

``` r
gc() #Will run out of memory soon if don't include
```

    ##           used  (Mb) gc trigger    (Mb)   max used    (Mb)
    ## Ncells 2241397 119.8    7008842   374.4    7008842   374.4
    ## Vcells 9260489  70.7 1322919291 10093.1 1384034591 10559.4

``` r
glm_model <- cv.glmnet(train_dtm, 
                       y = as.factor(train_news$label), 
                       alpha = 1, # LASSO regression, 0 for Ridge regression, Glmnet for 0-1 
                       family = "binomial", 
                       nfolds = 10, #Cross-Validation
                       type.measure = "class")
```

#### Glm model plot interpretations

- There are two vertical dotted lines on the plot.

1.  The left line carries a lambda value which minimizes the
    misclassification error (FP+FN)/(TP+TN+FP+FN).
2.  The second line represents the highest regularization value within
    one standard deviation of the minimal class error.

- Model based on the first line is more accurate with the best penalty
  value while the second line converges more with minimal input values.

``` r
plot(glm_model)
```

![](GLM_Classification_files/figure-gfm/plot%20the%20glm%20train%20model-1.png)<!-- -->

``` r
train_prediction <- predict(glm_model, train_dtm, type = "class", 
                            s = glm_model$lambda.1se ) # lambda.lse is the second model lambda
train_auc <- roc(as.integer(as.factor(train_news$label)),
                 as.integer(as.factor(train_prediction)))
```

    ## Setting levels: control = 1, case = 2

    ## Setting direction: controls < cases

``` r
print(train_auc)
```

    ## 
    ## Call:
    ## roc.default(response = as.integer(as.factor(train_news$label)),     predictor = as.integer(as.factor(train_prediction)))
    ## 
    ## Data: as.integer(as.factor(train_prediction)) in 2231 controls (as.integer(as.factor(train_news$label)) 1) < 2231 cases (as.integer(as.factor(train_news$label)) 2).
    ## Area under the curve: 0.968

``` r
plot(train_auc)
```

![](GLM_Classification_files/figure-gfm/make%20training%20set%20predictions-1.png)<!-- -->

``` r
table(as.integer(as.factor(train_news$label)), 
      as.integer(as.factor(train_prediction)))
```

    ##    
    ##        1    2
    ##   1 2213   18
    ##   2  125 2106

#### test dtm is required to make test predictions. However, the new words are possible in the unseen documents. As the prediction GLM would not understand the unseen terms, they should be dropped from the test dtm object.

``` r
test_corpus <- Corpus(VectorSource(test_news$text))
test_dtm <- DocumentTermMatrix(test_corpus)
terms <- colnames(train_dtm[ , which(!colnames(train_dtm) %in% colnames(test_dtm))])
test_matrix <- matrix(0, nrow = nrow(test_dtm), ncol = length(terms))
colnames(test_matrix) <- terms
rownames(test_matrix) <- rownames(test_dtm)
test_dtm <- as.DocumentTermMatrix(
  cbind(test_dtm[, which(colnames(test_dtm) %in% colnames(train_dtm))],
        test_matrix),
  weighting = weightTf
  )

# Converting the Matrix
test_dtm <- as.matrix(test_dtm)
test_dtm <- Matrix(test_dtm, sparse = T)
test_dtm <- test_dtm[, sort(colnames(test_dtm))]
```

``` r
test_prediction <- predict(glm_model, test_dtm, type = "class", 
                            s = glm_model$lambda.min) 
test_auc <- roc(as.integer(as.factor(test_news$label)),
                 as.integer(as.factor(test_prediction)))
```

    ## Setting levels: control = 1, case = 2

    ## Setting direction: controls < cases

``` r
print(test_auc)
```

    ## 
    ## Call:
    ## roc.default(response = as.integer(as.factor(test_news$label)),     predictor = as.integer(as.factor(test_prediction)))
    ## 
    ## Data: as.integer(as.factor(test_prediction)) in 933 controls (as.integer(as.factor(test_news$label)) 1) < 940 cases (as.integer(as.factor(test_news$label)) 2).
    ## Area under the curve: 0.8998

``` r
plot(train_auc, col = "blue", lty = 1)
plot(test_auc, add = TRUE, col = "red", lty = 2)
```

![](GLM_Classification_files/figure-gfm/make%20test%20set%20predictions-1.png)<!-- -->

``` r
table(as.integer(as.factor(test_news$label)),
                 as.integer(as.factor(test_prediction)))
```

    ##    
    ##       1   2
    ##   1 880  53
    ##   2 135 805

#### Finding the impactful words

``` r
glmnet_coefficients <- as.matrix(coef(glm_model, s = "lambda.min"))
glmnet_coefficients <- data.frame(words = row.names(glmnet_coefficients), 
                                  glmnet_coefficients = glmnet_coefficients[,1])
glmnet_coefficients <- glmnet_coefficients[order(
  glmnet_coefficients$glmnet_coefficients, decreasing = T), ]
glmnet_coefficients$words <- factor(glmnet_coefficients$words, 
                                    levels = unique(glmnet_coefficients$words))
summary(glmnet_coefficients$glmnet_coefficients)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -6.754466  0.000000  0.000000  0.001961  0.000000  6.759648

#### Check how many coefficients have value greater than 0

``` r
# Total number of coefficients. 
print(paste0("total words: ", length(glmnet_coefficients$glmnet_coefficients)))
```

    ## [1] "total words: 153119"

``` r
print(paste0("Impactful words: ", length(subset(glmnet_coefficients$glmnet_coefficients, 
              glmnet_coefficients$glmnet_coefficients > 0))))
```

    ## [1] "Impactful words: 1304"

#### Density plot of the coefficient values.

``` r
ggplot(data = glmnet_coefficients) + 
  geom_density(aes(x = glmnet_coefficients))
```

![](GLM_Classification_files/figure-gfm/glm%20coefficients%20density-1.png)<!-- -->

``` r
glmnet_coefficients <- glmnet_coefficients %>% 
  arrange(desc(abs(glmnet_coefficients)))

glmnet_coefficients <- head(glmnet_coefficients, 20)

# ggplot(glmnet_coefficients) +
#  geom_segment(aes(x = glmnet_coefficients, y = words, yend = words), xend = 0,
#               color = "blue") +
#  geom_point(aes(x= glmnet_coefficients, y = words, color = glmnet_coefficients), size = 2.5 )
```

**GLM Summary for classification** - Feature engineering: Create
additional inputs for the number of characters of words per document to
be used. - Tokenization: Using bi-grams and tri-grams could be
beneficial. - Alpha penalty: Adjust the penalty parameter to balance the
number of inputs and accuracy. - Prediction stacking and ensembling
modeling: Combine other models such as Lasso + SVM. - After creating one
final model, use it. With usage, the test data keeps increasing. Create
a process to label the data and retrain the model.
