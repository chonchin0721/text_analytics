HW04_Neural_Networks
================
Jessica She
2023-06-11

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.2.3

``` r
library(neuralnet)
```

    ## Warning: package 'neuralnet' was built under R version 4.2.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:neuralnet':
    ## 
    ##     compute

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.2.3

``` r
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ purrr     1.0.1
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::compute() masks neuralnet::compute()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(textdata)
```

    ## Warning: package 'textdata' was built under R version 4.2.3

``` r
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 4.2.3

    ## ------------------------------------------------------------------------------
    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)
    ## ------------------------------------------------------------------------------
    ## 
    ## Attaching package: 'plyr'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(keras)
```

    ## Warning: package 'keras' was built under R version 4.2.3

    ## 
    ## Attaching package: 'keras'
    ## 
    ## The following object is masked from 'package:textdata':
    ## 
    ##     dataset_imdb

``` r
library(tensorflow)
```

    ## Warning: package 'tensorflow' was built under R version 4.2.3

``` r
# Get the current working directory
current_dir <- getwd()

# Create the file path
file_path <- file.path(current_dir, "Input", "bank-full.csv")

# Read the CSV file with ";" delimiter
bank_orig <- read.csv(file_path, sep = ";")
```

``` r
# Normalize data using min-max normalization to enhance the training process of neural network
min_max_normalization <- function(X) {
  X <- (X - min(X))/(max(X) - min(X))
  X}

# converting categorical data into numeric/integer
bank <- bank_orig %>% 
  mutate(
    job = as.integer(factor(job)),
    marital = as.integer(factor(marital)),
    education = as.integer(factor(education)),
    contact = as.integer(factor(contact)),
    poutcome = as.integer(factor(poutcome)),
    month = recode(month, 
                   "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
                   "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12),
    default = ifelse(default=="no", 0, 1),
    housing = ifelse(housing =="no", 0,1),
    loan = ifelse(loan =="no", 0,1),
    y = ifelse(y == "no", 0, 1),
    age = min_max_normalization(age),
    balance = min_max_normalization(balance),
    duration = min_max_normalization(duration),
    day = min_max_normalization(day)
  )
```

``` r
# Setting seed to make example reproducible
set.seed(1234)


# Use 70% of the dataset as the training set and the remaining 30% as the testing set
train_indices <- sample(c(TRUE, FALSE), nrow(bank), replace = TRUE, prob = c(0.7, 0.3))
train_data <- bank[train_indices, ]
test_data <- bank[!train_indices, ]

# Extract the predictors from the train data 
train_feature <- as.matrix(train_data[, -17])

# Extract the target variable (y) from the train data
train_target <- as.matrix(train_data[, 17])

# Extract the predictors from the test data 
test_feature <- as.matrix(test_data[, -17])

# Extract the target variable (y) from the test data
test_target <- as.matrix(test_data[, 17])
```

``` r
# Define the model
set.seed(1234)
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = dim(train_data)[2] - 1) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid") 

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Perform model fit to compute optimal weights and accuracy
set.seed(1234)
model_fit <- model %>% fit(
  train_feature ,                    # Input features (all predictors except the target variable)
  train_target,                      # Target variable (y)
  validation_split = 0.2,            # 20% of the training data will be used for validation
  epochs = 10,                       # Number of training epochs (iterations)
  batch_size = 32                    
)
```

``` r
predicted <- model %>%
    predict(train_feature)

binary_prediction <- ifelse(predicted >= 0.5, 1, 0)

# Create a confusion matrix using the binary predictions and the actual target variable
confusion_matrix_train <- as.data.frame(table(train_target, binary_prediction))

# Print the confusion matrix
confusion_matrix_train
```

    ##   train_target binary_prediction  Freq
    ## 1            0                 0 27646
    ## 2            1                 0  3287
    ## 3            0                 1   264
    ## 4            1                 1   446

``` r
# Calculate accuracy
accuracy_train <- sum(
    confusion_matrix_train[confusion_matrix_train[,"train_target"] == confusion_matrix_train[,"binary_prediction"], c("Freq")])/sum(confusion_matrix_train[,"Freq"])
accuracy_train
```

    ## [1] 0.8877793

``` r
# Evaluate the model on the test data
metrics <- model %>% evaluate(
  test_feature,   # Input features (all predictors except the target variable)
  test_target)    # Target variable (y)


# Print the evaluation metrics
print(metrics)
```

    ##      loss  accuracy 
    ## 0.2782286 0.8901828

``` r
# Make predictions and create a confusion matrix
predicted_target <- model %>%
  predict(test_feature)

binary_predictions <- ifelse(predicted_target >= 0.5, 1, 0)

# Create a confusion matrix using the binary predictions and the actual target variable
confusion_matrix <- as.data.frame(table(test_target, binary_predictions))

# Print the confusion matrix
confusion_matrix
```

    ##   test_target binary_predictions  Freq
    ## 1           0                  0 11916
    ## 2           1                  0  1394
    ## 3           0                  1    96
    ## 4           1                  1   162

``` r
# Calculate accuracy
accuracy <- sum(
  confusion_matrix[confusion_matrix[,"test_target"] == confusion_matrix[,"binary_predictions"], c("Freq")])/sum(confusion_matrix[,"Freq"])
accuracy
```

    ## [1] 0.8901828
