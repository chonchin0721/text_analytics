Introduction_to_Neural_Network
================
Jessica She
2023-06-07

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
admissions <- read_csv(str_c(getwd(),"/Input/Student_Admissions.csv"))
```

    ## Rows: 500 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (9): Id, GRE, Toefl, UG_School_Rating, SOP_Rating, Recommendation_Streng...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Do not compare the apple to oranges 
# Normalization: x- mean(x)/sd(x) or Max-Min Normalization: (x - min(x))/(max(x)-min(x))

max_min_normalization <- function(X) {
  X <- (X - min(X))/(max(X) - min(X))
  X
}

# Normalize all the numeric input columns except ID and Admit Probability Column 
admissions_matrix <- apply(admissions[,c(-1,-9)],
                           2,
                           FUN = max_min_normalization)
```

``` r
head(admissions$GRE - min(admissions$GRE))/(max(admissions$GRE) - min(admissions$GRE))
```

    ## [1] 0.94 0.68 0.52 0.64 0.48 0.80

``` r
admissions$binary_output <- ifelse(admissions$Admit_Probability > 0.5, 1, 0)
```

``` r
# Any machine learning problem, the steps are common:
# Training dataset (use this to create a model) - 80% of data
# Test dataset (20% for testing)
# Validation (standard process)
# k-fold cross-validation (common validation approach in machine learning)
set.seed(1234)
training_rows <- sample(1:500, #Random row numbers of my input dataset size
                        400 #400 observations are part of training
                        )

train <- cbind(
  admissions_matrix[training_rows,],
  admissions[training_rows, "binary_output"])
```

``` r
testing_rows <- c()
for(i in 1:500) {
  if(i %in% training_rows) {
    next
  }
  testing_rows <- append(testing_rows, i)
}

test <- cbind(
  admissions_matrix[testing_rows,],
  admissions[testing_rows, "binary_output"])
```

``` r
# . all the possible/available input columns 
model <- neuralnet(binary_output ~ .,
                   data = train,
                   hidden = 1, 
                   linear.output = FALSE, 
                   err.fct = "ce"
                   )
```

``` r
# The Number of Iterations is the Steps
# The threshold of 0.01 means if it's less than <0.01 errors, the iterations will stop
plot(model)
```

``` r
#Make predictions 
test_outcome <- predict(model,
                        test[,c(-8)])

# Convert output prediction to binary
test_outcome <- ifelse(test_outcome >0.5,
                       1,0)

# The overall accuracy is 97%
table(test[, c(8)],
      test_outcome)
```

    ##    test_outcome
    ##      0  1
    ##   0  3  2
    ##   1  1 94

``` r
set.seed(1234)

train_one <- cbind(
  admissions_matrix[training_rows,],
  admissions[training_rows, "Admit_Probability"])

test_one <- cbind(
  admissions_matrix[testing_rows,],
  admissions[testing_rows, "Admit_Probability"])
```

``` r
model_one <- neuralnet(Admit_Probability ~ .,
  data = train_one,
  hidden = 1,
  linear.output = TRUE,
  err.fct = "sse"
  )
```

``` r
plot(model_one)
```

``` r
#Make predictions 
test_outcome_one <- predict(model,
                        test_one[,-8])

head(test_one[,8])
```

    ## [1] 0.92 0.65 0.75 0.68 0.50 0.52

``` r
# Look at MSE (to be low) 
# Mean Square Error 
# (True Value - Predicted Value)^2/Total number of observations. 
# The lower the MSE, the better the model

MSE <- sum((test_one[,8] - test_outcome_one)^2)/(100-1)
MSE
```

    ## [1] 0.05505242

``` r
# If run multiple regression and get a better MSE, then neural network is no good
model_lm <- glm(Admit_Probability ~ .,
  data = as.data.frame(train_one))
predict_lm <- predict(model_lm, as.data.frame(test_one[, -8]))
MSE <- sum((test[,8]- predict_lm)^2)/(100-1)
MSE
```

    ## [1] 0.09582987

``` r
summary(model_lm)
```

    ## 
    ## Call:
    ## glm(formula = Admit_Probability ~ ., data = as.data.frame(train_one))
    ## 
    ## Deviance Residuals: 
    ##       Min         1Q     Median         3Q        Max  
    ## -0.262700  -0.023794   0.008146   0.035028   0.163160  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             0.338770   0.010309  32.861  < 2e-16 ***
    ## GRE                     0.093503   0.029372   3.183 0.001572 ** 
    ## Toefl                   0.076938   0.028205   2.728 0.006663 ** 
    ## UG_School_Rating        0.021316   0.017319   1.231 0.219150    
    ## SOP_Rating              0.002305   0.020351   0.113 0.909868    
    ## Recommendation_Strength 0.068641   0.019341   3.549 0.000434 ***
    ## CGPA                    0.384512   0.034516  11.140  < 2e-16 ***
    ## Research_Exp            0.025257   0.007668   3.294 0.001078 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.00383515)
    ## 
    ##     Null deviance: 8.3668  on 399  degrees of freedom
    ## Residual deviance: 1.5034  on 392  degrees of freedom
    ## AIC: -1080.3
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
train_outcome_two <- predict(model_one,
                         train_one[,-8])

test_outcome_two <- predict(model_one, test_one[,-8])

test_MSE <- sum((test_one[,8] - test_outcome_two)^2)/(100)
train_MSE <- sum((train_one[,8] - train_outcome_two)^2)/(100)
print(paste(train_MSE, test_MSE))
```

    ## [1] "0.0150788557725705 0.00275714782455278"
