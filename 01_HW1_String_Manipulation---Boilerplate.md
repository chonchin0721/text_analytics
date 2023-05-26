Delta Customer Service Tweets Processing
================
Jessica She
2023-05-20

## Text Mining approach

The Bag of Words approach is a text mining technique used on text data
that unavoidably lacks a solid syntactic foundation. Social media data
is one such instance of this kind of text data. On the other hand, text
data with pure syntactic structure, as that found in legal texts, can be
analyzed using syntactic parsing. Some of the bag-of-words-related
analysis in this markdown file is done with the help of R
string-specific libraries like “stringr,” “tm,” and base functions.

To work on the stringr functions, refer to the stringr cheatsheet
uploaded to the moodle page.

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.2.3

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

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

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
library(qdap)
```

    ## Warning: package 'qdap' was built under R version 4.2.3

    ## Loading required package: qdapDictionaries

    ## Loading required package: qdapRegex

    ## Warning: package 'qdapRegex' was built under R version 4.2.3

    ## 
    ## Attaching package: 'qdapRegex'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     explain

    ## Loading required package: qdapTools

    ## Warning: package 'qdapTools' was built under R version 4.2.3

    ## 
    ## Attaching package: 'qdapTools'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     id

    ## Loading required package: RColorBrewer

    ## 
    ## Attaching package: 'qdap'

    ## The following objects are masked from 'package:tm':
    ## 
    ##     as.DocumentTermMatrix, as.TermDocumentMatrix

    ## The following object is masked from 'package:NLP':
    ## 
    ##     ngrams

    ## The following objects are masked from 'package:base':
    ## 
    ##     Filter, proportions

## Including Plots

``` r
library(readr)
tweets_data <- read_csv(str_c(getwd(),"/Input/HW1 - Input_Tweets.csv"))
```

    ## Rows: 1377 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): weekday, month, text
    ## dbl (2): date, year
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tweets_data
```

    ## # A tibble: 1,377 × 5
    ##    weekday month  date  year text                                               
    ##    <chr>   <chr> <dbl> <dbl> <chr>                                              
    ##  1 Thu     Oct       1  2015 @mjdout I know that can be frustrating..we hope to…
    ##  2 Thu     Oct       1  2015 @rmarkerm Terribly sorry for the inconvenience. If…
    ##  3 Thu     Oct       1  2015 @checho85  I can check, pls follow and DM your con…
    ##  4 Thu     Oct       1  2015 @nealaa ...Alerts, pls check here: http://t.co/0jl…
    ##  5 Thu     Oct       1  2015 @nealaa ...advisory has only been issued for the B…
    ##  6 Thu     Oct       1  2015 @nealaa Hi. Our meteorologist team is aware of Hur…
    ##  7 Thu     Oct       1  2015 @BigGucciQueen  This is your direct dial number + …
    ##  8 Thu     Oct       1  2015 @marxypoo ...for any inconvenience.  *JH 3/3       
    ##  9 Thu     Oct       1  2015 @marxypoo ...the system &amp; when you are ready t…
    ## 10 Thu     Oct       1  2015 @marxypoo Hi Marx. This is a known issue &amp; our…
    ## # ℹ 1,367 more rows

``` r
tweets_data <- tweets_data %>%
  select(weekday, month, date, year, text) %>%
  mutate(tweets_length = str_count(tweets_data$text))
tweets_data
```

    ## # A tibble: 1,377 × 6
    ##    weekday month  date  year text                                  tweets_length
    ##    <chr>   <chr> <dbl> <dbl> <chr>                                         <int>
    ##  1 Thu     Oct       1  2015 @mjdout I know that can be frustrati…           119
    ##  2 Thu     Oct       1  2015 @rmarkerm Terribly sorry for the inc…           110
    ##  3 Thu     Oct       1  2015 @checho85  I can check, pls follow a…            78
    ##  4 Thu     Oct       1  2015 @nealaa ...Alerts, pls check here: h…            65
    ##  5 Thu     Oct       1  2015 @nealaa ...advisory has only been is…           137
    ##  6 Thu     Oct       1  2015 @nealaa Hi. Our meteorologist team i…           142
    ##  7 Thu     Oct       1  2015 @BigGucciQueen  This is your direct …            75
    ##  8 Thu     Oct       1  2015 @marxypoo ...for any inconvenience. …            44
    ##  9 Thu     Oct       1  2015 @marxypoo ...the system &amp; when y…           140
    ## 10 Thu     Oct       1  2015 @marxypoo Hi Marx. This is a known i…           141
    ## # ℹ 1,367 more rows

``` r
# Capturing the average length
avg_length <- mean(tweets_data$tweets_length)
avg_length
```

    ## [1] 92.14379

``` r
# What percentage of short tweets (tweets less than 50 characters in length) are there in the dataset? Use nchar to answer this question. 
sum(nchar(tweets_data$text) < 50)/nrow(tweets_data) * 100
```

    ## [1] 13.21714

``` r
# Create a new dataset tweets_data_subset without short tweets. Use subset() function from base R installation. 
tweets_data2 <- subset(tweets_data, nchar(as.character(text)) >= 50)
tweets_data2
```

    ## # A tibble: 1,195 × 6
    ##    weekday month  date  year text                                  tweets_length
    ##    <chr>   <chr> <dbl> <dbl> <chr>                                         <int>
    ##  1 Thu     Oct       1  2015 @mjdout I know that can be frustrati…           119
    ##  2 Thu     Oct       1  2015 @rmarkerm Terribly sorry for the inc…           110
    ##  3 Thu     Oct       1  2015 @checho85  I can check, pls follow a…            78
    ##  4 Thu     Oct       1  2015 @nealaa ...Alerts, pls check here: h…            65
    ##  5 Thu     Oct       1  2015 @nealaa ...advisory has only been is…           137
    ##  6 Thu     Oct       1  2015 @nealaa Hi. Our meteorologist team i…           142
    ##  7 Thu     Oct       1  2015 @BigGucciQueen  This is your direct …            75
    ##  8 Thu     Oct       1  2015 @marxypoo ...the system &amp; when y…           140
    ##  9 Thu     Oct       1  2015 @marxypoo Hi Marx. This is a known i…           141
    ## 10 Thu     Oct       1  2015 @BigGucciQueen  What country are you…            54
    ## # ℹ 1,185 more rows

``` r
tweets_data$text <- str_to_lower(tweets_data$text)
```

``` r
str_detect(tweets_data$text, "pls") %>% sum()
```

    ## [1] 152

``` r
library(qdap)
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
patterns <-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
replacements <-seq(1:12)
tweets_data$month_numeric <- mgsub(patterns,replacements,tweets_data$month)

patterns_weekday <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
replacements_weekday <- seq(1:7)
tweets_data$weekday_numeric <- mgsub(patterns_weekday,replacements_weekday,tweets_data$weekday)

tweets_data$response_date <- str_c(tweets_data$month_numeric, tweets_data$date, tweets_data$year, sep = "/")
tweets_data$response_date <- as.Date(tweets_data$response_date, format = "%m/%d/%Y")
tweets_data
```

    ## # A tibble: 1,377 × 9
    ##    weekday month  date  year text    tweets_length month_numeric weekday_numeric
    ##    <chr>   <chr> <dbl> <dbl> <chr>           <int> <chr>         <chr>          
    ##  1 Thu     Oct       1  2015 @mjdou…           119 10            4              
    ##  2 Thu     Oct       1  2015 @rmark…           110 10            4              
    ##  3 Thu     Oct       1  2015 @chech…            78 10            4              
    ##  4 Thu     Oct       1  2015 @neala…            65 10            4              
    ##  5 Thu     Oct       1  2015 @neala…           137 10            4              
    ##  6 Thu     Oct       1  2015 @neala…           142 10            4              
    ##  7 Thu     Oct       1  2015 @biggu…            75 10            4              
    ##  8 Thu     Oct       1  2015 @marxy…            44 10            4              
    ##  9 Thu     Oct       1  2015 @marxy…           140 10            4              
    ## 10 Thu     Oct       1  2015 @marxy…           141 10            4              
    ## # ℹ 1,367 more rows
    ## # ℹ 1 more variable: response_date <date>

``` r
ymd(as.Date("2023-05-19"))
```

    ## [1] "2023-05-19"

``` r
tweets_data$agent <- str_replace_na(str_remove(str_extract(tweets_data$text, "\\*[:alnum:]+$"), "\\*"), replacement = "")
```

``` r
agent_extracted <- tweets_data$agent
agent_extracted[agent_extracted == ""] <- NA
names(sort(table(agent_extracted),decreasing=TRUE)[1])
```

    ## [1] "pl"

``` r
# Calculate the number of responses per employee per day
response_counts <- tweets_data %>%
  filter(agent !="") %>%
  group_by(response_date, agent) %>%
  summarise(responses = n())
```

    ## `summarise()` has grouped output by 'response_date'. You can override using the
    ## `.groups` argument.

``` r
response_counts
```

    ## # A tibble: 195 × 3
    ## # Groups:   response_date [15]
    ##    response_date agent responses
    ##    <date>        <chr>     <int>
    ##  1 2015-10-01    aa            4
    ##  2 2015-10-01    ab            9
    ##  3 2015-10-01    dd            5
    ##  4 2015-10-01    jh            1
    ##  5 2015-10-01    kc            8
    ##  6 2015-10-01    km            5
    ##  7 2015-10-01    mr            2
    ##  8 2015-10-01    ng            9
    ##  9 2015-10-01    rb            5
    ## 10 2015-10-01    rd            3
    ## # ℹ 185 more rows

``` r
sd(response_counts$responses)
```

    ## [1] 3.844981

``` r
mean(response_counts$responses)
```

    ## [1] 5.225641

``` r
#Calculate the average number of responses per day per employee
avg_responses <- response_counts %>%
  group_by(agent) %>%
  summarise(average_per_day = mean(responses))
avg_responses
```

    ## # A tibble: 33 × 2
    ##    agent average_per_day
    ##    <chr>           <dbl>
    ##  1 aa               7.75
    ##  2 ab               7   
    ##  3 ad               3.67
    ##  4 an               4   
    ##  5 bb               3   
    ##  6 ck               1.75
    ##  7 cm               6   
    ##  8 cs               1   
    ##  9 dd               6.11
    ## 10 dr               2.33
    ## # ℹ 23 more rows

``` r
# Counted Unique
total_number_of_agents <- tweets_data %>%
  group_by(response_date) %>%
  summarise(total_agents = n_distinct(agent))

total_number_of_agents
```

    ## # A tibble: 15 × 2
    ##    response_date total_agents
    ##    <date>               <int>
    ##  1 2015-10-01              14
    ##  2 2015-10-02              14
    ##  3 2015-10-03              12
    ##  4 2015-10-04              12
    ##  5 2015-10-05              12
    ##  6 2015-10-06              13
    ##  7 2015-10-07              16
    ##  8 2015-10-08              20
    ##  9 2015-10-09              11
    ## 10 2015-10-10              12
    ## 11 2015-10-11              14
    ## 12 2015-10-12              15
    ## 13 2015-10-13              16
    ## 14 2015-10-14              12
    ## 15 2015-10-15              17

What is the typical word length for a social customer service response?
Answer: The typical word length for a social customer service response
is 92.143

Which links were mentioned the most? (Not Required For the Assignment)

Which customer representation (agent) is the most hard-working
(addresses maximum responses)? Answer: agent ‘pl’ is the most
hardworking. There are 95 responses from him.

``` r
sort(table(tweets_data$agent),decreasing=TRUE)[1:3]
```

    ## 
    ##      pl  ng 
    ## 358  95  81

How many social media responses can a customer care agent reasonably
handle? Answer: the maximum social media responses is 100, therefore a
customer care agent can reasonably handle 100 social media responses.
