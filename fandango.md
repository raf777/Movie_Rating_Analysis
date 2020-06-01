Analysis of Fandango Movie ratings
================

Project Goal:

To investigate the integrity of movie ratings on Fandango before and
after Walt Hickey’s article in FiveThirthyEight
<https://fivethirtyeight.com/features/fandango-movies-ratings/>. Hickey
described an error in the aggregation of Fandango’s scores in comparison
to other popular movie rating sites like Rotten Tomatoes and IMDB.
Leading to Fandango being much more generous than their counterparts

This project will compare and contrast Fandango’s ratings pre- Hickey’s
article (2015) and post-Hickey’s article (2016)

Loading required libraries

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(stringr)
library(tidyr)
```

Reading in datasets in of the ratings Hickey used in his analysis and
the 16/17 comparison

``` r
original_ratings <- read.csv("fandango_score_comparison.csv")
new_ratings <- read.csv("movie_ratings_16_17.csv")
```

Next I’ll drop the columns that are not relevent to our analysis of
Fandango

``` r
original_ratings <- original_ratings %>%
                select("FILM", "Fandango_Stars", "Fandango_Ratingvalue",
                       "Fandango_votes")
new_ratings <- new_ratings %>%
        select("movie", "year", "fandango")
```

Lets have a quick glimpse of our two dataframes

``` r
glimpse(original_ratings)
```

    ## Observations: 146
    ## Variables: 4
    ## $ FILM                 <fct> "Avengers: Age of Ultron (2015)", "Cinder...
    ## $ Fandango_Stars       <dbl> 5.0, 5.0, 5.0, 5.0, 3.5, 4.5, 4.0, 4.0, 4...
    ## $ Fandango_Ratingvalue <dbl> 4.5, 4.5, 4.5, 4.5, 3.0, 4.0, 3.5, 3.5, 4...
    ## $ Fandango_votes       <int> 14846, 12640, 12055, 1793, 1021, 397, 252...

``` r
glimpse(new_ratings)
```

    ## Observations: 214
    ## Variables: 3
    ## $ movie    <fct> 10 Cloverfield Lane, 13 Hours, A Cure for Wellness, A...
    ## $ year     <int> 2016, 2016, 2016, 2017, 2016, 2016, 2016, 2016, 2016,...
    ## $ fandango <dbl> 3.5, 4.5, 3.0, 4.5, 3.0, 4.0, 4.5, 4.0, 4.0, 3.5, 4.0...

Few things to note from this;

  - We have considerably more rows in the new\_ratings dataframe
  - The year of release is embedded into the title column for the
    original\_ratings, so we’ll have to do a bit of string manipulation
    later to pull that out
  - We can also see that the Fandango column in the new\_ratings
    dataframe equates to the fandango\_Stars in the original\_ratings
    dataframe, so I might quickly change that name so they match up.

Quickly changing the columns names to match both dataframes and satisfy
my OCD

``` r
new_ratings <- new_ratings %>%
    mutate(Fandango_Stars = fandango,
           fandango = NULL,
           FILM = movie,
           movie = NULL)
glimpse(new_ratings)
```

    ## Observations: 214
    ## Variables: 3
    ## $ year           <int> 2016, 2016, 2016, 2017, 2016, 2016, 2016, 2016,...
    ## $ Fandango_Stars <dbl> 3.5, 4.5, 3.0, 4.5, 3.0, 4.0, 4.5, 4.0, 4.0, 3....
    ## $ FILM           <fct> 10 Cloverfield Lane, 13 Hours, A Cure for Welln...

In Hickey’s article, he set a minimum of 30 user\_votes for a movie to
be considered in his analysis, so I’ll use the same figure.

``` r
summary(original_ratings$Fandango_votes)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    35.0   222.2  1446.0  3848.8  4439.5 34846.0

Great so we can see we’ve a minimum value of 35, so we’re good to go\!

As was mentioed before, we’ll need to get that year out of the of the
movie title column.

``` r
original_ratings <- original_ratings %>%
        mutate(year = str_sub(FILM, -5, -2))
```

Next step is to isolate the films released in 2015 and 2016, as these
will be the two we’ll be using in our comparison.

``` r
movies_15 <- original_ratings %>%
        filter(year == 2015)

movies_16 <- new_ratings %>%
        filter(year == 2016)
```

So now we have out two dataframes with just the information we need to
compare their distributions

``` r
ggplot(data = movies_15, aes(x = Fandango_Stars)) +
        geom_density(fill = "red", alpha = 0.35) +
        geom_density(data = movies_16, aes(x = Fandango_Stars), color = "blue", 
                     fill = "blue", alpha = 0.35) +
        geom_vline(xintercept = mean(movies_15$Fandango_Stars), color = "blue",
                   lwd = 0.75, linetype = "longdash") +
        geom_vline(xintercept = mean(movies_16$Fandango_Stars), color = "red",
                   lwd = 0.75, linetype = "longdash") +
        labs(x = "Number of Stars", y = "Density") +
        scale_x_continuous(breaks = seq(0,5,0.5)) +
        theme(legend.position = "Top")
```

![](fandango_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> Few things
to note from this \* Both years are left-skewed distributions \* The
mean rating being higher in 2015 than it was in 2016 \* Big spike for
4-star ratings in 2016

Time to delve a little deeper in to the data and take a more granular
look at it. We’ll do this by generating frequency tables for both years

2015

``` r
freq_table_2015 <- movies_15 %>%
                group_by(Fandango_Stars) %>%
                summarise(Absolute_Freq = n()) %>%
                mutate(Percentage = (Absolute_Freq / nrow(movies_15)) * 100)
freq_table_2015
```

    ## # A tibble: 5 x 3
    ##   Fandango_Stars Absolute_Freq Percentage
    ##            <dbl>         <int>      <dbl>
    ## 1            3              11       8.53
    ## 2            3.5            23      17.8 
    ## 3            4              37      28.7 
    ## 4            4.5            49      38.0 
    ## 5            5               9       6.98

2016

``` r
freq_table_2016 <- movies_16 %>%
                group_by(Fandango_Stars) %>%
                summarise(Absolute_Freq = n()) %>%
                mutate(Percentage = (Absolute_Freq / nrow(movies_15)) * 100)
freq_table_2016
```

    ## # A tibble: 6 x 3
    ##   Fandango_Stars Absolute_Freq Percentage
    ##            <dbl>         <int>      <dbl>
    ## 1            2.5             6      4.65 
    ## 2            3              14     10.9  
    ## 3            3.5            46     35.7  
    ## 4            4              77     59.7  
    ## 5            4.5            47     36.4  
    ## 6            5               1      0.775

FInally we will calculate summary statistics for 2015 and 2016

As there is no built\_in mode function in R, this mode function is taken
from
<https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode>

``` r
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
```

Generating the summary statistics

``` r
summary_2015 <- movies_15 %>%
                summarise(year = '2015',mean = mean(Fandango_Stars),
                          median = median(Fandango_Stars),
                          mode = mode(Fandango_Stars))
summary_2015
```

    ##   year     mean median mode
    ## 1 2015 4.085271      4  4.5

``` r
summary_2016 <- movies_16 %>%
                summarise(year = '2016', mean = mean(Fandango_Stars),
                          median = median(Fandango_Stars),
                          mode = mode(Fandango_Stars))
summary_2016
```

    ##   year     mean median mode
    ## 1 2016 3.887435      4    4

Combining the summary statistics from both years

``` r
combined_year_stats <- bind_rows(summary_2015, summary_2016)
```

Pivoting the summary table to make is usable for ggplot

``` r
combined_year_stats <- combined_year_stats %>%
    pivot_longer(cols = c(mean, mode, median),
                 values_to = 'value',
                 names_to = 'statistic')
```

Visualizing the differnece in the summary statistics

``` r
ggplot(data = combined_year_stats, aes(x = statistic, y = value, fill = year)) +
    geom_bar(stat = 'identity',position = 'dodge') +
    labs(y = 'Summary Statistic', x = 'Statistic') +
    ggtitle('Statistical comparison 2015 vs 2016') +
    scale_y_continuous(breaks = seq(1,5,0.5))
```

![](fandango_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Conclusions;

  - It looks like Fandango did modify their rating slightly in the wake
    of Walt Hickey’s article in 2015

  - We can also see the most common rating awarded in 2015 was 4.5,
    against 4.0 in 2016. Which suggest that they may hav corrected their
    rounding up issue that was the reason for their embelished ratings
