Iteration
================
Alexey Abramov
11/9/2020

  - [Setup](#setup)
  - [Simple](#simple)
      - [Z-scores](#z-scores)

# Setup

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(readr)
library(patchwork)
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 6,
  out.width = "90%"
)

theme_set(
  ggthemes::theme_fivethirtyeight() + theme(legend.position = "bottom")
  )

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.colour = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

# Simple

## Z-scores

(Sample - mean) / sd

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
mean(x_vec)
```

    ## [1] 4.283267

``` r
sd(x_vec)
```

    ## [1] 3.06515

``` r
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.1345337 -1.5994529  1.0190594 -1.4844095  1.3777786  1.7627103
    ##  [7]  0.2487785 -0.1062922  0.4748066  1.6211489 -0.2636724 -0.7189403
    ## [13]  0.2207083 -0.7610507  1.9450122 -0.1534813  1.3178525  0.1238359
    ## [19]  0.6920154 -0.1863135 -1.3782421  0.2698977 -1.4675514 -0.3351525
    ## [25] -1.1065377 -0.9021145 -1.0794023  0.5558979  0.1955592 -0.4169819

I want to write a function for Z-scores

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

``` r
z_scores(x_vec)
```

    ##  [1]  0.1345337 -1.5994529  1.0190594 -1.4844095  1.3777786  1.7627103
    ##  [7]  0.2487785 -0.1062922  0.4748066  1.6211489 -0.2636724 -0.7189403
    ## [13]  0.2207083 -0.7610507  1.9450122 -0.1534813  1.3178525  0.1238359
    ## [19]  0.6920154 -0.1863135 -1.3782421  0.2698977 -1.4675514 -0.3351525
    ## [25] -1.1065377 -0.9021145 -1.0794023  0.5558979  0.1955592 -0.4169819

Try my function on some other things.

``` r
z_scores(3)
```

    ## [1] NA

Ok, updating the function.

``` r
z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if(length(x) < 3){
    stop("Input must be at least 3")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

``` r
z_scores("my name is Jeff")
```

Multiple outputs

``` r
z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if(length(x) < 3){
    stop("Input must be at least 3")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(
    mean = mean_x,
    sd = sd_x
  )
}
```

``` r
z_scores(x_vec)
```

    ## $mean
    ## [1] 4.283267
    ## 
    ## $sd
    ## [1] 3.06515

Multiple inputs

I’d like to do this with a function\!…

``` r
sim_df = tibble(
  x = rnorm(100, 4, 3))

sim_df %>% 
  summarize(
    mean = mean(x),
    sd = sd(x))
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.88  3.05

Here we go:

``` r
sim_mean_sd = function(samp_size, mu, sigma){
  
  sim_df = tibble(
  x = rnorm(n = samp_size, mean = mu, sd = sigma))

sim_df %>% 
  summarize(
    mean = mean(x),
    sd = sd(x))

}
```

``` r
sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.83  2.54
