Dataset Project Prep - Lab 6
================
Jennifer Habicher
2024-10-10

# load packages and dataset

``` r
library (haven)
library (dplyr)
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
library (ggplot2)

#load (change example)
load("/Users/northlight/Desktop/PSY329 - My Data Project - ICPSR_38417 - Dataset/DS0001/38417-0001-Data.rda")
```

``` r
Selected_Dataset <- da38417.0001 %>%
  select(Q17_1, Q19, Q20, Q21, Q22, Q117, Q65, Q55E, D2, HHR5, IDENTITY_1_R)
```
