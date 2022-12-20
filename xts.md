Time Series with xts and Zoo
================
2022-12-20

# Introduction

Xts stands for eXtensible time series i.e an extended zoo object. It
contains a matrix and an index representing an observation in time.
Essentially, an xts is a matrix with associated times for each
observation. The index must be in increasing order of time. \####
Example

``` r
# create matrix
data <- matrix(1:10, ncol = 5, byrow = T)
data
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    3    4    5
    ## [2,]    6    7    8    9   10

``` r
# create times
idx <- seq(as.Date("2020-01-01"), length = 2, by = "days")
idx
```

    ## [1] "2020-01-01" "2020-01-02"

``` r
# call the xts constructor 
xts <- xts(data, order.by = idx)
```

### Deconstructing the xts

-   **Extract the data components**

``` r
coredata(xts, fmt = F)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    3    4    5
    ## [2,]    6    7    8    9   10

-   **Extracting the index/times**

``` r
index(xts)
```

    ## [1] "2020-01-01" "2020-01-02"

-   Adding an metadata to the xts\*\*

``` r
# add birthday
xts_1999 <- xts(data, order.by = idx, born = as.Date("1999-10-29"))
xts_1999
```

    ##            [,1] [,2] [,3] [,4] [,5]
    ## 2020-01-01    1    2    3    4    5
    ## 2020-01-02    6    7    8    9   10

-   **Converting R objects to xts**

``` r
# call sunspots dataset
data("sunspots")
class(sunspots)
```

    ## [1] "ts"

``` r
# view dataset before conversion
head(sunspots)
```

    ## [1] 58.0 62.6 70.0 55.7 85.0 83.5

``` r
# convert to xts 
sunspots_xts <- as.xts(sunspots)

# view dataset after conversion
head(sunspots_xts)
```

    ##          [,1]
    ## Jan 1749 58.0
    ## Feb 1749 62.6
    ## Mar 1749 70.0
    ## Apr 1749 55.7
    ## May 1749 85.0
    ## Jun 1749 83.5
