Time Series With tsibble
================
2022-12-20

``` r
# install.packages("fpp3")
pacman::p_load(fpp3, tidyverse, janitor, lubridate, readxl)

# laod data
data("global_economy")

head(global_economy)
```

| Country     | Code | Year |        GDP | Growth | CPI |   Imports |   Exports | Population |
|:------------|:-----|-----:|-----------:|-------:|----:|----------:|----------:|-----------:|
| Afghanistan | AFG  | 1960 |  537777811 |     NA |  NA |  7.024793 |  4.132233 |    8996351 |
| Afghanistan | AFG  | 1961 |  548888896 |     NA |  NA |  8.097166 |  4.453443 |    9166764 |
| Afghanistan | AFG  | 1962 |  546666678 |     NA |  NA |  9.349593 |  4.878051 |    9345868 |
| Afghanistan | AFG  | 1963 |  751111191 |     NA |  NA | 16.863910 |  9.171601 |    9533954 |
| Afghanistan | AFG  | 1964 |  800000044 |     NA |  NA | 18.055555 |  8.888893 |    9731361 |
| Afghanistan | AFG  | 1965 | 1006666638 |     NA |  NA | 21.412803 | 11.258279 |    9938414 |

### Characteristics of tsibble objects

-   Essentially a tsibble is a time series table with time being the
    index i.e temporal structure. The year column is the index is this
    dataset.

-   Has a frequency, in this case, it is annual (IY).

-   Has key column(s). In this case, the country column is the key
    column and helps us identify the time series uniquely. Keys are
    unique identifiers.

-   Has measured variables. For each key, we have measured variable(s).

### The tourism dataset

A dataset containing the quarterly overnight trips from 1998 Q1 to 2016
Q4 across Australia.

``` r
data("tourism")

head(tourism)
```

| Quarter | Region   | State           | Purpose  |    Trips |
|:--------|:---------|:----------------|:---------|---------:|
| 1998 Q1 | Adelaide | South Australia | Business | 135.0777 |
| 1998 Q2 | Adelaide | South Australia | Business | 109.9873 |
| 1998 Q3 | Adelaide | South Australia | Business | 166.0347 |
| 1998 Q4 | Adelaide | South Australia | Business | 127.1605 |
| 1999 Q1 | Adelaide | South Australia | Business | 137.4485 |
| 1999 Q2 | Adelaide | South Australia | Business | 199.9126 |

Also a tsibble object with the Quarter column being the index, Region,
State, and Purpose being the keys, and Trips being the measured
variable. The data is in quarterly frequency.

### Constructing a tsibble

``` r
data <- tsibble(
  year = 2017:2022,
  cost = c(100, 393, 192, 394, 400, 500),
  index = year
)

data
```

| year | cost |
|-----:|-----:|
| 2017 |  100 |
| 2018 |  393 |
| 2019 |  192 |
| 2020 |  394 |
| 2021 |  400 |
| 2022 |  500 |

### Convert a tibble to a tsibble

``` r
data <- tibble(
  year = 2017:2022,
  cost = c(100, 393, 192, 394, 400, 500)
  ) %>% 
  as_tsibble(index = year)

data
```

| year | cost |
|-----:|-----:|
| 2017 |  100 |
| 2018 |  393 |
| 2019 |  192 |
| 2020 |  394 |
| 2021 |  400 |
| 2022 |  500 |

``` r
mobile <- read_csv("datasets/mobile_money_data.csv", show_col_types = F)

mobile %>% 
  select(start_time, 3:5) %>% 
  mutate(start_time = mdy_hms(start_time),
         year_quartet = wday(start_time)) %>% 
  head()
```

| start_time          | hhid | account_num | account_type  | year_quartet |
|:--------------------|-----:|------------:|:--------------|-------------:|
| 2019-10-28 09:05:08 | 1001 |           1 | Mobile Money  |            2 |
| 2019-10-28 10:42:17 | 1001 |           2 | Bank Account  |            2 |
| 2019-10-28 11:47:47 | 1001 |           3 | VSLA Account  |            2 |
| 2019-10-28 13:02:33 | 1002 |           1 | SACCO Account |            2 |
| 2019-10-28 14:01:04 | 1002 |           2 | VSLA Account  |            2 |
| 2019-10-28 15:03:31 | 1003 |           1 | Mobile Money  |            2 |
