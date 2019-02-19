
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r4sda

The goal of r4sda is to make it easier when working with large scale
assessment studies data.

## Installation

You can install the development version of r4sda from my
[github](https://github.com/dacarras) repository with:

``` r
devtools::install_github("dacarras/r4sda")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# create an items data frame
# items_data <- dplyr::select(data_frame, LS2T01:LS2T16)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` text
# request a wide response data table with wide_resp() function
r4sda::wide_resp(items_data)

   variable    `1`    `2`    `3`    `4`    `NA`
   <chr>     <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
 1 LS2T01   0.105  0.308  0.0917 0.486  0.00961
 2 LS2T02   0.697  0.103  0.150  0.0389 0.0104 
 3 LS2T03   0.133  0.0316 0.758  0.0708 0.00664
 4 LS2T04   0.100  0.115  0.0423 0.734  0.00841
 5 LS2T05   0.567  0.0802 0.200  0.147  0.00641
 6 LS2T06   0.276  0.219  0.232  0.250  0.0234 
 7 LS2T07   0.0951 0.213  0.494  0.190  0.00871
 8 LS2T08   0.138  0.237  0.584  0.0317 0.00945
 9 LS2T09   0.191  0.426  0.104  0.270  0.00808
10 LS2T10   0.382  0.187  0.202  0.209  0.0208 
11 LS2T11   0.447  0.271  0.107  0.162  0.0125 
12 LS2T12   0.274  0.242  0.159  0.309  0.0150 
13 LS2T13   0.247  0.417  0.149  0.170  0.0166 
14 LS2T14   0.600  0.159  0.0940 0.129  0.0187 
15 LS2T15   0.0956 0.134  0.0665 0.683  0.0210 
16 LS2T16   0.0801 0.593  0.0948 0.208  0.0240 
```

Because this is a table, it can be render as kable table:

You can also embed plots, for example:

``` text
knitr::kable(wide_resp(items_data), digits = 2)
```

| variable |    1 |    2 |    3 |    4 |   NA |
| :------- | ---: | ---: | ---: | ---: | ---: |
| LS2T01   | 0.11 | 0.31 | 0.09 | 0.49 | 0.01 |
| LS2T02   | 0.70 | 0.10 | 0.15 | 0.04 | 0.01 |
| LS2T03   | 0.13 | 0.03 | 0.76 | 0.07 | 0.01 |
| LS2T04   | 0.10 | 0.12 | 0.04 | 0.73 | 0.01 |
| LS2T05   | 0.57 | 0.08 | 0.20 | 0.15 | 0.01 |
| LS2T06   | 0.28 | 0.22 | 0.23 | 0.25 | 0.02 |
| LS2T07   | 0.10 | 0.21 | 0.49 | 0.19 | 0.01 |
| LS2T08   | 0.14 | 0.24 | 0.58 | 0.03 | 0.01 |
| LS2T09   | 0.19 | 0.43 | 0.10 | 0.27 | 0.01 |
| LS2T10   | 0.38 | 0.19 | 0.20 | 0.21 | 0.02 |
| LS2T11   | 0.45 | 0.27 | 0.11 | 0.16 | 0.01 |
| LS2T12   | 0.27 | 0.24 | 0.16 | 0.31 | 0.02 |
| LS2T13   | 0.25 | 0.42 | 0.15 | 0.17 | 0.02 |
| LS2T14   | 0.60 | 0.16 | 0.09 | 0.13 | 0.02 |
| LS2T15   | 0.10 | 0.13 | 0.07 | 0.68 | 0.02 |
| LS2T16   | 0.08 | 0.59 | 0.09 | 0.21 | 0.02 |

> Note: this readme is provisory
