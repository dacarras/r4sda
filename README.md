
# r4sda

-   The goal of r4sda is to make it easier to work with data from large
    scale assessment studies.
-   Is a collection of simple wrapper functions, mainly, generated to
    solve common tasks when working with data from large scale
    assessment, secondary data from large national records, or other
    examples of data with nested observations
-   It generates tables, variables, weights, and other objects to aid
    data analysis
-   It relies on libraries such as: dplyr, stringr, purrr among others

## Installation

You can install, in R, the development version of r4sda from this
[github](https://github.com/dacarras/r4sda) repository with:

``` r
devtools::install_github("dacarras/r4sda")
```

# Development

## Pendings

-   add sample data to show functions examples
-   add a viggnete
-   add merge function

## Done

-   add functions for weights in mixed models, methods a and method b,
    also called normalized and effective sample size weights
-   add mean score within observations
-   add check cluster id function

# List of functions

## Descriptives

-   `wide_resp()` generates a table of items as rows, and response value
    as columns, and displays the percentage of responses per item.
-   `wide_var()` generates a table of response as rows, and variablesas
    columns, and displays the percentage of responses per item.
-   `stack_resp()` generates a table of items as rows, responses as
    attributes of items, and the percentage of each response category.
-   `get_desc()` it produces a table with descriptives, including:
    percentage of missing, complete observations, n, means, sd, minimum,
    maximum, skewness, kurtosis, and histogram of variables.

## Aggregated scores

-   `c_mean()` estimate cluster means to aid cluster mean centering in
    mixed models.
-   `c_wmean()` estimate cluster means to aid cluster mean centering in
    mixed models, including weights within clusters.
-   `c_sum()` estimate cluster sums to aid cluster variables generation
    for mixed models.
-   `c_sd()` estimate cluster standard deviations of a variable.

## Manifest Scores

-   `reverse()` it generates a reverse score for any given numeric
    vector (it removes the labels if the variable is labelled).
-   `z_score()` it standardize variables returning these as z scores.
-   `mean_score()` it create mean score of variable (i.e. row wise
    means).
-   `sum_score()` it create sum score of variables (i.e. row wise sum).

## LSA Weights

-   `lsa_weights()` add normalized and effective sample weights to the
    provided data frame.
-   `senate_weights()` add senate weights scaled up a to a number
    (e.g. 500, 1000 or else).
-   `jkr_iccs()` add jackknifes replicate weights to ICCS 2009 study
    data frame.
-   `svy_freq()` estimates proportions for each category from
    categorical variable, from a survey object.

## Meta-data

-   `variable_label()` it gets variable labels from a variable, from
    labelled vector.
-   `value_label()` it generate a table from the value labels from a
    labelled vector.
-   `variables_table()` generates a table of a data frame, including
    variable names, variable types, sample values, and variable labels.
-   `remove_labels()` it remove labels from a data frame. It aids the
    use of data frame for other packages and software that needs plain
    data for their use.

## Mixed Models

-   `get_icc()` it estimates the Intra class correlation of an MLM model
    from lme4.

-   `check_cluster_id()` it tests if the cluster id is unique across the
    data frame, or if these repeats between addtional cluster factors.
    For example, it checks if schools id repeats between country
    observations, or if observations id are unique between schools.

-   `caterpillar_plot()` it extracts the realizations of a random
    intercept model, generated by `lme4`. The output is a plot, from
    `ggplot2`, thus, the user can further specify theme options, axis
    length, among other customizations. It was develop to visually
    inspect random intercept spreadings, for unconditioned and
    conditioned models.

-   `caterpillar_mean_plot()` it extracts the realizations of a random
    intercept model, generated by `lme4`. The output is a plot, from
    `ggplot2`, thus, the user can further specify theme options, axis
    length, among other customizations. It was develop to visually
    inspect random intercept spreadings, for unconditioned and
    conditioned models. This version adds the grand of the model to the
    random effect, depicting the latent mean in return.

-   \[…\]

## Misc

-   `local_path()` aids the generation of relative working folders. It
    assumes your syntax and data folders are on the same logical level
    of folder structure, and aids getting the parent directory. As such:
    -   `local_path(getwd())` retrieves the parent directory of workint
        directory
    -   `local_path('/00_data/')` retrieves the absolute location of the
        ‘/00\_data/’ folder, if this folder is below the parent
        directory of the working directory, where the syntax is located.
    -   This function allows to run ’\*.rmd’ files between Mac and
        Windows machines using Rstudio, or R-Box, and source the entire
        code using a relative folder structure.
    -   An example of such a structure is:

``` text
/00_data/
/01_syntax/
/02_tables/
```

-   `text_to_table()` helps to take unstructured text that includes the
    variables of MPLUS data, and create a table with the list of
    included variables.

-   `decimal()` format number with decimal places, into strings with a
    given number of decimal places.

-   `get_lrt_scf()` Likelihood Ratio Test for MPLUS fitted model with
    the MLR estimator (see <https://www.statmodel.com/chidiff.shtml>.)

# Example

This is a basic example which shows you how to solve a common problem:

``` r
# create an items data frame
# items_data <- dplyr::select(data_frame, LS2T01:LS2T16)
```

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

``` text
knitr::kable(wide_resp(items_data), digits = 2)
```

| variable |    1 |    2 |    3 |    4 |   NA |
|:---------|-----:|-----:|-----:|-----:|-----:|
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
