Homework 3
================
Megan Marziali

# Beginning infrastructure

This code chunk calls any relevant libraries.

``` r
library(tidyverse)
library(p8105.datasets)
```

## Problem 1

#### Loading instacart dataset

The following code chunk loads the Instacart dataset

``` r
data("instacart")
```

The variables included in the instacart dataset are order\_id,
product\_id, add\_to\_cart\_order, reordered, user\_id, eval\_set,
order\_number, order\_dow, order\_hour\_of\_day,
days\_since\_prior\_order, product\_name, aisle\_id, department\_id,
aisle, department. There are a total of 1384617 observations, and 15
variables.

#### Answering specific questions

The following code is to check how many aisles.
