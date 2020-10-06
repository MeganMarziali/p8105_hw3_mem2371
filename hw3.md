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

## Problem 2

The following code is to load in the accelerometer data.

``` r
accel_df = 
  read_csv("./problem2/accel_data.csv",
           na = "") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity",
    names_prefix = "activity_",
    values_to = "measures"
  ) %>% 
  mutate(
    day_type = recode(
      day, 
      Monday = "Weekday",
      Tuesday = "Weekday",
      Wednesday = "Weekday",
      Thursday = "Weekday",
      Friday = "Weekday", 
      Saturday = "Weekend",
      Sunday = "Weekend"),
    day = factor(day),
    day = forcats::fct_relevel(day, c("Monday",
                                      "Tuesday",
                                      "Wednesday",
                                      "Thursday",
                                      "Friday",
                                      "Saturday",
                                      "Sunday")),
    activity = as.integer(activity)
  )

max(pull(accel_df, day_id))
```

    ## [1] 35

This dataset includes accelerometer data collected from a 65 year old
patient over the course of five weeks. The variables included are week,
day\_id, day, activity, measures, day\_type. There are a total of 50400
rows and 6 columns.

The total number of **activity** measurements taken is 1440.
Measurements were taken every Monday, Tuesday, Wednesday, Thursday,
Friday, Saturday, Sunday, for a total of 5 weeks and 35 days.

The value of the accelerometer readings as per the **measures** variable
ranges from 1 to 8982. The mean of the accelerometer readings is
267.0440592, with a standard deviation of 443.1575016. The median is 74,
with an IQR of 363.

The following code makes a table.

``` r
accel_df %>% 
  group_by(day_id) %>% 
  summarize(
    total_activity = sum(measures)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 35 x 2
    ##    day_id total_activity
    ##     <dbl>          <dbl>
    ##  1      1        480543.
    ##  2      2         78828.
    ##  3      3        376254 
    ##  4      4        631105 
    ##  5      5        355924.
    ##  6      6        307094.
    ##  7      7        340115.
    ##  8      8        568839 
    ##  9      9        295431 
    ## 10     10        607175 
    ## # … with 25 more rows
