Homework 3
================
Megan Marziali

## Beginning infrastructure

This code chunk calls any relevant libraries and setting options.

``` r
library(tidyverse)
library(p8105.datasets)
library(patchwork)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Problem 1: Instacart

#### Loading instacart dataset

The following code chunk loads the Instacart dataset

``` r
data("instacart")
```

This dataset contains 1384617 rows and 15 columns. Observations are the
level of items in orders by users of instacart. There are user and order
variables – user\_id, order id, order day, and order hour. There are
also item variables – name, aisle, department and some numeric codes.

#### Answering specific questions

The following code is to check how many aisles there are which are
ordered from.

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

Now, we need to make a plot of the number of orders purchased per aisle.

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(
    aisle = factor(aisle),
    aisle = fct_reorder(aisle, n)
  ) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Aisle order plot",
    x = "Aisle names",
    y = "Number of orders",
    caption = "Data from instacart."
    )
```

<img src="hw3_files/figure-gfm/plot_aisles-1.png" width="90%" />

The next step is to make a table showing the most popular items.

``` r
instacart %>% 
  filter(aisle %in% c(
    "baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(aisle, rank) %>% 
  knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

The next step is to make a table of the mean hour at which specific
items are ordered.

``` r
instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour = mean(order_hour_of_day)) %>% 
  pivot_wider(
    names_from = order_dow,
    values_from = mean_hour
  ) %>% 
  knitr::kable()
```

| product\_name    |        0 |        1 |        2 |        3 |        4 |        5 |        6 |
| :--------------- | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 | 15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 | 14.25000 | 11.55172 | 12.78431 | 11.93750 |

## Problem 2: Accelerometer

The following code is to load in the accelerometer data.

``` r
accel_df = 
  read_csv("./problem2/accel_data.csv",
           na = "") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    names_prefix = "activity_",
    values_to = "activity_count"
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
    minute = as.integer(minute)
  )
```

This dataset includes accelerometer data collected from a 65 year old
patient. Observations correspond to activity counts over the course of
24 hours. The variables included are week, day\_id, day, minute,
activity\_count, day\_type. There are a total of 50400 rows and 6
columns. Activity counts were taken every minute of the day, for a total
of 1440 minutes. Measurements were taken every Monday, Tuesday,
Wednesday, Thursday, Friday, Saturday, Sunday, for a total of 5 weeks
and 35 days.

The value of the accelerometer readings as per the activity\_count
variable ranges from 1 to 8982. The mean of the accelerometer readings
is 267.0440592, with a standard deviation of 443.1575016. The median is
74, with an IQR of 363.

### Aggregating data

The following code makes a table, aggregated by total activity per day.

``` r
accel_df %>% 
  group_by(day_id, day) %>% 
  summarize(
    total_activity = sum(activity_count)
  ) %>% 
  arrange(desc(total_activity)) %>% 
  knitr::kable(digits = 1)
```

| day\_id | day       | total\_activity |
| ------: | :-------- | --------------: |
|      16 | Monday    |        685910.0 |
|       4 | Sunday    |        631105.0 |
|      29 | Friday    |        620860.0 |
|      10 | Saturday  |        607175.0 |
|       8 | Friday    |        568839.0 |
|      33 | Thursday  |        549658.0 |
|       1 | Friday    |        480542.6 |
|      12 | Thursday  |        474048.0 |
|      21 | Wednesday |        468869.0 |
|      15 | Friday    |        467420.0 |
|      18 | Sunday    |        467052.0 |
|      35 | Wednesday |        445366.0 |
|      14 | Wednesday |        440962.0 |
|      28 | Wednesday |        434460.0 |
|      13 | Tuesday   |        423245.0 |
|      11 | Sunday    |        422018.0 |
|      23 | Monday    |        409450.0 |
|      30 | Monday    |        389080.0 |
|      17 | Saturday  |        382928.0 |
|      20 | Tuesday   |        381507.0 |
|       3 | Saturday  |        376254.0 |
|      19 | Thursday  |        371230.0 |
|      34 | Tuesday   |        367824.0 |
|       5 | Thursday  |        355923.6 |
|      26 | Thursday  |        340291.0 |
|       7 | Wednesday |        340115.0 |
|      27 | Tuesday   |        319568.0 |
|       6 | Tuesday   |        307094.2 |
|       9 | Monday    |        295431.0 |
|      25 | Sunday    |        260617.0 |
|      22 | Friday    |        154049.0 |
|      32 | Sunday    |        138421.0 |
|       2 | Monday    |         78828.1 |
|      24 | Saturday  |          1440.0 |
|      31 | Saturday  |          1440.0 |

It seems that the patient is the least active on Saturdays, and the most
active on Fridays.

``` r
accel_df %>% 
  group_by(day_id, day) %>% 
  ggplot(aes(x = minute, y = activity_count, color = day)) +
  geom_line(alpha = 0.5) +
  geom_smooth()
```

<img src="hw3_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

It seems that the patient is active more during the day on Sundays, and
more active in the evening on Fridays. The time at which the patient is
most active depends on the day.

## Problem 3: NY NOAA

The following code loads in the NY NOAA dataset.

``` r
data("ny_noaa")
```

The following code cleans and tidies the data.

``` r
noaa_clean = 
  ny_noaa %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  separate(
    date,
    sep = "-",
    into = c("year", "month", "day")
  ) %>% 
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    prcp = prcp/10,
    tmax = as.integer(tmax)/10,
    tmin = as.integer(tmin)/10
  )
```

The following code assesses the most commonly observed values for
snowfall.

``` r
noaa_clean %>% 
  count(snow) %>% 
  arrange(desc(n))
```

    ## # A tibble: 248 x 2
    ##     snow       n
    ##    <int>   <int>
    ##  1     0 1112758
    ##  2    25   15809
    ##  3    13   12460
    ##  4    51    9252
    ##  5     5    5669
    ##  6     8    5380
    ##  7    76    5296
    ##  8     3    5276
    ##  9    38    5050
    ## 10   102    3386
    ## # … with 238 more rows

The most common observed value for snowfall is 0 mm of snowfall. The
second most common value for snowfall is 25mm. This could suggest either
seasonality of snowfall, or that some weather stations are located in
areas that are less likely to have snowfall compared to others.

The following code makes a two-panel plot showing the average max
temperature in January and July.

``` r
month_df = 
  tibble(
    month = 1:12,
    month_name = month.name)

noaa_month = 
  left_join(noaa_clean, month_df, by = "month") %>% 
  select(-month)

noaa_month %>% 
  group_by(id, year, month_name) %>% 
  summarize(
    average = mean(tmax)
  ) %>% 
  filter(month_name %in% c("January", "July")) %>% 
  ggplot(aes(x = year, y = average)) +
  geom_point(alpha = 0.5, size = 0.5) +
  facet_grid(. ~ month_name)
```

    ## `summarise()` regrouping output by 'id', 'year' (override with `.groups` argument)

<img src="hw3_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

On average, the maximum temperatures in January are lower than they are
in July. It seems that there are some discernible outliers, where the
max temperature in January is much higher than the average. There are
also certain outliers that are much colder in January. Similarly,there
is one outlier which is much colder in July than the rest of the
temperatures.

Making a two-panel plot showing (i) tmax and tmin and (ii) plot showing
the distribution of snowfall values greater than 0 and less than 100
separately by year

``` r
tmax_tmin_plot = 
  noaa_clean %>% 
  select(tmax, tmin) %>% 
  ggplot(aes(x = tmax, y = tmin)) +
  geom_hex() +
  theme(legend.position = "none")

snow_dens_plot = 
  noaa_clean %>% 
  filter(
    snow < 100,
    snow > 0
  ) %>% 
  ggplot(aes(x = year, y = snow)) +
  geom_smooth() +
  theme(legend.position = "none")

tmax_tmin_plot + snow_dens_plot
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

<img src="hw3_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />
