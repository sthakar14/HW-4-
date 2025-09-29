hw4
================
Samruddhi Thakar
2025-09-26

``` r
library(RMariaDB)
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
con <- dbConnect(
  MariaDB(), host = "scidb.smith.edu",
  user = "waiuser", password = "smith_waiDB", 
  dbname = "wai"
)
Measurements <- tbl(con, "Measurements")
num_rows = Measurements %>% count() %>% pull(n)
print(paste0('Number of rows in Measurement table are ', num_rows ))
```

    ## [1] "Number of rows in Measurement table are 5052304"

Q3. Identify what years of data are available in the flights table of
the airlines database.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.2     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.4     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.4     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(mdsr)
library(RMariaDB)
con <- dbConnect_scidb("airlines")

flights <- tbl(con, "flights")


# find distinct years in the data
years_available <- flights %>%
  distinct(year) %>%      # pick unique values of the 'year' column
  arrange(year) %>%       # order them
  collect()               # bring the results into R

print(years_available)
```

    ## # A tibble: 3 × 1
    ##    year
    ##   <int>
    ## 1  2013
    ## 2  2014
    ## 3  2015

Q4. Checking the cloumns available in the DB.

``` r
print(head(flights))
```

    ## # Source:   SQL [?? x 21]
    ## # Database: mysql  [mdsr_public@mdsr.crcbo51tmesf.us-east-2.rds.amazonaws.com:3306/airlines]
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <int>    <int>          <int>
    ## 1  2013    10     1        2             10        -8      453            505
    ## 2  2013    10     1        4           2359         5      730            729
    ## 3  2013    10     1       11             15        -4      528            530
    ## 4  2013    10     1       14           2355        19      544            540
    ## 5  2013    10     1       16             17        -1      515            525
    ## 6  2013    10     1       22             20         2      552            554
    ## # ℹ 13 more variables: arr_delay <int>, carrier <chr>, tailnum <chr>,
    ## #   flight <int>, origin <chr>, dest <chr>, air_time <int>, distance <int>,
    ## #   cancelled <int>, diverted <int>, hour <int>, minute <int>, time_hour <dttm>

1.  How many domestic flights flew into Dallas-Fort Worth (DFW) on May
    14, 2015?

``` r
num_flights <- flights %>% 
      filter(dest == 'DFW', year== 2015, month ==5, day == 14 ) %>% tally() %>% collect() %>% pull(n)

print(paste0(num_flights, " flights flew into DFW on May 14, 2015."))
```

    ## [1] "737 flights flew into DFW on May 14, 2015."

2.  Of all the destinations from Chicago O’Hare (ORD), which were the
    most common in 2015?

``` r
flights_from_ORD <- flights %>% filter(origin == "ORD", year == 2015) %>% group_by(dest) %>% tally(sort = TRUE) %>% collect() %>% pull(dest)
print(flights_from_ORD[1])
```

    ## [1] "LGA"

``` r
print(paste0("Of all the destinations ", flights_from_ORD[1], " was the most common destination from ORD in 2015") )
```

    ## [1] "Of all the destinations LGA was the most common destination from ORD in 2015"

3.  Which airport had the highest average arrival delay time in 2015?

``` r
airport_delay <- flights %>% filter(year ==2015) %>% group_by(dest) %>% summarise(avr_arr_delay = mean(arr_delay)) %>% arrange(desc(avr_arr_delay)) %>%  collect() %>% pull(dest)
```

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

``` r
print(airport_delay[1])
```

    ## [1] "STC"

4.  How many domestic flights came into or flew out of Bradley Airport
    (BDL) in 2015?

``` r
BDL_flights <- flights %>% filter( year== 2015, dest == "BDL" | origin == "BDL") %>% 
  summarise(num_flights = sum(flight)) %>% collect() %>% pull(num_flights)

print(paste0("Total flights in and out of BDL in 2015 are ", BDL_flights))
```

    ## [1] "Total flights in and out of BDL in 2015 are 79267894"

5.  List the airline and flight number for all flights between LAX and
    JFK on September 26th, 2015.

``` r
LAX_and_JFK <- flights %>% filter(day == 26, month == 9, year == 2015,
                                  (origin == "LAX" & dest == "JFK") | 
                                  (origin == "JFK" & dest == "LAX")) %>% 
                                  select(carrier, flight, dest, origin)%>%
                                  collect()
print(LAX_and_JFK)
```

    ## # A tibble: 60 × 4
    ##    carrier flight dest  origin
    ##    <chr>    <int> <chr> <chr> 
    ##  1 UA         441 LAX   JFK   
    ##  2 B6          24 JFK   LAX   
    ##  3 VX         399 LAX   JFK   
    ##  4 DL        1908 JFK   LAX   
    ##  5 B6          23 LAX   JFK   
    ##  6 AA         118 JFK   LAX   
    ##  7 DL         476 JFK   LAX   
    ##  8 VX         404 JFK   LAX   
    ##  9 AA          34 JFK   LAX   
    ## 10 AA          33 LAX   JFK   
    ## # ℹ 50 more rows

Q5.

``` r
library(RMariaDB)
db <- dbConnect(
  MariaDB(), 
  user = "waiuser", 
  password = "smith_waiDB", 
  host = "scidb.smith.edu", 
  dbname = "wai"
)
```

Check what tables are available inside the WAI database.

``` r
dbListTables(db)
```

    ## [1] "Codebook"             "Measurements"         "Measurements_pre2020"
    ## [4] "PI_Info"              "PI_Info_OLD"          "Subjects"            
    ## [7] "Subjects_pre2020"

1.  How many female subjects are there in total across all studies?

We need to look at subject table. Load the table and look at its
structure.

``` r
subjects = tbl(db, 'Subjects')
head(subjects)
```

    ## # Source:   SQL [?? x 11]
    ## # Database: mysql  [waiuser@scidb.smith.edu:3306/wai]
    ##   Identifier SubjectNumber SessionTotal AgeFirstMeasurement
    ##   <chr>              <int>        <int>               <dbl>
    ## 1 Abur_2014              1            7                  20
    ## 2 Abur_2014              3            8                  19
    ## 3 Abur_2014              4            7                  21
    ## 4 Abur_2014              6            8                  21
    ## 5 Abur_2014              7            5                  20
    ## 6 Abur_2014              8            5                  19
    ## # ℹ 7 more variables: AgeCategoryFirstMeasurement <chr>, Sex <chr>, Race <chr>,
    ## #   Ethnicity <chr>, LeftEarStatusFirstMeasurement <chr>,
    ## #   RightEarStatusFirstMeasurement <chr>, SubjectNotes <chr>

Now find number of female subjects. (Not sire if i should use
distinct(SubjectNumber) or not. )

``` r
num_females = subjects %>% filter(Sex == 'Female') %>% distinct(SubjectNumber) %>% tally() %>% collect() %>% pull(n)

print(paste0('Number of distinct female subjects in the stidy are ',num_females))
```

    ## [1] "Number of distinct female subjects in the stidy are 3315"

2.  Find the average absorbance for participants for each study, ordered
    by highest to lowest value.

``` r
mes = tbl(db, 'Measurements')
abs <- mes %>% group_by(SubjectNumber) %>% summarise(avg_abs = mean(Absorbance)) %>% arrange(desc(avg_abs)) %>% collect()
print(abs)
```

    ## # A tibble: 6,582 × 2
    ##    SubjectNumber avg_abs
    ##            <int>   <dbl>
    ##  1           804   0.962
    ##  2          1563   0.934
    ##  3          2092   0.932
    ##  4          1609   0.930
    ##  5          1646   0.929
    ##  6          1928   0.926
    ##  7        322023   0.925
    ##  8          1980   0.913
    ##  9          1627   0.911
    ## 10          1823   0.909
    ## # ℹ 6,572 more rows

3.  Write a query to count all the measurements with a calculated
    absorbance of less than 0.

``` r
neg_absorbance_count <- mes %>%
  filter(Absorbance < 0) %>% tally() %>%
  collect()    
print(neg_absorbance_count)
```

    ## # A tibble: 1 × 1
    ##         n
    ##   <int64>
    ## 1   28694

Q6. The following open-ended question may require more than one query
and a thoughtful response. Based on data from 2013 only, and assuming
that transportation to the airport is not an issue, would you rather fly
out of JFK, LaGuardia (LGA), or Newark (EWR)? Why or why not? Use the
dbConnect_scidb function to connect to the airlines database.

We will check all the tables in the airlines database to see which one
gives us relevant information.

``` r
library(dplyr)
library(mdsr)
library(RMariaDB)

# Connect to airlines database
con <- dbConnect_scidb("airlines")

# Reference flights table
flights <- tbl(con, "flights")
airports = tbl(con, 'airports')
summ = tbl(con, 'flights_summary')
planes = tbl(con, 'planes')
print(head(flights))
```

    ## # Source:   SQL [?? x 21]
    ## # Database: mysql  [mdsr_public@mdsr.crcbo51tmesf.us-east-2.rds.amazonaws.com:3306/airlines]
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <int>    <int>          <int>
    ## 1  2013    10     1        2             10        -8      453            505
    ## 2  2013    10     1        4           2359         5      730            729
    ## 3  2013    10     1       11             15        -4      528            530
    ## 4  2013    10     1       14           2355        19      544            540
    ## 5  2013    10     1       16             17        -1      515            525
    ## 6  2013    10     1       22             20         2      552            554
    ## # ℹ 13 more variables: arr_delay <int>, carrier <chr>, tailnum <chr>,
    ## #   flight <int>, origin <chr>, dest <chr>, air_time <int>, distance <int>,
    ## #   cancelled <int>, diverted <int>, hour <int>, minute <int>, time_hour <dttm>

``` r
#print(head(summ))
#print(head(airports))
#print(head(planes))
```

Seems like only flights table is relevant for this question. We will
look at departure delay, number of cancelled flights, and number of
flights diverted starting from these 3 locations.

``` r
flights_2013_nyc <- flights %>%
  filter(year == 2013, origin %in% c("JFK", "LGA", "EWR"))

# Step 2: Summarize key metrics for each airport
library(dplyr)
library(mdsr)
library(RMariaDB)

# Connect to airlines database
con <- dbConnect_scidb("airlines")
flights <- tbl(con, "flights")

# Filter for 2013 and NYC airports
flights_2013_nyc <- flights %>%
  filter(year == 2013, origin %in% c("JFK", "LGA", "EWR"))

# Summarize departure delay, cancellations, and diversions
airport_summary <- flights_2013_nyc %>%
  group_by(origin) %>%
  summarise(
    
    
    
    # Counts

    dep_delay_count = sum(dep_delay > 0, na.rm = TRUE),
    cancelled_count = sum(cancelled > 0, na.rm = TRUE),
    diverted_count = sum(diverted > 0, na.rm = TRUE),
    
    # Averages
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_cancelled = mean(cancelled, na.rm = TRUE),
    avg_diverted = mean(diverted, na.rm = TRUE),
    
    # Minimums
    min_dep_delay = min(dep_delay, na.rm = TRUE),
    min_cancelled = min(cancelled, na.rm = TRUE),
    min_diverted = min(diverted, na.rm = TRUE),
    
    # Maximums
    max_dep_delay = max(dep_delay, na.rm = TRUE),
    max_cancelled = max(cancelled, na.rm = TRUE),
    max_diverted = max(diverted, na.rm = TRUE)
  ) %>%
  collect()

print(airport_summary)
```

    ## # A tibble: 3 × 13
    ##   origin dep_delay_count cancelled_count diverted_count avg_dep_delay
    ##   <chr>            <dbl>           <dbl>          <dbl>         <dbl>
    ## 1 EWR              52711            3350            358          14.7
    ## 2 JFK              42031            1962            238          11.9
    ## 3 LGA              33690            3311            211          10.0
    ## # ℹ 8 more variables: avg_cancelled <dbl>, avg_diverted <dbl>,
    ## #   min_dep_delay <int>, min_cancelled <int>, min_diverted <int>,
    ## #   max_dep_delay <int>, max_cancelled <int>, max_diverted <int>

Count the number of times departure delay \>0 in all 3 airports. Count
the number of times canceled \> 0 and diverted \>0 for all 3 airports
after we find the avg of these values. Find min and max of all of these
values for all the airports. Seems like JFK has minimum cancellations
among the 3, but it has higher number of delayed flights than LGA. We
will check the percent of cacelation and delays in the the table below
to take more informed decision.

``` r
airport_summary2 <- flights_2013_nyc %>%
  group_by(origin) %>%
  summarise(
    total_flights = n(),
    delayed_flights = sum(dep_delay > 0, na.rm = TRUE),
    cancelled_flights = sum(cancelled > 0, na.rm = TRUE),
    pct_delayed = mean(dep_delay > 0, na.rm = TRUE) * 100,
    pct_cancelled = mean(cancelled > 0, na.rm = TRUE) * 100) %>%
  collect()

print(airport_summary2)
```

    ## # A tibble: 3 × 6
    ##   origin total_flights delayed_flights cancelled_flights pct_delayed
    ##   <chr>        <int64>           <dbl>             <dbl>       <dbl>
    ## 1 EWR           120835           52711              3350        43.6
    ## 2 JFK           111279           42031              1962        37.8
    ## 3 LGA           104662           33690              3311        32.2
    ## # ℹ 1 more variable: pct_cancelled <dbl>

Looking at the summary tables above, I would choose JFK over the other 2
airports. NWK has most delays and cancellations, and although LGA has
less flight delay percent than JFK it has more percentage of cancelled
flights, which is worse than delayed flights.
