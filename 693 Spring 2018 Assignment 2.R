##install packages
#install.packages("tidyverse")
#install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights <- as.tibble(flights)

## 


##worst delay for carrier only
glimpse(flights %>%
        group_by(carrier) %>%
        filter(dep_delay >= 0) %>%
        summarise(n_planes = n()) %>%
        arrange(n_planes))

### can disentangle effect of airport and carrier
glimpse(flights %>%
        group_by(carrier, dest) %>%
        filter(dep_delay >= 0) %>%
        summarise(n_planes = n()) %>%
        arrange(desc(n_planes)))


### each plane count flights before first delay of greater than 1 hour


###  which has the worst on-time record
flights %>%
    group_by(tailnum) %>%
    filter(dep_delay == 0) %>%
    summarise(nplanes = n()) %>%
    arrange(desc(nplanes))


###what time of day you want to avoid delay much as possible
flights %>%
    mutate(Date = paste(year, month, day, sep = '-')) %>%
    group_by(Date) %>%
    filter(dep_delay >= 0) %>%
    summarise(nplanes = n()) %>%
    arrange(nplanes)

###For each destination, compute the total minutes of delay.###
flights %>%
    filter(arr_delay >= 0) %>%
    group_by(dest) %>%
    summarize(totalarrDelay = sum(arr_delay, na.rm = TRUE))

##For each flight, compute the proportion of the total delay for its destination###
flights %>%
    filter(arr_delay >= 0) %>%
    group_by(dest, flight) %>%
    summarize(arrD_ef = sum(arr_delay, na.rm = TRUE)) %>%
    mutate(prop_f = as.vector(arrD_ef) / as.vector(by(arrD_ef, dest, sum)))

### suspiciously fast
flights %>%
    mutate(realtime = hour * 60 + minute, timediff=(realtime - air_time))%>%
    arrange(desc(timediff))

### most delayed
flights %>%
    mutate(realtime = hour * 60 + minute) %>%
    arrange(realtime) %>%
    group_by(dest) %>%
    mutate(diff = (realtime - by(realtime, dest, min))) %>%
    arrange(desc(diff))
