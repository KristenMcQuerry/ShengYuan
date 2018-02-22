##install packages
#install.packages("tidyverse")
#install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights

## arrival delay of two or more hours
flights %>%
    filter(arr_delay >= 120)

## Flew to Houston
flights %>%
    filter(dest == 'HOU' | dest == 'IAH')

## operated by United, American, or Delta
flights %>%
    filter(carrier == 'AA' | carrier == 'UA' | carrier == 'DL')

## Departed in summer
flights %>%
    filter(month == 6 | month == 7 | month == 8)

## Arrived more than two hours late, but didn't leave late
flights %>%
    filter(arr_delay >= 120 & dep_delay <= 0)

##  Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>%
    filter(dep_delay >= 60) %>%
    filter(arr_delay <= dep_delay-30)

## Departed between midnight and 6 a.m. (inclusive)
flights %>%
    filter(dep_time >= 0 & dep_time <= 630)

## Sort to find the most delayed flights. Find the flights that left earliest.
flights %>%
    arrange(dep_delay)

flights %>%
    arrange(desc(dep_delay))

## Sort to find the fastest flights
flights %>%
    mutate(speed = distance / air_time) %>%
    arrange(speed)

## Which flights traveled the longest? Which traveled the shortest?
flights %>%
    arrange(distance)

flights %>%
    arrange(desc(distance))
