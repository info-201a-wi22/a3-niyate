## A3 ##start at 1990, and do jails


#DPLYR packages I need:
library("dplyr")
library("tidyverse")

#LOADING THE HUGE DATA SET:
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

#1.Most recent year of the data set:
most_recent_year <- incarceration_data %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(year)

#1: avg value of black people in jail in 2013
avg_black_people <- incarceration_data %>%
  drop_na() %>% 
  filter(year == 2013) %>%
  summarize(avg = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(avg)
#2:avg value of white people in jail in 2013
avg_white_people <- incarceration_data %>%
  drop_na() %>% 
  filter(year == 2013) %>%
  summarise(avg = mean(white_jail_pop, na.rm = TRUE)) %>%
  pull(avg)

#3. avg value of black people in prison as of 2018:
avg_prison_black_people <- incarceration_data %>%
  drop_na() %>%
  filter(year == 2013) %>%
  summarize(avg = mean(black_prison_pop, na.rm = TRUE)) %>%
  pull(avg)

#4. avg value of white people in prison as of 2018:
avg_prison_white_people <- incarceration_data %>%
  drop_na() %>%
  filter(year == 2013) %>%
  summarize(avg = mean(white_prison_pop, na.rm = TRUE)) %>%
  pull(avg)

#5. State with the high amount of black people in jail
state_high_black_jail_pop <- incarceration_data %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarize(black_jail = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail == max(black_jail, na.rm = TRUE)) %>%
  pull(state)

#9. States with the lowest amount of black people in jail
state_low_black_jail_pop <- incarceration_data %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarize(black_jail = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail == min(black_jail, na.rm = TRUE)) %>%
  pull(state)

#8.The proportion of black people in jail compared to the total jail population
#in2018 in Georgia
proportion_black_ga <- incarceration_data %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarize(black_jail_GA = sum(black_jail_pop, na.rm = TRUE),
            jail_pop_GA = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(black_prop_GA = (black_jail_GA / jail_pop_GA), na.rm = TRUE) %>%
  pull(black_prop_GA)

#9.Proportion of white people in jail compared to total jail population in 2018
#in Georgia
proportion_white_ga <- incarceration_data %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarize(white_jail_GA = sum(white_jail_pop, na.rm = TRUE),
            jail_pop_GA = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(white_prop_GA = (white_jail_GA / jail_pop_GA), na.rm = TRUE) %>%
  pull(white_prop_GA)

##Extra info for me:
#found black population in Georgia:
black_pop_ga <- incarceration_data %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarise(black_ga = sum(black_pop_15to64, na.rm = TRUE)) %>%
  pull(black_ga)
#found white population in Georgia:
white_pop_ga <- incarceration_data %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarise(white_ga = sum(white_pop_15to64, na.rm = TRUE)) %>%
  pull(white_ga)

library(lintr)
