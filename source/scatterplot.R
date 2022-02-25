library("plotly")
library("ggplot2")
library("dplyr")
library("tidyverse")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

#scatter plot:focuses on total population of people compared to jail population
#by sex
#Data for males
male_pop <- incarceration_data %>%
  filter(year == 2018) %>%
  select(year, male_pop_15to64, male_jail_pop)
#Data for females in the U.S.
female_pop <- incarceration_data %>%
  filter(year == 2018) %>%
  select(year, female_pop_15to64, female_jail_pop)
#scatterplot for nation vs. jail populations
scatter_plot <- ggplot(data = female_pop) +
  geom_point(mapping = aes(x = female_pop_15to64, y = female_jail_pop,
                           color = "female")) +
  geom_point(data = male_pop, mapping = aes(x = male_pop_15to64,
                                             y = male_jail_pop,
                                             color = "male")) +
  labs(title = "Nation population vs. Jailed Population by Sex",
       x = "U.S. Population",
       y = "Jail Population",
       color = "Sex")

library(lintr)