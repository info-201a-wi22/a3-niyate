library("plotly")
library("ggplot2")
library("dplyr")
library("tidyverse")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

#scatter plot:focuses on total population of people compared to jail population
#by sex

#Calculated the state with the most jail populations
state_most_jail <- incarceration_data %>%
  filter(year == "2018") %>%
  group_by(state) %>%
  summarize(total = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(state)

##Data for males
male_pop <- incarceration_data %>%
  filter(year == 2018) %>%
  select(year, male_pop_15to64, male_jail_pop) 

##Data for females in the U.S.
female_pop <- incarceration_data %>%
  filter(year == 2018) %>%
  select(year, female_pop_15to64, female_jail_pop) 

##scatterplot for nation vs. jail populations
scatter_plot <- ggplot(data = female_pop) +
  geom_point(mapping = aes(x = female_pop_15to64, y = female_jail_pop,
                           color = "female")) +
  geom_point(data = male_pop, mapping = aes( x = male_pop_15to64,
                                             y = male_jail_pop,
                                             color = "male")) +
  labs(
    title = "Nation population vs. Jailed Population by Sex",
    x = "U.S. Population",
    y = "Jail Population",
    color = "Sex")

library(lintr)
  ##male_jail <- incarceration_data %>%
  #filter(year == 2018) %>% 
    #select(year, female_jail_pop) 
  
  #male_jail <- incarceration_data %>%
    #group_by(year) %>% 
    #filter(state == "CA") %>% 
    #filter(year == 2018) %>%
    #summarize(male_jail_total = sum(male_jail_pop, na.rm = TRUE)) %>% 
    #mutate(round_male_jail_total = round(male_jail_total), 0) %>% 
    #select(year, round_male_jail_total) 
  
  #male_state <- incarceration_data %>%
    #filter(year == 2018) %>%
    #filter(state == "CA") %>% 
    #summarize(male_state_total = sum(male_pop_15to64, na.rm = TRUE)) %>% 
    #mutate(round_male_state_total = round(male_state_total), 0) %>%
    #select(year, round_male_state_total)
  
  #total_in_jail <- left_join(female_jail, male_jail, by = c("year"))
  #total_pop <- left_join(female_state, male_state, by = c("year"))
  
  #df <- left_join(total_in_jail, total_pop, by = c("year")) %>% 
    #gather(key = sex, value = population, -year)## need help on this part
  
