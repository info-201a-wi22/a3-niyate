library("plotly")
library("ggplot2")
library("dplyr")
library("tidyverse")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)


#DATA WRANGLING:
#found the sum of jail populations over all the races in the state of Georgia
#did this because Georgia is the state with the highest number of black people
#in jail
df_trendline_1 <- incarceration_data %>%
  filter(state == "GA") %>%
  filter(year > 1984) %>%
  group_by(year) %>%
  summarize(black = sum(black_jail_pop, na.rm = TRUE),
            white = sum(white_jail_pop, na.rm = TRUE),
            aapi =  sum(aapi_jail_pop, na.rm = TRUE),
            latinx = sum(latinx_jail_pop, na.rm = TRUE),
            native = sum(native_jail_pop, na.rm = TRUE)) %>%
  gather(key = race, value = population, - year)



#Created a trend line showing how jail populations have changed from 1984 by race
#added points so the years are clearer
small_trendline <- ggplot(data = df_trendline_1) +
geom_line(mapping = aes(x = year, y = population, color = race)) +
  geom_point(mapping = aes(x = year, y = population),
             size = .5,
             color = "grey") +
  labs(
    title = "Jail Populations by Race Over Time In Georgia, USA",
    x = "Year",
    y = "Amount of People in Jail")
library("lintr")

               
 


