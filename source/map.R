##maps:LOADING EVERYTHING:
library(ggplot2)
library(tidyverse)
library(maps)
library(dplyr)
library(maps)
library(mapdata)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

##Data Wrangling

#Found the proportion of black people in jail compared to the total
#number of people in jail for each state
black_map <- incarceration_data %>%
  filter(year == 2018) %>%
  group_by(fips) %>%
  summarize(black_people = sum(black_jail_pop, na.rm = TRUE),
            total_people = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(black_prop_jail = (black_people / total_people)) %>%
  filter(black_prop_jail < 1)
  

#loaded the county data set and saved it in variable `county`
county <- map_data("county")

#DATA WRANGLING: combined the county data set with the `county.fips` data set to
#get a data set that has both `polyname` and `fips`
df1 <- county %>%
  unite("polyname", region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname") 
  
##Joined the `df1` and `black_map` data sets together   
df_map <- full_join(df1, black_map, by = "fips") 

View(df_map)
##DEFINED A MINIMALIST THEME AND SAVED IT TO variable `blank_them`:
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
    )

##made a map showing the proportion of black people, how do I make this prettier 
map <- ggplot(data = df_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = black_prop_jail),
               size = .1,
               alpha = .9) +
  labs( title = "Proportion of Black People in Jail compared to Jail Population",
        fill = "Black Proportion") +
  scale_fill_continuous(low = "#CCFFCB", high = "#A846A0") +
  coord_map() +
  blank_theme
map

library(lintr)

  
  


                   


