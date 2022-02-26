library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(reshape2)

incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors=FALSE)

# Summary: Values calculated from the data
summary_info <- list()

# 1. How many total jail population in the most recent recorded year?
summary_info$total_jail_pop_2018 <- incarcerations %>%
  select(year, total_jail_pop) %>%
  filter(year==max(year)) %>%
  summarize(current_total=ceiling(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(current_total)

#2. Proportion of black jail population in the most recent recorded year?
summary_info$black_jail_prop_2018 <- incarcerations %>%
  select(year, black_jail_pop) %>%
  filter(year==max(year)) %>%
  summarize(current_total=ceiling(sum(black_jail_pop, na.rm = TRUE))) %>%
  summarize(prop=round(current_total/summary_info$total_jail_pop_2018, digits=2)) %>%
  pull(prop)

#3. Proportion of nonwhite jail population in the most recent recorded year
summary_info$nonwhite_jail_prop_2018 <- incarcerations %>%
  filter(year==max(year)) %>%
  summarize(black_total=ceiling(sum(black_jail_pop, na.rm = TRUE)),
            latinx_total=ceiling(sum(latinx_jail_pop, na.rm = TRUE)),
            native_total=ceiling(sum(native_jail_pop, na.rm = TRUE)),
            other_total=ceiling(sum(other_race_jail_pop, na.rm = TRUE))) %>%
  summarize(prop=round((black_total+latinx_total+native_total+other_total)/summary_info$total_jail_pop_2018, digits=2)) %>%
  pull(prop)

#4. Which state has the highest jail population in the most recent recorded year?
summary_info$state_most_jail <- incarcerations %>%
  group_by(state) %>%
  filter(year==max(year)) %>%
  summarize(state_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(state_jail_pop==max(state_jail_pop)) %>%
  pull(state)

#5. Which state has the highest black jail population in the most recent recorded year?
summary_info$highest_black_jail <- incarcerations %>%
  filter(year==max(year)) %>%
  filter(black_jail_pop==max(black_jail_pop, na.rm = TRUE)) %>%
  pull(state)

#6. Which state has the highest white jail population in the most recent recorded year?
summary_info$highest_white_jail <- incarcerations %>%
  filter(year==max(year)) %>%
  filter(white_jail_pop==max(white_jail_pop, na.rm = TRUE)) %>%
  pull(state)

#7. What is the average black jail population among all counties in the most recent recorded year?
summary_info$avg_black_jail_pop <- incarcerations %>%
  filter(year==max(year)) %>%
  summarize(avg_jail = round(mean(black_jail_pop, na.rm = TRUE), digits=2)) %>%
  pull(avg_jail)

# A chart that shows trends over time for a variable of your choice
# This chart shows the the Los Angeles County jail population by different races over time
la_race_jail_overtime <- incarcerations %>%
  filter(county_name=="Los Angeles County") %>%
  filter(year>=1985, na.rum=TRUE) %>%
  select(year, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  na.omit()

la_race_jail_overtime_mod <- melt(la_race_jail_overtime, id.vars="year", variable.name="race")

time_chart <- ggplot(la_race_jail_overtime_mod, aes(x=year, y=value, color=race)) +
  geom_line() +
  labs(
    title = "Race Jail Population Over Time in Los Angeles County",
    x = "Year",
    y = "Total Jail Population",
    color = "Race"
  ) + 
  scale_color_discrete(labels = c("Black Jail Population", 
                                 "Latinx Jail Population",
                                 "Native Jail Population",
                                 "White Race Jail Population",
                                 "Other Jail Population"))
time_chart

# A chart that compares two variables to one another
# This chart shows the comparison between white and black jail population in Los Angeles County
la_black_total <- incarcerations %>%
  filter(county_name=="Los Angeles County") %>%
  filter(year==max(year)) %>%
  select(year, black_jail_pop, white_jail_pop) %>%
  na.omit()

la_black_white_mod <- melt(la_black_total, id.vars="year", variable.name="race") %>%
  arrange(desc(race)) %>%
  mutate(prop = round(value / sum(value) *100, 2))
  
comparison_chart <- ggplot(la_black_white_mod, aes(x = "", y = prop, fill = race)) +
  geom_col(width = 1) +
  coord_polar("y", start=0) +
  labs(
    title = "White vs. Black Jail Population Proportion in Los Angeles County in 2018",
    x = "",
    y = "",
    fill = "Race"
  ) +
  scale_fill_discrete(labels = c("Black Jail Population", 
                                  "White Jail Population")) +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5))
comparison_chart

# A map that shows how your measure of interest varies geographically
# This is a map of California, comparing black jail populations across different counties
incarcerations_mod <- incarcerations %>%
  filter(year==max(year))

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

map_data <- county_shapes %>%
  left_join(incarcerations_mod, by="fips") %>%
  filter(state=="CA" & county_name != "Unknown")

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

ca_jail_map <- ggplot(data = map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.1        
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$total_jail_pop)), na.value="white", low="#132B43", high="red") +
  labs(
    title = "California State County Black Jail Populations",
    fill = "Black Jail Population") +
  blank_theme

ca_jail_map
