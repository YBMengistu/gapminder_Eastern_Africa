library(tidyverse)
library(dslabs)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(scales)

#Extracting Eastern Africa data-set from the gapminder in dslabs
Eastern_Africa <- gapminder %>% 
  group_by(region) %>% 
  filter(region=="Eastern Africa")

#Viewing the extracted data-set
View(Eastern_Africa)

#Let us look for long term patterns and trends in the data-set for life_expectancy
Eastern_Africa %>% filter(year %in% c(1960, 2010)) %>% 
  ggplot(aes(x=life_expectancy, y=fct_reorder(country, life_expectancy), 
             fill=country)) +
  geom_col() +
  theme_bw() + 
  theme(legend.position = "none") + 
  facet_wrap(~year) +
  labs(title = "Eastern Africa life expectancy in 1960 Vs 2010", 
       y="Eastern African Country", subtitle = "Life expectancy has increased", 
       caption = "gapminder")

#Let us compute dollars per day for the Eastern African countries
Eastern_Africa <- Eastern_Africa %>% 
  mutate(dollar_per_day=(gdp/population)/365)

#Let us see the trend on the variation of dollars per day by making a box plot
Eastern_Africa %>% filter(!country %in% c("Seychelles", "Mauritius")) %>% 
  ggplot(aes(x=dollar_per_day, y=fct_reorder(country, dollar_per_day), 
             fill=country)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  labs(y="Country", x="Dollars_per_day", 
       title = "Eastern Africa dollars per day", 
       caption = "Data Source:gapminder", 
       subtitle = "Most of them are under 2 dollars per day") + 
  theme_bw() + 
  theme(legend.position = "none")

#Analyzing long term changes in infant mortality in Eastern Africa
Eastern_Africa %>%  filter(year %in% c(1960, 2010) & 
                             !country %in% c("Eritrea", "Djibouti")) %>% 
  ggplot(aes(x=infant_mortality, y=fct_reorder(country, infant_mortality), 
             fill=country)) + 
  geom_col() +
  theme_bw() + 
  theme(legend.position = "none") + 
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Infant Mortality in 1960 Vs 2010 of E.Africa", 
       y="Country",  caption = "gapminder data")

#Analyzing population versus gdp in 1960 and 2010 for Eastern Africa
Eastern_Africa %>%  filter(year %in% c(1960, 2010)) %>% 
  ggplot(aes(x=population, y=gdp, color=country, label=country)) + 
  geom_point() +
  geom_text_repel() +
  scale_y_continuous(labels = label_comma()) +
  theme_bw() + 
  theme(legend.position = "none") + 
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Population Vs gdp in 1960 & 2010 of E.Africa", 
       caption = "gapminder data")

#How does the population of Eastern Africa change over the years?
Eastern_Africa %>% filter(year %in% c(1960, 2010)) %>% 
  ggplot(aes(x=population, y=fct_reorder(country, population))) + 
  geom_col(aes(fill=country)) + 
  scale_x_continuous(labels = label_comma()) + 
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~year, scales = "free_x") +
  labs(title = "Population of Eastern Africa in 1960 and 2010", 
       subtitle = "Some countries have doubled their population",
       caption = "gapminder data", y="Country", x="Population in millions")

#Life expectancy change of Eastern Africa countries over time
Eastern_Africa %>% filter(year %in% c(1960, 2010)) %>% 
  ggplot(aes(x=life_expectancy, y=fct_reorder(country, life_expectancy))) +
  geom_col(aes(fill=country)) +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~year, scales = "free_x") +
  labs(title = "Life expectancy change over time in E.Africa", 
       y="Country", caption = "gapminder data")
  