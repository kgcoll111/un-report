library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

# %>% has a shortcut of: shift control m
  
gapminder_data_summarized <- gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

#danger 1: this clears gapminder_data
gapminder_data <- gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data %>%
  summarize(recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>%  #have to use 2 equal signs
  summarize(average = mean(lifeExp))

gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(average = mean(gdpPercap))

#group_by command

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average=mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average=mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average=mean(lifeExp), min=min(lifeExp))

#mutate() function

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap) #adds a column for GDP

  #adds a second column for populationInMillions
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop/1000000)

#select() function - to remove columns
gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

  #the following two do the same
gapminder_data %>% 
  select(-pop, -gdpPercap)

gapminder_data %>% 
  select(country, continent, year, lifeExp) #puts it in order listed

gapminder_data %>% 
  select(year, starts_with('c')) #starts with c = country and continent

gapminder_data %>% 
  select(ends_with('p'))

#opens help window for select()
?select

#pivot_wider gives a wider data frame
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)


#new dataset
getwd()

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

temp <- read_csv("co2-un-data.csv")

read_csv("co2-un-data.csv", skip = 1)

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2, 
                                col_names = c("region", "country", "year", "series", 
                                              "value", "footnotes", "source"))

co2_emissions_dirty

read_csv("co2-un-data.csv", skip = 1) %>% 
  rename(country=...2) #renames column 2 from the default ...2

co2_emissions_dirty %>% 
  select(country, year, series, value)

#recodes long descriptions with shorter ones
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
        "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
        "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

#puts total_emissions and per_capita_emissions in 2 columns
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)

#counts how much data is in each year
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)



#Challenge: pull out data from 2005 and drop out year column
#following 2 solutions both work
co2_emissions_dirty %>% 
  filter(year == 2005) %>% 
  select(country, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)

#solution 2
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)



#puts it into a new object
co2_emissions <- co2_emissions_dirty %>% 
  filter(year == 2005) %>% 
  select(country, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)



#Joins 2 datasets, function is smart and knows to join by country
inner_join(gapminder_data, co2_emissions)

#you could also explicitly tell it by the following
inner_join(gapminder_data, co2_emissions, by="country")


#to filter by 2 columns:
# gapminder_data <- inner_join(gapminder_data_2007, co2_emissions, by=c("country", "year"))


gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by="country")

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
         geom_point() + 
         labs(x = "GDP (per capita)", y = "CO2 emitted per capita", 
              title = "Strong Association between GDP per capita and CO2 Emissions")
  