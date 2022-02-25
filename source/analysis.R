library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

# data import

veraRawCounty  <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
veraDataCounty <- read.csv(veraRawCounty)

# veraRawJurisdiction <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv"
# veraDataJurisdiction <- read.csv(veraRawJurisdiction)

# --------------- Trend Chart ---------------

prisonPopRace <- veraDataCounty %>%
  group_by(year) %>%
  mutate(year_aapi_JoP_pop = (sum(aapi_jail_pop, na.rm = TRUE) + sum(aapi_prison_pop, na.rm = TRUE))/(sum(aapi_pop_15to64, na.rm = TRUE)) * 100,
         year_black_JoP_pop = (sum(black_jail_pop, na.rm = TRUE) + sum(black_prison_pop, na.rm = TRUE))/(sum(black_pop_15to64, na.rm = TRUE)) * 100,
         year_latinx_JoP_pop = (sum(latinx_jail_pop, na.rm = TRUE) + sum(latinx_prison_pop, na.rm = TRUE))/(sum(latinx_pop_15to64, na.rm = TRUE)) * 100,
         year_native_JoP_pop = (sum(native_jail_pop, na.rm = TRUE) + sum(native_prison_pop, na.rm = TRUE))/(sum(native_pop_15to64, na.rm = TRUE)) * 100,
         year_white_JoP_pop = (sum(white_jail_pop, na.rm = TRUE) + sum(white_prison_pop, na.rm = TRUE))/(sum(white_pop_15to64, na.rm = TRUE)) * 100) %>%
  distinct(year, year_aapi_JoP_pop, year_black_JoP_pop, year_latinx_JoP_pop, year_native_JoP_pop, year_white_JoP_pop) %>%
  filter(year >= 1990)

incarceratedPopProp <- ggplot(prisonPopRace, aes(x=year)) + 
  geom_line(aes(y = year_aapi_JoP_pop, color = "Asian American / Pacific Islander")) + 
  geom_line(aes(y = year_black_JoP_pop, color="Black")) + 
  geom_line(aes(y = year_latinx_JoP_pop, color="Latinx")) + 
  geom_line(aes(y = year_native_JoP_pop, color="Native")) + 
  geom_line(aes(y = year_white_JoP_pop, color="White")) + 
  labs(title = "Percentage of US Population in Prison or Jail, by Race", 
       x = "Year", 
       y = "Percentage of Population",
       color = "Demographic",
       subtitle = "1990 to 2018")

ggplotly(incarceratedPopProp)

# --------------- Compare Chart ---------------

prisonPopGenPop <- veraDataCounty %>%
  group_by(year) %>%
  mutate(year_gen_pop = sum(total_pop, na.rm = TRUE),
         year_prison_pop = sum(total_jail_pop, na.rm = TRUE) + sum(total_prison_pop, na.rm = TRUE)) %>%
  distinct(year, year_gen_pop, year_prison_pop)

prisonPopGenPopGraph <- ggplot(prisonPopGenPop, aes(x=year_gen_pop)) + 
  geom_line(aes(y = year_prison_pop)) + 
  labs(title = "US General Population vs. Incarcerated Population Between 1970 and 2018", 
       x = "General Population", 
       y = "Incarcerated Population")

ggplotly(prisonPopGenPopGraph)

# --------------- Map ---------------
  # Proportion of black incarcerated persons by state
  # Columns to use
    # State
    # black_jail_pop
    # black_prison_pop
    # total_jail_pop 
    # total_prison_pop


# --------------- Values ---------------

  # max incarceration rates
aapiMaxRate <- max(prisonPopRace$year_aapi_JoP_pop)
blackMaxRate <- max(prisonPopRace$year_black_JoP_pop)
latinxMaxRate <- max(prisonPopRace$year_latinx_JoP_pop)
nativeMaxRate <- max(prisonPopRace$year_native_JoP_pop)
whiteMaxRate <- max(prisonPopRace$year_white_JoP_pop)

  # tells us that for every 59.17 people we add, one of them will go to prison
rocGenPrisonPop <- 1/((prisonPopGenPop$year_prison_pop[19] - prisonPopGenPop$year_prison_pop[1]) / (prisonPopGenPop$year_gen_pop[19] - prisonPopGenPop$year_gen_pop[1]))
