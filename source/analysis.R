library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)

# data import

veraRawCounty  <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
veraDataCounty <- read.csv(veraRawCounty)

veraRawJurisdiction <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv"
veraDataJurisdiction <- read.csv(veraRawJurisdiction)

# Trend Chart - US Prison & Jail population by race 
# Columns to use
  # year
  # aapi_jail_pop 
  # aapi_prison_pop
  # black_jail_pop
  # black_prison_pop
  # latinx_jail_pop
  # latinx_prison_pop
  # native_jail_pop
  # native_prison_pop
  # white_jail_pop
  # white_prison_pop
  # other_race_jail_pop
  # other_race_prison_pop

prisonPopRace <- veraDataCounty %>%
  group_by(year) %>%
  mutate(year_aapi_JoP_pop = (sum(aapi_jail_pop, na.rm = TRUE) + sum(aapi_prison_pop, na.rm = TRUE))/(sum(aapi_pop_15to64, na.rm = TRUE)) * 100,
         year_black_JoP_pop = (sum(black_jail_pop, na.rm = TRUE) + sum(black_prison_pop, na.rm = TRUE))/(sum(black_pop_15to64, na.rm = TRUE)) * 100,
         year_latinx_JoP_pop = (sum(latinx_jail_pop, na.rm = TRUE) + sum(latinx_prison_pop, na.rm = TRUE))/(sum(latinx_pop_15to64, na.rm = TRUE)) * 100,
         year_native_JoP_pop = (sum(native_jail_pop, na.rm = TRUE) + sum(native_prison_pop, na.rm = TRUE))/(sum(native_pop_15to64, na.rm = TRUE)) * 100,
         year_white_JoP_pop = (sum(white_jail_pop, na.rm = TRUE) + sum(white_prison_pop, na.rm = TRUE))/(sum(white_pop_15to64, na.rm = TRUE)) * 100) %>%
  distinct(year, year_aapi_JoP_pop, year_black_JoP_pop, year_latinx_JoP_pop, year_native_JoP_pop, year_white_JoP_pop) %>%
  filter(year >= 1990)

ggplot(prisonPopRace, aes(x=year)) + 
  geom_line(aes(y = year_aapi_JoP_pop), color = "red") + 
  geom_line(aes(y = year_black_JoP_pop), color="blue") + 
  geom_line(aes(y = year_latinx_JoP_pop), color="darkgreen") + 
  geom_line(aes(y = year_native_JoP_pop), color="orange") + 
  geom_line(aes(y = year_white_JoP_pop), color="black") + 
  xlab("Year") + 
  ylab("Percentage of Population in Prison or Jail")

# Compare Chart - Total Pop vs Total Prison & Jail Pop over Time
# Columns to use
  # year
  # total_pop
  # total_jail_pop 
  # total_prison_pop



# Map - White vs. Non-white prison & Jail proportions by State
# Columns to use
  # State
  # aapi_jail_pop 
  # black_jail_pop
  # latinx_jail_pop
  # native_jail_pop
  # white_jail_pop
  # other_race_jail_pop

                       
                       
                       
                       
                       
                       