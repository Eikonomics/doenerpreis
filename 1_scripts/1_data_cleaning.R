library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
rm(list = ls())

gdpdata <- eurostat::get_eurostat(id = "nama_10r_3gdp")

gdpdata <-
gdpdata %>%
  filter(unit == "EUR_HAB" | unit == "MIO_EUR", 
         str_detect(geo, "DE"), 
         time == "2020-01-01") %>%
  rename(NUTS3 = geo)

nuts_pzl_lookup <-
read_delim(file = here("0_rawdata", "pc2020_DE_NUTS-2021_v4.0.csv"),  delim =  ";") %>%
  mutate(NUTS3 = str_replace_all(NUTS3, pattern = "'",replacement = "")) %>%
  mutate(CODE = str_replace_all(CODE, pattern = "'",replacement = ""))

plz2data <- readxl::read_excel(path = here("0_rawdata", "21-postleit-regionen.xlsx"), sheet = "PLZ_2-stellig", skip = 4)

plz2data <-
  plz2data %>%
  slice(-1) %>%
  select(1,4,5,6) %>%
  rename(plz2 = 1, 
         kreis = 2,
         flaeche = 3, 
         bevolkerung = 4) %>%
  filter(!is.na(flaeche))

plz2data <-
  plz2data %>%
  group_by(plz2) %>%
  summarise_at(vars(flaeche,bevolkerung ), sum)


data <- readxl::read_excel(path = here("0_rawdata", "Dönerpreisumfrage 2023 Q1 (Antworten).xlsx"))

data <-
  data %>%
  rename(date = Zeitstempel, 
         land = `[1] In welchem Bundesland liegt der Dönerladen?`, 
         preis = `[2] Wie viel Kostet ein Standard Döner ("einmal Döner mit alles") dort?`, 
         plz = `[3] Wie lautet die PLZ von besagtem Dönerladen?  ⁽²⁾`)  %>%
  select(1:4)

data <-
  data %>% 
  mutate(preis = as.numeric(preis),
         plz = as.integer(plz), 
         date = as.Date(date))

left_join(gdpdata, nuts_pzl_lookup, by = "NUTS3") %>%
  filter(!is.na(CODE)) %>%
  rename(plz = CODE) %>%
  mutate(plz = as.integer(plz)) %>%
  left_join(data, by = "plz") %>%
  filter(!is.na(land)) 



data <- 
data %>%
  ungroup() %>%
  mutate(plz2 = str_sub(plz,1,2))  %>%
  mutate(id = row_number()) %>%
  left_join(plz2data, by = "plz2") 

data %>%
  lm(formula = log(preis) ~ I(log(bevolkerung / flaeche)) + ost_west + land) %>%
  summary()

# no clear signal of shirt term price steigerung
data %>%
  filter(date < "2023-04-01") %>%
  ggplot() + 
  aes(y = preis, x = date) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  filter(date < "2023-04-01") %>%
  mutate(date = floor_date(date, "week")) %>%
  group_by(land, date) %>%
  summarise(preis = mean(preis), 
            count = n()) 

data %>%
  ggplot() + 
  aes(x = preis) +
  geom_histogram(aes(y = ..density..), bins = 15) + 
  geom_density(adjust = 5)

data %>%
  ggplot() + 
  aes(x = preis, color = land) +
  geom_density(adjust = 4)

data <- 
  data %>% 
  mutate(ost_west = if_else(land %in% c("Mecklenburg-Vorpommern", "Brandenburg", "Berlin", "Sachsen-Anhalt", "Thüringen", "Sachsen"), "ost", "west"))

data %>%
  ggplot() + 
  aes(x = preis, color = ost_west) +
  geom_density(adjust = 4)

eurostat::