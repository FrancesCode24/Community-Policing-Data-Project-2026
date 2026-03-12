# From FBI crime data exploer
# Law Enforcement Employees Data
# https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/le/pe
# 2026-01-02 mpc

# Setup ----
library(tidyverse)

# Download and read ----
# From https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/le/pe
# Scroll down to "Resources"
# Click "Download Participation and Population Data"
# Click "Law Enforcement Employees Data"
# Click "Download" 
# lee_1960_2024.csv

lee <- read_csv("download/lee_1960_2024.csv")

# filter to VA
va_lee <- lee %>% 
  filter(state_abbr == "VA")

# review
va_lee %>% 
  filter(str_detect(county_name, "CHARLOTTESVILLE|ALBEMARLE")) %>% 
  ggplot(aes(x = data_year, y = officer_ct, color = ori)) +
    geom_line()
  
va_lee %>% 
  filter(str_detect(county_name, "CHARLOTTESVILLE|ALBEMARLE")) %>% 
  group_by(data_year) %>% 
  summarize(officers = sum(officer_ct)) %>% 
  ggplot(aes(x = data_year, y = officers)) +
  geom_line()

# save ---- 
write_csv(va_lee, "data/fbi_lee_virginia.csv")
