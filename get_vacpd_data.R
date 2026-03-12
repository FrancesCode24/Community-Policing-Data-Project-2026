# Virginia Community Policing Data
# https://data.virginia.gov/dataset/community-policing-data
# 2026-01-02 mpc initial
# 2026-02-25 mpc updated

# Setup ----
library(tidyverse)

# Download/Read ----
url <- "https://data.virginia.gov/dataset/de833a43-7019-444c-8384-9e0cf5255140/resource/60506bbb-685f-4360-8a8c-30e137ce3615/download/community-policing-data.csv"
va_cpd <- read_csv(url) 

# Save ----
write_csv(va_cpd, "data/vacpd_20260225.csv")

# Brief explore ----
df <- read_csv("data/vacpd_20260225.csv")

# by year
df <- df %>% 
  mutate(date = mdy(STOP_DATE),
         year = year(date))
df %>% count(year)

