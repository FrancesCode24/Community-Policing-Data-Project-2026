#Community Policing Data 
#Team Berries
#Date: March 2026

#Start here----
#Project Objective----
# To analyze ... 



#Load the data----
library(tidyverse)
library(janitor)

fbi_lee <- read_csv("~/Desktop/R class materials/community policing/data cp/fbi_lee_virginia.csv")

lee_1960_2024 <- read_csv("~/Desktop/R class materials/community policing/data cp/lee_1960_2024.csv")

vacpd <- read_csv("~/Desktop/R class materials/community policing/data cp/vacpd_20260225.csv") %>%
  clean_names 

glimpse(vacpd)

vacpd <- vacpd %>%
  mutate(date = mdy(STOP_DATE), 
         year = year(date))

#How many stops and force used by officer per year statewide?
vacpd %>%
  group_by(year) %>%
  summarize(
    tot_stops = n(), 
    force_by_officer = sum(FORCE.USED.BY.OFFICER == "Y", na.rm = TRUE), 
    force_rate = FORCE.USED.BY.OFFICER / tot_stops * 100)

glimpse(vacpd)




