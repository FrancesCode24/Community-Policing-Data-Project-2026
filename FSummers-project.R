#Community Policing Data 
#Team Berries
#Date: March 2026

#Start here----
#Project Objective----
  #To explore trends in Virginia police interactions with local residents, looking directly at racial disparities in stops, arrests, and use of force

#To analyze ... 


#Load the data----
library(tidyverse)
library(janitor)
library(tidycensus)

fbi_lee <- readRDS("data/fbi_lee.RDS") #Includes all VA data
vacpd <- readRDS("data/vacpd.RDS") #Includes all VA data

#vacpd data----
glimpse(vacpd)

#Clean up the variable names
vacpd <- vacpd %>% 
           clean_names()

#Specify year, month, and quarter of police stop
vacpd <- vacpd %>%
  mutate(date = mdy(stop_date), 
         year = year(date),
         month = month(date),
         quarter = quarter(date))

#How many stops and force used by officer per year statewide?
vacpd %>%
  group_by(year) %>%
  summarize(
    tot_stops = n(), 
    force_by_officer = sum(force_used_by_officer == "Y", na.rm = TRUE), 
    force_rate = force_by_officer / tot_stops * 100)



#Looking at agencies and jurisdictions. Helpful if we want to combine this with fbi data later.
unique(vacpd$agency_name) #361 unique agencies

unique(vacpd$jurisdiction) #135 jurisdictions


#fbi_lee data----

#Only include 2020-2025 data
fbi_lee <- fbi_lee %>% 
            filter(data_year > 2019)

#Looking at agencies
unique(fbi_lee$pub_agency_name) #281 agencies

fbi_lee %>% 
  count(agency_type_name) %>% 
  arrange(desc(n))


#Question

#Visuals----

#Stops over time
hist(vacpd$date, breaks = 25)

#Use of force over time
hist(
  (vacpd %>% 
    filter(force_used_by_officer == "Y", na.rm = TRUE))$date
  , breaks = 25)


#Quarters where stops are prevalent
vacpdQuarter <- vacpd %>% 
                  group_by(year, quarter) %>% 
                  summarize(
                    tot_stops = n(), 
                    force_by_officer = sum(force_used_by_officer == "Y", na.rm = TRUE), 
                    force_rate = force_by_officer / tot_stops * 100)

#Maybe not the best way to visualize this tbh
vacpdQuarter %>% 
  ggplot(aes(x = year, y = tot_stops)) +
  geom_line(aes(group = year)) +
  geom_point(aes(color = as.factor(quarter)), size = 3) +
  scale_color_discrete(palette = "Accent") +
  coord_flip()

  




