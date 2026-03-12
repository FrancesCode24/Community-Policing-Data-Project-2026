# Public Interest Data
# Make smaller dataset for in-class example and problem set
# 2026-01-08 mpc

library(tidyverse)
library(janitor)

va_cpd <- read_csv("data/vacpd_20260225.csv") %>% 
  clean_names()

# Cville ----
cvl_cpd <- va_cpd %>% 
  filter(jurisdiction == "Charlottesville (103)") %>% 
  mutate(date = mdy(stop_date),
         year = year(date),
         age = if_else(age == "Over 98 Years", "99", age),
         age = as.numeric(age),
         across(c(reason_for_stop, person_type, action_taken, residency), factor))

summary(cvl_cpd$date)
cvl_cpd %>% count(agency_name)
cvl_cpd %>% count(year)
cvl_cpd %>% count(force_used_by_officer)
cvl_cpd %>% count(force_used_by_subject)
cvl_cpd %>% group_by(residency) %>% 
  summarize(n = n()) %>% 
  mutate(perc = n/sum(n)*100)
  
saveRDS(cvl_cpd, "data/cvl_cpd.RDS")

# Albemarle ----
alb_cpd <- va_cpd %>% 
  filter(jurisdiction == "Albemarle CO (002)") %>% 
  mutate(date = mdy(stop_date),
         year = year(date),
         age = if_else(age == "Over 98 Years", "99", age),
         age = as.numeric(age),
         across(c(reason_for_stop, person_type, action_taken, residency), factor))

summary(alb_cpd$date)
alb_cpd %>% count(agency_name)
alb_cpd %>% count(year)
alb_cpd %>% count(force_used_by_officer)
alb_cpd %>% count(force_used_by_subject)
alb_cpd %>% group_by(residency) %>% 
  summarize(n = n()) %>% 
  mutate(perc = n/sum(n)*100)

saveRDS(alb_cpd, "data/alb_cpd.RDS")

# agency, year, reason, action, residency
