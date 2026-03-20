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

#fbi_lee data----

#Only include 2020-2025 data
fbi_lee <- fbi_lee %>% 
            filter(data_year > 2019)

fbi_lee %>% 
  count(agency_type_name) %>% 
  arrange(desc(n))

----------------------------------------------------------------------------------------------------
  
#Frances jumped in here 3-19 3pm

  #reminder, fbi_lee is male_officer_ct, male_civilian_ct, male_total_ct, female_officer_ct, female_civilian_ct, etc. which describes each ORI (law agency place) and their count of officers and their subsequent descriptions
  #useful for if we want to observe use of force by male or female predominant offices? 
  
#Using census data ----
  #to learn about the relevant communities in Virginia being policed
#https://walker-data.com/tidycensus/articles/basic-usage.html


#2020 DATA----
#had to google this
v20 <- load_variables(2020, "acs5", cache = TRUE)
View(v20)

va_demog <- get_acs(
  geography = "county",
  state = "VA",
  variables = c(
    tot_pop = "B01003_001",
      white = "B02001_002",
      Black = "B02001_003",
        Hispanic = "B03003_003",
        asian = "B02001_005",
        med_income = "B19013_001",
        poverty = "B17001_002",
        college_educ = "B15003_022"
),
year = 2020,
output= "wide"
)
#FIPS code = 51 for VA  

#join ACS data with vacpd and fbi_lee for example?
  #join via jurisdiction/ county... 
  #fbi_lee has pub_agency_name and county_name
  #vacpd has jurisdiction all capitals 
#had to look up how to do this, but useful 
vacpd <- vacpd %>%
  mutate(
    county_clean = jurisdiction %>%
      str_to_upper() %>%
      str_replace(" CO ", "") %>%
      str_sub(end = -6) %>% 
      str_trim())
#Maybe ignore this actually ^ -Win

#Combine using agency names
cbind(sort(unique(vacpd$agency_name)),sort(unique(fbi_lee$pub_agency_name)))

#Need to edit this to be agency name, when we get agency names to match
join_countyname <- fbi_lee %>%
  left_join(vacpd, by = c("county_name" = "county_clean")) #Counties don't exactly match, so the merge isn't working


#fbi_lee into vacpd: see last row of vacpd now includes county_clean
---------------------------------------------------------------------------------------------------
  
#STOPS DATA-----
#Which counties have the most stops by an officer in 2020?----
vacpd %>%
  filter(year == 2020) %>%
  count(county_clean) %>%
  arrange(desc(n))

#Answer  = 
  #Fairfax County (493,377 stops), 
  #Henrico County (236,669 stops), 
  #Virginia Beach (221,665 stops), 
  #Prince William County (221,038 stops), 
  #and Loudoun County (200,377 stops)

#By gender and by race, proportion of each group stopped in 2020?
glimpse(vacpd)
vacpd %>%
  filter(year == 2020) %>%
  summarize(
    total_stops = n(), 
    pct_male = mean(gender == "Male", na.rm = TRUE), 
    pct_female = mean(gender == "Female", na.rm = TRUE), 
    pct_white = mean(race == "White", na.rm = TRUE), 
    pct_black = mean(race == "Black or African American", na.rm = TRUE), 
    pct_hispanic = mean(ethnicity == "Hisapnic or Latino")
  )

  #Answer = In the year 2020, there were 465,356 total stops. 63.3% were male, 36.3% were female, 62.4% were white, 31% were black and 0 were Hispanic. 

#More specific, combining gender and race in 2020?
vacpd %>%
  filter(year == 2020) %>%
  summarize(
    total_stops = n(), 
    pct_black_male = mean(gender == "Male" & race == "Black or African American", na.rm = TRUE), 
    pct_black_fem = mean(gender == "Female" & race == "Black or African American", na.rm = TRUE), 
    pct_white_male = mean(gender == "Male" & race == "White", na.rm = TRUE), 
    pct_white_fem = mean(gender == "Female" & race == "White", na.rm = TRUE), 
    pct_hispanic_male = mean(gender == "Male" & ethnicity == "Hispanic", na.rm = TRUE), 
    pct_hispanic_fem = mean(gender == "Male" & ethnicity == "Hispanic", na.rm=TRUE)
  )


  #Answer = In 2020, all of the stops included 19.2% Black men, 11.7% Black women, 39.7% white men, 22.6% white women, and 0 Hispanic people stopped of either gender. 

------------------------------------------------------------------------------------
#OFFICER DETAILS----
#What is the officer make up of each of the localities with the highest number of stops in 2020? 

#Va statewide officer totals in 2020 
fbi_lee %>%
  filter(data_year == 2020) %>%
  summarize(
    tot_male_officers = sum(male_officer_ct, na.rm = TRUE), 
    tot_female_officers = sum(female_officer_ct, na.rm = TRUE), 
    tot_officers = tot_male_officers + tot_female_officers, 
    pct_female = tot_female_officers / tot_officers*100)

#Answer = 19,875 officers in Virginia, 16,967 were male, 2,908 were female, so 14.6% of total officers statewide were female in 2020

#Fairfax Officer Make up in 2020
fbi_lee %>%
  filter(data_year == 2020) %>%
  filter(county_name == "FAIRFAX") %>%
  summarize(
    tot_male_officers = sum(male_officer_ct, na.rm = TRUE), 
    tot_female_officers = sum(female_officer_ct, na.rm = TRUE), 
    tot_officers = tot_male_officers + tot_female_officers, 
    pct_female = tot_female_officers / tot_officers*100
  )
#Answer = 1,353 male officers, 261 female officers (1,614 total) so 16.2% are female in 2020

#Note- F needs to finish ----
#CONTINUE WITH THE OTHER LOCALITIES (2020)

---------------------------------------------------------------------------------------------
#What localities had force used by officers the most in 2020? 
vacpd %>%
  filter(force_used_by_officer == "Y") %>%
  count(county_clean) %>%
  arrange(desc(n))

# FAIRFAX CO (029)         2815
# CHESTERFIELD CO (020)     559
# VIRGINIA BEACH (125)      421
# CHARLOTTESVILLE (103)     401
# HENRICO CO (043)          293
# ARLINGTON CO (000)        234
# CAMPBELL CO (015)         234
# ROANOKE CITY (121)        179
# CHESAPEAKE (126)          173
# PRINCE WILLIAM CO (076)   173
  #Yes, Fairfax County also had the highest use of force by officer.
-------------------------------------------------------------------------------------------------
#Is there relation between use of force, number of stops, and woman/male officer make up in Fairfax County?----
  #Answer = I would like to learn more about if the make up of the police team is correlated with less stops or less use of force over time? 

#2021 DATA----
#TBD
------------------------------------------------------------------------------------------------
#Summary of what I (Frances) did above 3/19
  #I got an API key for the tidycensus library & found the codes for the different groups of demographics we can look at per county.
    #like if wealthier counties use force more or less, if more impoverished counties stop people less etc
  #2020 vacpd-- I looked into 2020 data 
    #Saw which counties in Va had most stops & most use of force by officer 
    #Saw total # of stops and what the % per groups of race and gender were 
  #2020 fbi_lee -- looked into statewide make up of officers (women, men, and percent women in 2020)
    #Saw make up of Fairfax County (same)
  
#Need to do before 3/26 (Frances)----
  #I want to see the statewide average of use of force (vacpd) from 2020 thru 2024 (outside influence of Biden administration, BLM protests)... this dataset has 2020-2025 but fbi_lee only has 2020-2024
  #I want to see county by county wealth distrib of pop (CENSUS) alongside use of force by county (vacpd)
  #I want to see statewide, racial differences between the officers (fbi_lee) and those who received force by officers over the years (2020-2024)
  
------------------------------------------------------------------------------------------------
#Win started here ----
#Stops over time

hist(vacpd$date, breaks = 25)

#Quarters where stops are prevalent
vacpd_byquarter <- vacpd %>% 
                  group_by(year, quarter) %>% 
                  summarize(
                    tot_stops = n(), 
                    force_by_officer = sum(force_used_by_officer == "Y", na.rm = TRUE), 
                    force_rate = force_by_officer / tot_stops * 100)

#Frances note -  I renamed it vacpd_byquarter if that's ok! 

#By quarter of each year 
vacpd_byquarter %>% 
  ggplot(aes(x = year, y = tot_stops)) +
  geom_line(aes(group = year)) +
  geom_point(aes(color = as.factor(quarter)), size = 3) +
  scale_color_discrete(palette = "Accent") +
  coord_flip()

-------------------------------------------------------------------------------------------------
#Use of force by officer over time
  hist(
  (vacpd %>% 
     filter(force_used_by_officer == "Y", na.rm = TRUE))$date
  , breaks = 25)

-----------------------------------------------------------------------------------------------
#end of code -----
saveRDS(vacpd, "vacpd.RDS")
saveRDS(fbi_lee, "fbi_lee.RDS")

