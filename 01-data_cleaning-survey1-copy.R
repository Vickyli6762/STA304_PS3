#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/
# Author: Junwen Yuan, Fei Yang, Ruoqiuyan Zhang, Yuxin Li
# Data: 31 October 2020
# Contact: junwen.yuan@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/lizyuan/Desktop/ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("/Users/lizyuan/Desktop/ps3/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
    state,
         education,
         race_ethnicity,
         gender,
         age,
         household_income,
         gender)


reduced_data<-
  reduced_data %>%
  #the following three groups have a sd of 500 if included in the data, so I must eliminate it
  filter(race_ethnicity!="Pacific Islander (Guamanian)") %>% 
  filter(race_ethnicity!="Pacific Islander (Samoan)") %>% 
  filter(race_ethnicity!="Pacific Islander (Other)") %>% 
  #binary, whether trump
  mutate(vote_trump =  
           ifelse(vote_2020=="Donald Trump", 1, 0))%>%
  #binary, whether biden
  mutate(vote_biden=
           ifelse(vote_2020=="Joe Biden",1,0)) %>%
  #binary of finishing college or higher degree of education 
  mutate(finish_college=
           ifelse((education=="College Degree (such as B.A., B.S.)" | education=="Completed some graduate, but no degree"),1,0)) %>% 
  #old enough to vote
  filter(age>="18") %>% 
  #different age group
  mutate(age_group=
           ifelse((age>=18 &age<25),"19-24",ifelse(age>=25& age<45,"25-44",
                                                   ifelse(age>=45&age<65,"45-65",
                                                          "older than 65"
                                                          )))) %>% 
  filter(age_group!="underage") %>% 
  #make race_ethicity varibale similar to the one in census data, so the model could fit
  mutate(race=
           ifelse((race_ethnicity=="White"),"White",
                  
                  ifelse(race_ethnicity=="Black, or African American","Black/African American/Negro",
                         ifelse(race_ethnicity=="Asian (Chinese)","Chinese",
                                ifelse(race_ethnicity=="Asian (Japanese)","Japanese",
                                       ifelse((race_ethnicity=="Asian (Asian Indian)"|race_ethnicity=="Asian (Filipino)"|race_ethnicity=="Asian (Korean)"|
                                                 race_ethnicity=="Asian (Vietnamese)"|race_ethnicity=="Asian (Other)"|race_ethnicity=="Pacific Islander (Native Hawaiian)")
                                              ,"Other Asian or Pacific Islander",
                                              ifelse(race_ethnicity=="American Indian or Alaska Native",
                                                     "American Indian or Alaska Native","Some other race")
                                              ) )))                  )) %>% 
  #different income group
mutate(income=
         ifelse(household_income=="Less than $14,999"|household_income=="$15,000 to $19,999"|household_income=="$20,000 to $24,999","Less than 25,000",
                ifelse(household_income=="$25,000 to $29,999"|household_income=="$30,000 to $34,999"|household_income=="$35,000 to $39,999"|household_income=="$40,000 to $44,999"
                       |household_income=="$45,000 to $49,999"|household_income=="$50,000 to $54,999"|
                         household_income=="$55,000 to $59,999"|household_income=="$60,000 to $64,999"|household_income=="$65,000 to $69,999"|
                         household_income=="$70,000 to $74,999","25,000 to 74,999",
                       ifelse(household_income=="$250,000 and above","above 250,000",
                              ifelse(household_income=="$125,000 to $149,999"|household_income=="$150,000 to $174,999"
                                     |household_income=="$175,000 to $199,999"|household_income=="$200,000 to $249,999","125,000 to 250,000","75,000 to 125,000")
                              )
                       )
                )
         )


reduced_data<-reduced_data %>% mutate(racial_group=race) %>%  select(vote_trump,vote_biden,finish_college,age_group,racial_group,income, gender)
                                                
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

