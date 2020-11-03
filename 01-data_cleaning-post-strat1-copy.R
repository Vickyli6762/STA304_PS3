#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Junwen Yuan, Fei Yang, Ruoqiuyan Zhang, Yuxin Li
# Data: 31 October 2020
# Contact: junwen.yuan@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/lizyuan/Desktop/ps3")
raw_data <- read_dta("/Users/lizyuan/Desktop/ps3/usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(degfield,
         ftotinc,
         sex, 
         age,
         race) %>% 
  #ftotinc is family income, and these number are not actual income, but speical indication, so need to the eliminated
  filter(ftotinc!="-000001") %>% 
  filter(ftotinc!="0000000") %>% 
  filter(ftotinc!="9999998") %>% 
  filter(ftotinc!="9999999") 


reduced_data<-reduced_data %>% mutate(income=
 #different level of income group                      
  ifelse(ftotinc<25000,"Less than 25,000",
         ifelse(ftotinc>=25000&ftotinc<75000,"75,000 to 125,000",
                ifelse(ftotinc>=75000&ftotinc<125000,"75,000 to 125,000",
                       ifelse(ftotinc>=125000&ftotinc<250000,"125,000 to 250,000","above 250,000")
                       )
                )
         )
  )
#must be >=18 to vote and divide different age group
reduced_data<-reduced_data %>% mutate(AGE=as.integer(age)-1)%>% filter(AGE>="18") %>% 
  mutate(age_group=
           ifelse((AGE>=18 &AGE<25),"19-24",ifelse(AGE>=25& AGE<45,"25-44",
                                                   ifelse(AGE>=45&AGE<65,"45-65","older than 65"
                                                          
                                                   )))) 
#make the first letter upper case to match survey data
reduced_data<-reduced_data %>% mutate(gender=ifelse(sex=="male","Male","Female")) %>% 
  #binary of finishing college or higher degree of education 
  mutate(finish_college=ifelse(degfield=="n/a","0","1")) %>% 
  #make race varibale similar to the one in survey data, so the model could fit
  mutate(racial_group=
           ifelse((race=="white"),"White",
                  ifelse(race=="black/african american/negro","Black/African American/Negro",
                         ifelse(race=="chinese","Chinese",
                                ifelse(race=="japanese","Japanese",
                                       ifelse(race=="other asian or pacific islander"
                                              ,"Other Asian or Pacific Islander",
                                              ifelse(race=="american indian or alaska native",
                                                     "American Indian or Alaska Native",
                                                     "Some other race")
                                       ) )))                  )) 
reduced_data %>% select(income,age_group,gender,finish_college,racial_group)

#group different cell, in order to the following post-Stratification
reduced_data <- reduced_data %>% select(income,age_group,gender,finish_college,racial_group) %>% 
  count(income,age_group,gender,finish_college,racial_group) %>% 
  group_by(income,age_group,gender,finish_college,racial_group)




# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         