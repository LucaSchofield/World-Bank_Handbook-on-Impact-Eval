###########################
# World Eval Handbook 
# Double-Difference / DiD Method 
###########################


library(dplyr)
library(foreign)    
library(ggplot2)
library(survey)
library(VIF)
library(plm)
library(car)
library(Matching)

hh_9198<- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_9198.csv")
hh_9198

#use panel data to determine comparison of observable characteristics 

#########################
# Simplest implementation 
#########################

#generate exptot0 for year 0
hh_9198 <- hh_9198 %>%
  mutate(exptot0 = ifelse(year == 0, exptot, NA))

#calculate max of exptot grouped by nh 
hh_9198 <- hh_9198 %>%
  group_by(nh) %>%
  mutate(exptot91 = max(exptot0, na.rm = TRUE)) %>%
  ungroup()

#keep only rows where year == 1
hh_9198 <- hh_9198 %>%
  filter(year == 1)

#generate lexptot91 and lexptot98 
hh_9198 <- hh_9198 %>%
  mutate(lexptot91 = log(1 + exptot91),
         lexptot98 = log(1 + exptot),
         lexptot9891 = lexptot98 - lexptot91)

head(hh_9198)



################
# t-test
################


t_test <- t.test(lexptot9891 ~ dfmfd, data = hh_9198)
t_test
