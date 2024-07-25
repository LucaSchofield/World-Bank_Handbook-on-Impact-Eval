setwd("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data")

data <- read.csv("hh_98.csv")

#download packages 
install.packages("dplyr")
library(dplyr)


#view the data and confirm it is reading 
data

#12. Randomised Impact Evaluation 
#12.1 impacts of Program placement in villages 

#create the log form of two variables, outcome 'exptot' and hh land before joining the Program 'hhland' which is changed to acre from decimial
#create a dummy variable for the microcredit program placement in villages
#create one for male programs and another for female programs 

data.df <- data %>%
  mutate(lexptot = log(1 + exptot)) %>%
  mutate(lnland = log(1 + hhland / 100)) %>%\
  mutate(vill = thanaid * 10 + villid) %>%
  group_by(vill) %>%
  mutate(progvillm = max(dmmfd), progvillf = max(dfmfd))
