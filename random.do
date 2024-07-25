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

data.df

#use t-test to calculate the average treatment effect of village program placement using equal variances 

attach(data.df)
t.test(lexptot ~ progvillf, var.equal = TRUE)
t.test(lexptot ~ progvillm, var.equal = TRUE)

#alternatively use a simplest equation to regress per capita expenditure against the villgage program dummy 
#des1 <- svydesign(id = ~X, weights = ~weight, data = data.df)

prog_place_1.1 <- lm(lexptot ~ progvillf, data = data.df)
summary(prog_place_1.1)
prog_place_2.1 <- lm(lexptot ~ progvillm, data = data.df)
summary(prog_place_2.1)
