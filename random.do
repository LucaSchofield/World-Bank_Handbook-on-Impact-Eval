setwd("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data")

data <- read.csv("hh_98.csv")

#download packages 
install.packages("dplyr")
library(dplyr)
library(survey)


#view the data and confirm it is reading 
data

#12
#Randomised Impact Evaluation 
#12.1 
#impacts of Program placement in villages 

#create the log form of two variables, outcome 'exptot' and hh land before joining the Program 'hhland' which is changed to acre from decimial
#create a dummy variable for the microcredit program placement in villages
#create one for male programs and another for female programs 

data.df <- data %>%
  mutate(lexptot = log(1 + exptot)) %>%
  mutate(lnland = log(1 + hhland / 100)) %>%
  mutate(vill = thanaid * 10 + villid) %>%
  group_by(vill) %>%
  mutate(progvillm = max(dmmfd), progvillf = max(dfmfd))

data.df


#use t-test to calculate the average treatment effect of village program placement using equal variances 

attach(data.df)
t.test(lexptot ~ progvillf, var.equal = TRUE)
t.test(lexptot ~ progvillm, var.equal = TRUE)

#alternatively use a simplest equation to regress per capita expenditure against the villgage program dummy 

prog_place_1.1 <- lm(lexptot ~ progvillf, data = data.df)
summary(prog_place_1.1)
prog_place_2.1 <- lm(lexptot ~ progvillm, data = data.df)
summary(prog_place_2.1)

#the above estimates the overall impact of the village programs on the per cpaita expenditure of households.This may be different from the impact on the expenditure after holding other factors constant. 
#the following regresses the same outcome (log of per capita household expenditures) against the village program dummy plus other factors that may infl uence the expenditure.

prog_villf_3 <- lm(
  lexptot ~ progvillf + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
  data = data.df,
  weights = weight
)

summary(prog_villf_3)

prog_villm_3 <- lm(
  lexptot ~ progvillm + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
  data = data.df,
  weights = weight 
)
  
summary(prog_villm_3)

#Adjusting for other covariates there is still no significant impacts of program placement on the outcome variable
#prog_villf has a P > |t| is 0.42155, t value is -0.804 

#12.2 
#Impacts of Program participation 
# even thoigh the program assignment is random, particiaption may not be 

#calculate average treatment effect of program participation by starting with the simplest method (not equal)

t.test(lexptot ~ dmmfd)

#exploring - checking for heteroscedasticity 

library(lmtest)
model <- lm(lexptot ~ dmmfd, data = data.df)
bptest(model)

#BP test suggest rejecting the num for evidence of heterscadicitiy 

#t-test with equal variances 

t_test_equal_var <- t.test(lexptot ~ dmmfd, data = data.df, var.equal = TRUE)
print(t_test_equal_var)

#dmmfd does not have a signifcant effect on lexptot, any changes are likely due to chance 

#testing for females 

t_test_equal_var <- t.test(lexptot ~ dfmfd, data = data.df, var.equal = TRUE)
print(t_test_equal_var)

#similar, not significant effect 

#now look to include other house-hold and village-level covariates in the female particiaption equation 

model_fparticpation <- lm(lexptot ~ dfmfd, + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
                          data = data.df,
                          weights = weight 
)
summary(model_fparticpation)

#this shows the female participation is highly significant (***) in impacting  household expenditure
#p-value, f-stat shows the overall model is highly significant 

#12.3 
#Combining Program placement and participation 

plpf_model <- lm(
  lexptot ~ dfmfd + progvillf + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
  data = data.df,
  weights = weight
)
summary(ppf_model)

#results show no signficicant effect on program placement, but positive significant effect (*) of 7.3% of female program participation t=2.4 
# t-value is significantly different from 0 with a small p valueindicating strong evidence against the null 

#12.4
#Impacts of Program Participation in Program Villages
#test whether program participation matters for households living in program villages 

Pvf_data <- subset(data.df, progvillf == 1)

PpPvf_model<-lm(
  lexptot ~ dfmfd,
  data = Pvf_data,
  weights = weight
)

summary(PpPv_model)

#results show the impact of female participation in microcredit programs on household expenditure in program villages is negative by 7%

#now regress the extended model using other variables that influence total expenditure 

PvfT_model <- lm(
  lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
  data = Pvf_data,
  weights = weight
)
summary(PvfT_model)

#by keeping all other variables constant, female partipcation becomes postive (6.7%) and significant (*)

#12.5 
#Measuring spillover effects of microcredit Program placement
