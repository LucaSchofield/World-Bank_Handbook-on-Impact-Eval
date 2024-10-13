#######################

#Worl Bank Impact Eval Handbook 
#Instrumental Variables 

#########################


library(dplyr)
library(ggplot2)
library(ggthemes)
library(forecast)
library(psych)
library(tidyr)


#instrument is the households choice to participate 
# hosuehold choice is dependant on 2 factors 
  #1. availability of microcredit program in the village; and 
  #2 household eligibility to participate 
#program placeplacement in the village is endogenous but household eligibility is not

hh_98.df <- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_98.csv")
head(hh_98.df)

hh_98.df <- mutate(hh_98.df, lnland = log(1 + hhland / 100))
hh_98.df <- mutate(hh_98.df, lexptot = log(1 + exptot))

#create a village program varibale for females and then a female program choice varibale at the household level
#households are eligible it it has fewer than 50 db of land 

hh_98.df <- group_by(hh_98.df,villid) %>%
  mutate(villfmf =  max(dmmfd))

hh_98.df <- mutate(hh_98.df, fchoice = ifelse(villfmf == 1 & hhland < 50,1,0))

#add instruments by intercating the choice variable with all covariates 
# condition covaraiates on fchoice 

iv_variables <- c("agehead", "sexhead", "educhead", "lnland", "vaccess", "pcirr", "rice", "wheat", "milk", "potato", "egg", "oil")

hh_98.df <- hh_98.df %>%
  mutate(across(all_of(iv_variables), ~ fchoice * ., .names = "fch{col}"))

#########################################################################################
#----------------------------------------------------------------------------------------
# Estimation without interaction terms while just using fchoice as an instrument
#----------------------------------------------------------------------------------------
#########################################################################################


##################
#first stage 
#################

fsls <- lm(dfmfd ~ agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + potato + egg + oil + fchoice,
           data = hh_98.df)

summary(fsls)

#error in potato 
#omit due to colinearality 

install.packages("AER")
library(AER)


##################
#second stage 
#################


# Create the IV formula for 2SLS
# First Stage: endogenous variable dfmfd is instrumented by other variables + interaction terms
# Second Stage: dependent variable lexptot regressed on main variables and interactions
formula <- lexptot ~ dfmfd + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil |
  dfmfd + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil +
  fchagehead + fcheduchead + fchlnland + fchvaccess + fchpcirr + fchrice + fchwheat + fchmilk + fchoil

# Run the Instrumental Variables Regression (2SLS)
ssls <- ivreg(formula, data = hh_98.df)

# View the summary of the 2SLS regression model
summary(ssls)

##########################
# testing for endogeneity 
# OLS versus IV
##########################


#manual approach 

ols_residuals <- residuals(fsls)
iv_residuals <- residuals(ssls)

# Calculate Wu-Hausman statistic
wu_hausman_stat <- sum(ols_residuals^2) - sum(iv_residuals^2)

# Set the correct degrees of freedom
df1 <- 1  # This represents the number of endogenous regressors (dfmfd in this case)
df2 <- length(ols_residuals) - length(coef(fsls))  # Residual degrees of freedom from OLS model

# Calculate the p-value correctly
p_value <- 1 - pf(wu_hausman_stat, df1 = df1, df2 = df2)

# Output the results
print(wu_hausman_stat)

#relatively large value suggests IV model explains the variation in the data better 

print(p_value)

#p-value suggests reject the null
# OLS model is inconsistent and endogenous variable us liekly causing the bias in the OLS estimates 


###################################

# IV Method for Binary Treatment

###################################

install.packages("sampleSelection")
library(sampleSelection)

# Run the endogenous treatment model using selection() function
treatment_model <- selection(
  selection = lexptot ~ agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + fchagehead + fcheduchead + fchlnland + fchvaccess + fchpcirr + fchrice + fchwheat + fchmilk + fchoil,  # Treatment equation
  outcome = dfmfd ~ agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,  # Outcome equation
  data = hh_98.df
)

# Summarize the results
summary(treatment_model)


#The Tobit 2 model indicates that several variables significantly impact both the selection into the sample (censored vs. uncensored) and the outcome variable
#factors such as land size, education, milk, and oil consumption are crucial for understanding the observed behavior in the data
#The strong correlation (rho) suggests that the selection process is closely linked to the outcome.
