#PSM Technique 

#########USING SURVEY################

library(dplyr)
library(Matching)
library(ggplot2)
library(survey)
library(nonrandom)

setwd("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data")

data.df <- read.csv("hh_98.csv")

data.df <- mutate(data.df, lexptot = log(1 + exptot)) %>%
  mutate(lnland = log((1 + hhland/100)))

#step 1. determine the propensity score and satisy the balancing property 


# Estimating propensity scores and performing matching [USING "Svyglm"]

# Create survey design object
des1 <- svydesign(id = ~1, weights = ~weight, data = data.df)

# Fit the model
#nearest neighbour
prog.lm <- svyglm(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                  design = des1, family = quasibinomial(link = "probit"))

# erorr message - Check unique values of dmmfd
unique(data.df$dmmfd)

# View the summary of the matching results
summary(prog.lm)

X <- prog.lm$fitted.values
Tr <- data.df$dmmfd
Y <- data.df$lexptot
m.out <- Match(Tr = Tr, X = X, Y = Y, M = 1, caliper = 0.001, replace = TRUE)

#view the summary
summary(m.out)

#assess the balance of covariates after matching
#nboots = number of bootsrap samples for standard errors 
#ks = whether to use komogorov-Smirnov tests for balance 
MatchBalance(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, data = data.df, nboots = 500, ks = TRUE)

#balancing property is not satisfied due to "egg"

fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values

fit.control <- filter(fit, dmmfd ==0)
fit.treated <- filter(fit, dmmfd ==1)

#plot 
ggplot() + 
  geom_density(aes(x=fit.control$fvalues, linetype = '2')) +
  geom_density(aes(x=fit.treated$fvalues, linetype = '3')) +
  xlim(-.1,.6) +
  xlab("") +
  scale_linetype_discrete(name = "", labels = c("Control", "Treated")) +
  ggtitle("Control and Treated Densities")


#re-run with removing "egg" 

