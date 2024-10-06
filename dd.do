###########################
# World Eval Handbook 
# Double-Difference / DiD Method 
###########################


library(dplyr)
library(foreign)    
library(ggplot2)
library(survey)
library(lmtest)
library(plm)
library(car)
library(Matching)


hh_9198.df <- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_9198.csv")
head(hh_9198.df)

#use panel data to determine comparison of observable characteristics 

#########################
# Simplest implementation 
#########################

hh_9198.df <- mutate(hh_9198.df, exptot0=ifelse(year == 0, exptot, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(exptot91 = max(exptot0))
hh_9198.df <- subset(hh_9198.df, year == 1)
hh_9198.df <- mutate(hh_9198.df, lexptot91=ifelse(year == 1, log(1+exptot91), 0))
hh_9198.df <- mutate(hh_9198.df, lexptot98=ifelse(year == 1, log(1+exptot), 0))
hh_9198.df <- mutate(hh_9198.df, lexptot9891 = lexptot98-lexptot91)
hh_9198.df <- ungroup(hh_9198.df)

head(hh_9198.df)

#Regression implementation 

hh_9198.df <- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_9198.csv")

hh_9198.df <- mutate(hh_9198.df, lexptot = log(1 + exptot))
hh_9198.df <- mutate(hh_9198.df, lnland = log(1 + hhland / 100))
hh_9198.df <- mutate(hh_9198.df, dmmfd1=ifelse(dmmfd == 1 & year == 1, 1, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dmmfd98 = max(dmmfd1))
hh_9198.df <- mutate(hh_9198.df, dfmfd1=ifelse(dfmfd == 1 & year == 1, 1, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))
hh_9198.df <- mutate(hh_9198.df, dmmfdyr = dmmfd98*year)
hh_9198.df <- mutate(hh_9198.df, dfmfdyr = dfmfd98*year)
hh_9198.df <- ungroup(hh_9198.df)

###############################

# DD Regression 

################################


# Basic Model 

lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr,
         data = hh_9198.df)
summary(lm)


#adding in other variables 
lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
         data = hh_9198.df)

summary(lm)

#holding other factors constant dfmd98 changes from being significant to non significant 

#check for colinearality 

cor(hh_9198.df[, c("year", "dfmfd98", "dfmfdyr")], use = "complete.obs")

#             year   dfmfd98   dfmfdyr
# year    1.0000000 0.0000000 0.5978598
# dfmfd98 0.0000000 1.0000000 0.5668173
# dfmfdyr 0.5978598 0.5668173 1.0000000

#use eignevalues 
#eigenvalues measure the variance in the direction, 
# each eigenvalue tells you how much of the total variance is explained by that corresponding eigenvector

eigen(cor(hh_9198.df[, c("year", "dfmfd98", "dfmfdyr")], use = "complete.obs"))$values

# year = 1.8238435
# dfmfd98 = 1.0000000
# dfmfdyr = 0.1761565
# suggest some multicolinearality in dfmfdyr 


#use determinenants 
# measures how spread out the variables arr inn mutliple dimensios
det(cor(hh_9198.df[, c("year", "dfmfd98", "dfmfdyr")], use = "complete.obs"))
# 0.3212818


#remove dfmfdyr

lm1.1 <- lm(lexptot ~ year + dfmfd98 + factor(nh), data = hh_9198.df)
summary(lm1.1)


#Basic model with Fixed effects (FE) on nh 

#nh is the individual identifier

pdata <-pdata.frame(hh_9198.df, index = "nh")

plm1 <- plm(lexptot ~ year + dfmfd98 + dfmfdyr,
           data = pdata,
           model = "within")

summary(plm1)

#results show a positive impact on female participation

#extend model with other variables 

plm2 <- plm(lexptot ~ year + dfmfd98 + dfmfdyr + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
            data = pdata,
            model = "within")

summary(plm2)

#including other variables similarly reduces the significance of female participation 


#############################

# DD in cross-sectional data

#############################


#compare program and non program villages 

hh_91.df <- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_91.csv")
head(hh_91.df)

#create a dummy variable called target for those who are eligible to participate in the program inc. those with less than 50 db of land 
#create village program dummy progvill for those villages less than 25
# 

hh_91.df <- hh_91.df %>%
  mutate(
    lexptot = log(1 + exptot),
    lnland = log(1 + hhland / 100),
    target = ifelse(hhland < 50,1,0),
    progvill = ifelse(thanaid < 25,1,0)
  )

print(hh_91.df$target)

#geenerate a variable interacting with the program village and target 

hh_91.df <- hh_91.df %>%
  mutate(progtarget = progvill*target)

#calculate DD estimate by regressing log per cpaita expenditure against program village, target and their interaction 

lm_target <- lm(lexptot ~ progvill + target + progtarget, data = hh_91.df)
summary(lm_target)

#progtarget is 0.053 does not give the actual impact of the microcredit programs
# this needs to be adjusted by dividing by the proportion of target households in program villages 
#find the proprotion 


hh_91.df %>%
  filter(progvill == 1) %>%
  summarise(
    count = n(),
mean_target = mean(target, na.rm = TRUE),
    min_target = min(target, na.rm = TRUE),
    max_target = max(target, na.rm = TRUE)
)

#68% belong to the target group 
#therefore, divide progtarget regression coefficient by this value
#  count mean_target min_target max_target
#1   700   0.6885714          0          1

0.05294 / 0.6885714 
#0.07688382

#this is the true impact of microcredit programs on the target population 
# even though it is not significant 

#exapnd the reg. model adjusting for covariates that affect outcomes of interest 

lm_target2 <- lm(lexptot ~ progvill + target + progtarget + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
                weights = weight,
                data = hh_91.df)
summary(lm_target2)

#holding other factors constant, there is no change inb the significance level of microcredit impacts on households' annual total per capita expenditures

#fixed effects regression at the village level 
# cannot be run at the household level due to each hh appearing only once in the data 


##############

pdatavill <-pdata.frame(hh_91.df, index = "villid")

#drop progvill

plm3 <- plm(lexptot ~ target + progtarget,
            data = pdatavill,
            model = "within")
summary(plm3)


#there is a negative (insignificant) impact of microcredit programs on household per capita expenditure
# -1.2%


#including other covariates 

plm4 <- plm(lexptot ~ progvill + target + progtarget + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
            weights = weight,
            data = pdatavill)
summary(plm4)

# again no change seen in the significance level 


#including initial conditions 
#remove baseline levels without having multicolinearality issues 
# accounts for the seperate effect of initial conditions


# caluclate the difference for each household and variable by grouping 'nh'

head(hh_9198.df)

hh_diff <- hh_9198.df %>%
  group_by(nh) %>%
  mutate(
    dlexptot = lexptot[year == 1] - lexptot[year == 0],
    dlnland = lnland[year == 1] - lnland[year == 0],
    ddmmfd98 = dmmfd98[year == 1] - dmmfd98[year == 0],
    ddfmfd98 = dfmfd98[year == 1] - dfmfd98[year == 0],
    ddmmfdyr = dmmfdyr[year == 1] - dmmfdyr[year == 0],
    ddfmfdyr = dfmfdyr[year == 1] - dfmfdyr[year == 0],
    dagehead = agehead[year == 1] - agehead[year == 0],
    dsexhead = sexhead[year == 1] - sexhead[year == 0],
    deduchead = educhead[year == 1] - educhead[year == 0],
    dvaccess = vaccess[year == 1] - vaccess[year == 0],
    dpcirr = pcirr[year == 1] - pcirr[year == 0],
    drice = rice[year == 1] - rice[year == 0],
    dwheat = wheat[year == 1] - wheat[year == 0],
    dmilk = milk[year == 1] - milk[year == 0],
    dpotato = potato[year == 1] - potato[year == 0],
    degg = egg[year == 1] - egg[year == 0],
    doil = oil[year == 1] - oil[year == 0]
  ) %>%
  ungroup()

# View the result
(hh_diff$ddfmfd98)


# Run the regrdlexptot# Run the regression with initial condition variables and original covariates

lm_ic <- lm(dlexptot ~ ddfmfdyr + dsexhead + dagehead + deduchead +
              dlnland + dvaccess + dpcirr + drice + dmilk + 
              doil + degg + sexhead + agehead + educhead + 
              lnland + vaccess + pcirr + rice + wheat + milk + potato + egg + oil, 
            weights = weight,
            data = hh_diff)

# Summary of the model
summary(lm_ic)


#after controlling for the intial condition the impact of the microcredit participation disappears


#####################

# DD with PSM

#####################

hh_9198.df <- read.csv("C:\\Users\\lschofield\\OneDrive - KPMG\\Documents\\Business Development\\3.0 World-Bank_Impact-Eval-Handbook\\data\\hh_9198.csv")

hh_9198.df <- mutate(hh_9198.df, lnland = log(1 + hhland / 100))

hh_9198.df <- mutate(hh_9198.df, dfmfd1 = ifelse(dfmfd == 1 & year == 1, 1,0))
                     
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))

hh_9198 <- filter(hh_9198, year == 0)

head(hh_9198.df)


#first regression unbalanced 

library(Matching)
library(MatchIt)
library(cobalt)


prog.lm <- glm(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
               data = hh_9198.df,
               weights = weight,
               family = quasibinomial("probit"))
summary(prog.lm)


bal.tab.dd <- bal.tab(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
                      data = hh_9198.df,
                      un = TRUE)

bal.tab.dd

love.plot(bal.tab.dd, var.order = "unadj")


matchit_dd <- matchit(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
                      data = hh_9198.df,
                      method = "nearest",
                      distance = "glm",
                      caliper = 0.001,
                      replace = FALSE,
                      verbose = TRUE,
                      weights = hh_9198.df$weight)
summary(matchit_dd)

#assess balance 

bal.tab.dd_after <- bal.tab(matchit_dd)
bal.tab.dd_after

love.plot(bal.tab.dd_after, thresholds = 0.1)


ps <- predict(prog.lm, type = "response")
ps

#add propensity scores to data 

matched_psm <- match.data(matchit_dd)
head(matched_psm)

#add psm matched to nh in the new dataset 
matched_psm$pscore <- ps[match(matched_psm$nh, hh_9198.df$nh)]  # Match by nh (unique household ID)
head(matched_psm)


#re-esimtimate the baseline model with the matched dataset 

matched_psm <- mutate(matched_psm, lexptot = log(1 + exptot))
matched_psm <- mutate(matched_psm, lnland = log(1 + hhland / 100))
matched_psm <- mutate(matched_psm, dfmfd1=ifelse(dfmfd == 1 & year == 1, 1, 0))
matched_psm <- group_by(matched_psm,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))
matched_psm <- mutate(matched_psm, dfmfdyr = dfmfd98*year)
matched_psm <- ungroup(matched_psm)

matched_psm$pscore


# re estimate the basic model 


ps.lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = matched_psm)
summary(ps.lm)


#create analytical weights 
# weightsto adjust for any differences between treated and untreated groups after matching
# ensure any differences in outcomes are more likely due to the treatment effect rather than pre-existing differences between groups

matched_psm$a_weight <- 1
matched_psm$a_weight <- ifelse(matched_psm$dfmfd == 0, matched_psm$pscore/(1-matched_psm$pscore), 1)


#re estimate basis model with analytica weights 


psw.lm2 <- lm(lexptot ~ year + dfmfd98 + dfmfdyr + sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, data = matched_psm,weights = a_weight)
summary(psw.lm2)

#re estimate with the extended model


###################
# END
###################
