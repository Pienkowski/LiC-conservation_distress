######################################################################
######### Life in Conservation: Mental health and well-being #########
######################################################################
# The following describes the primary analysis steps to prepare the results presented in the article 'Protecting those who protect nature by supporting conservationists’ mental wellbeing


###### Steps ###### 
# 1) Set up the environment 
# 2) Conduct the main analysis analysis
# 3) Supplementary analysis 1: Remove dispositional optimism 
# 4) Supplementary analysis 2: Replace situational optimism with goal progress
# 5) Supplementary analysis 3: Exploring the role of age 
# 6) Supplementary analysis 4: Gender-disaggregated analysis 
# 7) Supplementary analysis 5: More conservative definition of conservationists


###### 1) Set up the environment ######
### Load packages ### 
library(mice)
library(fastDummies)
library(plyr)
library(dplyr)
library(psych)
library(kableExtra)
library(ltm)
library(lavaan)
library(semTools)
library(semPlot)
library(ggplot2)
library(ggpubr)
library(lavaan)
library(readr)
library(visdat)
library(naniar)


### Set seed ###
set.seed(123)

### Load data ###
# mice.imp.DIS.Rdata should be in the same folder as the code  
load("DF.DIS.1.Rdata") 
load("mice.imp.DIS.Rdata") 

# File containing variable information 
variables_df <- read.csv("Variables.csv")

### Load functions - save this ### 
# DF.DIS.1.Rdata should be in the same folder as the code  
source("LiC-distress_functions.R")


###### 2) Conduct the main analysis ######
### The post hoc model adjustments (based on the exploratory analysis are): 
# Drop variables with absolute rho > 0.6: ERI2, ADD2,  ERI4, ERI5, PS1, age_year_scaled 

# The model structure - excluding individual ERI variables
model_1_ERI_ratio <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'
### Run the SEM ###
# Using the imputed dataset
# WLSMVS
# And with ordered endogenous ordinal variables
endogenous_ord_vars <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6", 
                         "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10",
                         "SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")

# Run 
model_DIS_1_ERI_ratio <- lavaan::sem(model = model_1_ERI_ratio, data = mice.imp.DIS[[1]], estimator = "WLSMVS", ordered = endogenous_ord_vars)

# RMSEA - <.08
fitmeasures(model_DIS_1_ERI_ratio)["rmsea"]

# Comparative Fit Index - ≥.90
fitmeasures(model_DIS_1_ERI_ratio)["cfi"]

# SRMR - <0.08
fitmeasures(model_DIS_1_ERI_ratio)["srmr"]

# Summary 
summary(model_DIS_1_ERI_ratio)


# The model structure - excluding ERI_ratio
model_1_ERI_ind <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

### Run the SEM ###
# Run 
model_DIS_1_ERI_ind <- lavaan::sem(model = model_1_ERI_ind, data = mice.imp.DIS[[1]], estimator = "WLSMVS", ordered = endogenous_ord_vars)

# RMSEA - <.08
fitmeasures(model_DIS_1_ERI_ind)["rmsea"]

# Comparative Fit Index - ≥.90
fitmeasures(model_DIS_1_ERI_ind)["cfi"]

# SRMR - <0.08
fitmeasures(model_DIS_1_ERI_ind)["srmr"]

# Summary 
summary(model_DIS_1_ERI_ind)



### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI <- runMI(model_1_ERI_ratio, data = mice.imp.DIS, fun="sem", 
                                        estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI, file = "model_DIS_1_ERI_ratio_MI.Rdata")



# Run 
model_DIS_1_ERI_ind_MI <- runMI(model_1_ERI_ind, data = mice.imp.DIS, fun="sem", 
                                      estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI, file = "model_DIS_1_ERI_ind_MI.Rdata")


###### 3) Supplementary analysis 1: Removing dispositional optimism ######
# The model structure - excluding individual ERI variables
model_1_ERI_ratio_sub_1 <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

# ### Dispositional optimism ###
# # Latent part 
# OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6
# 
# # The method effect 
# method =~  LOTR_1 + LOTR_3 + LOTR_6
# 
# # OP and the method effect are orthognal 
# OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# # Dispositional optimism
# OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'
### Run the SEM ###
# Using the imputed dataset
# WLSMVS
# The model structure - excluding ERI_ratio
model_1_ERI_ind_sub_1 <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

# ### Dispositional optimism ###
# # Latent part 
# OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6
# 
# # The method effect 
# method =~  LOTR_1 + LOTR_3 + LOTR_6
# 
# # OP and the method effect are orthognal 
# OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# # Dispositional optimism
# OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

# And with ordered endogenous ordinal variables
endogenous_ord_vars <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6", 
                         "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10",
                         "SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")

### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_1 <- runMI(model_1_ERI_ratio_sub_1, data = mice.imp.DIS, fun="sem", 
                                        estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_1, file = "model_DIS_1_ERI_ratio_MI_sub_1.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_1 <- runMI(model_1_ERI_ind_sub_1, data = mice.imp.DIS, fun="sem", 
                                      estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_1, file = "model_DIS_1_ERI_ind_MI_sub_1.Rdata")


###### 4) Supplementary analysis 2: Replace situational optimism with goal progress ######
# The model structure - excluding individual ERI variables
model_1_ERI_ratio_sub_2 <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method

###### Regression part ###### 
### Goal progress and dispositional optimism
Goal_satisfaction_scaled ~ OP

#### Psychological distress and predictors
K10 ~ 

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Collective goal progress satisfaction 
Goal_satisfaction_scaled + 

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

# The model structure - excluding ERI_ratio
model_1_ERI_ind_sub_2 <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
###### Regression part ###### 
### Goal progress and dispositional optimism
Goal_satisfaction_scaled ~ OP

#### Psychological distress and predictors 
K10 ~ 

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Collective goal progress satisfaction 
Goal_satisfaction_scaled + 

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

### Run the SEM ###
# Using the imputed dataset
# WLSMVS
# And with ordered endogenous ordinal variables
endogenous_ord_vars <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6", 
                         "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10")


### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_2 <- semTools::runMI(model_1_ERI_ratio_sub_2, data = mice.imp.DIS, fun="sem", 
                                                  estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_2, file = "model_DIS_1_ERI_ratio_MI_sub_2.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_2 <- semTools::runMI(model_1_ERI_ind_sub_2, data = mice.imp.DIS, fun="sem", 
                                                estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_2, file = "model_DIS_1_ERI_ind_MI_sub_2.Rdata")



###### 5) Supplementary analysis 3: Exploring the role of age ######
### Repeat the analysis with age instead of years in conservation ### 
# The model structure - excluding individual ERI variables
model_1_ERI_ratio_sub_3.a <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Age 
age_year_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

# The model structure - excluding ERI_ratio
model_1_ERI_ind_sub_3.a <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Age 
age_year_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

### Run the SEM ###
# Using the imputed dataset
# WLSMVS
# And with ordered endogenous ordinal variables
endogenous_ord_vars <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6", 
                         "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10",
                         "SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")


### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_3.a <- runMI(model_1_ERI_ratio_sub_3.a, data = mice.imp.DIS, fun="sem", 
                                  estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_3.a, file = "model_DIS_1_ERI_ratio_MI_sub_3.a.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_3.a <- runMI(model_1_ERI_ind_sub_3.a, data = mice.imp.DIS, fun="sem", 
                                estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_3.a, file = "model_DIS_1_ERI_ind_MI_sub_3.a.Rdata")


### Repeat the analysis with age and years in conservation ### 
# The model structure - excluding individual ERI variables
model_1_ERI_ratio_sub_3.b <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Age AND years in conservation 
age_year_scaled + years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'

# The model structure - excluding ERI_ratio
model_1_ERI_ind_sub_3.b <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Gender (RL = gender_Female)
gender_Male + gender_Unknown  + 

# Age AND years in conservation 
age_year_scaled + years_cons_scaled +  

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'


### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_3.b <- runMI(model_1_ERI_ratio_sub_3.b, data = mice.imp.DIS, fun="sem", 
                                        estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_3.b, file = "model_DIS_1_ERI_ratio_MI_sub_3.b.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_3.b <- runMI(model_1_ERI_ind_sub_3.b, data = mice.imp.DIS, fun="sem", 
                                      estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_3.b, file = "model_DIS_1_ERI_ind_MI_sub_3.b.Rdata")



###### 6) Supplementary analysis 4: Gender-disaggregated analysis ######
### Split the data between male and female 
mice.imp.DIS_male <- list()
mice.imp.DIS_female <- list()
for (i in seq_along(1:length(mice.imp.DIS))){
  mice.imp.DIS_male[[i]] <- mice.imp.DIS[[i]][which(mice.imp.DIS[[i]]$gender=='Male'), ]
  mice.imp.DIS_female[[i]] <- mice.imp.DIS[[i]][which(mice.imp.DIS[[i]]$gender=='Female'), ]
}

# The model structure - excluding individual ERI variables - male 
model_1_ERI_ratio_sub_4_male <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'
### Run the SEM ###
# The model structure - excluding ERI_ratio - male
model_1_ERI_ind_sub_4_male <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + education_simple_Unknown +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'


### Run both analysis with imputed datasets - male ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_4_male <- runMI(model_1_ERI_ratio_sub_4_male, data = mice.imp.DIS_male, fun="sem", 
                                             estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_4_male, file = "model_DIS_1_ERI_ratio_MI_sub_4_male.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_4_male <- runMI(model_1_ERI_ind_sub_4_male, data = mice.imp.DIS_male, fun="sem", 
                                           estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_4_male, file = "model_DIS_1_ERI_ind_MI_sub_4_male.Rdata")


# The model structure - excluding individual ERI variables - female 
model_1_ERI_ratio_sub_4_female <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# ERI ratio 
ERI_ratio_scaled + 

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university + 

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'
### Run the SEM ###
# The model structure - excluding ERI_ratio - female
model_1_ERI_ind_sub_4_female <- '
###### Estimating the latent variables ######
### Distress ###
# Latent part
K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 

# Correlated error terms 
K10_2 ~~ K10_3 
K10_5 ~~ K10_6 
K10_7 ~~ K10_8

### Dispositional optimism ###
# Latent part 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
  
### Situational optimism ###
# Situational optimism 
SO =~ SO_1 + SO_2 + SO_3 +SO_4 + SO_5+ SO_6  + SO_8

# Correlated error terms 
SO_1 ~~ SO_2
SO_3 ~~ SO_4 
SO_5 ~~ SO_6 
SO_5 ~~ SO_8 

# Regressing dispositional against situational optimism 
SO ~ OP

###### Regression part ######  
K10 ~ SO +

# Efforts (drop ERI2, ADD2)
ERI_1 + # 1.	I have constant time pressure due to a heavy workload 
ERI_3 + # 3.	Over the past few years, my job has become more and more demanding 
ADD1  + # 4.	I do not have the resources I need to archive my work goals
ADD3  + # 6.	The organisation I work for may not exist in five years time

# Rewards (drop ERI4, ERI5)
ERI_6  + # 3.	I have experienced or I expect to experience an undesirable change in my work situation (reverse coding)
ERI_7  + # 4.	My job security is poor (reverse coding)
ERI_8  + # 5.	Considering all my efforts and achievements, I receive the respect and prestige I deserve at work
ERI_9  + # 6.	Considering all my efforts and achievements, my job promotion or advancement prospects are adequate 
ERI_10 + # 7.	Considering all my efforts and achievements, my salary or income is alright
ADD4   + # 8. I am satisfied with the contribution I make to conservation   
ADD5   + # 9.	My friends and family are proud that I work in conservation  

# Position (RL = position_simple_Academic)
position_simple_Practice + position_simple_Unknown.other + 

# Personal insecurity
PS_2 + # 2.	My work puts me in dangerous situations
PS_3 + # 3.	I do not feel safe, even where I live

# Working hours
WH_scaled +

# Dispositional optimism
OP +

# Years in conservation 
years_cons_scaled + 

# National / non-national (RL = national.nnational_1 = a national of the country they are working in)
national.nnational_0 + # Not a national of the work country 

# Education (RL = education_simple_University)
education_simple_Non_university +

# Physical health
health +

# Social support 
SS1 + # 1. … your personal relationships?
SS2 + # 2. … the support you get from your friends and family? 
SS3   # 3. … the amount of time you are able to spend with friends and family?
'


### Run both analysis with imputed datasets - female ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_4_female <- runMI(model_1_ERI_ratio_sub_4_female, data = mice.imp.DIS_female, fun="sem", 
                                               estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_4_female, file = "model_DIS_1_ERI_ratio_MI_sub_4_female.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_4_female <- runMI(model_1_ERI_ind_sub_4_female, data = mice.imp.DIS_female, fun="sem", 
                                             estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_4_female, file = "model_DIS_1_ERI_ind_MI_sub_4_female.Rdata")



######  7) Supplementary analysis 5: Using a more conservative definition of conservationist ###### 
# Subset to only those currently working in conservation 
mice.imp.DIS_conservative <- list()
for (i in seq_along(1:length(mice.imp.DIS))){
  mice.imp.DIS_conservative[[i]] <- mice.imp.DIS[[i]][ which(mice.imp.DIS[[i]]$Conservation %in% c("Yes", "Sim", "Sí", "S<ed>", "Oui") ), ]
}
 
### Run both analysis with imputed datasets ###
# Run 
model_DIS_1_ERI_ratio_MI_sub_5 <- runMI(model_1_ERI_ratio, data = mice.imp.DIS_conservative, fun="sem", 
                                        estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ratio_MI_sub_5, file = "model_DIS_1_ERI_ratio_MI_sub_5.Rdata")



# Run 
model_DIS_1_ERI_ind_MI_sub_5 <- runMI(model_1_ERI_ind, data = mice.imp.DIS_conservative, fun="sem", 
                                      estimator = "WLSMVS", ordered = endogenous_ord_vars,  FUN = fitMeasures)

# Save the model 
save(model_DIS_1_ERI_ind_MI_sub_5, file = "model_DIS_1_ERI_ind_MI_sub_5.Rdata")

