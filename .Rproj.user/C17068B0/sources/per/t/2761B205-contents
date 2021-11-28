######################################################################
######### Life in Conservation: Mental health and well-being #########
######################################################################
# The following describes the primary steps used to prepare the data used in the analysis presented in the article Protecting those who protect nature by supporting conservationists’ mental wellbeing

###### Steps ###### 
# 1) Set up the environment 
# 2) Examine each variable 
# 3) Simple bivariate associations 
# 4) Patterns of missing data and imputation 
# 5) Post imputation manipulation 

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
library(DataExplorer)

### Set seed ###
set.seed(123)

### Load data ###
# DF.DIS.1.Rdata should be in the same folder as the code  
load("DF.DIS.1.Rdata") 

# File containing variable information 
variables_df <- read.csv("Variables.csv")

### Load functions - save this ### 
# DF.DIS.1.Rdata should be in the same folder as the code  
source("LiC-distress_functions.R")

### Group key variables ### 
SO_names <- c("SO_1", "SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")
K10_col_name <- c("K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9","K10_10")
LOTR_col_name <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4","LOTR_5","LOTR_6")
LOTR_pos <- c("LOTR_1","LOTR_3" ,"LOTR_6")
LOTR_neg <- c("LOTR_2","LOTR_4", "LOTR_5")
PS_col_name <- c("PS_1", "PS_2","PS_3")
SS_col_name <- c("SS1", "SS2","SS3")
ERI_effort <- c("ERI_1","ERI_2","ERI_3", "ADD1","ADD2" , "ADD3")
ERI_reward <- c("ERI_4","ERI_5", "ERI_6","ERI_7","ERI_8","ERI_9", "ERI_10", "ADD4", "ADD5")
ERI_reverse <- c("ERI_5", "ERI_6","ERI_7")
ERI_effort_o <- c("ERI_1","ERI_2","ERI_3")
ERI_reward_o <- c("ERI_4","ERI_5", "ERI_6","ERI_7","ERI_8","ERI_9", "ERI_10")



### Create test and training observations ### 
train = sort(sample(nrow(DF.DIS.1), nrow(DF.DIS.1)*2/3)) # 2/3 observations into training 

# my pallete 
mypal1 <- c("#af8dc3", "#7fbf7b")
mypal2 <- c("#332288", "#44AA99", "#88CCEE", "#CC6677", "#882255")


###

###### 2) Examine each variable ######

###### 2.1) Psychological distress ######
### Plotting raw reponses ###
# Subset to K10 items 
K10_DF <- DF.DIS.1[K10_col_name]

# create a DF with the proportions for the first K10 variable 
K10_prop <- data.frame(Variable = K10_col_name[1], cbind(t(prop.table(table(K10_DF[1])))))

# For loop appending the proportions for the remaining variables 
for (i in seq_along(2:(length(K10_col_name)))){
  K10_prop <- rbind(K10_prop, data.frame(Variable = K10_col_name[i+1], cbind(t(prop.table(table(K10_DF[i+1]))))))
}

# Rename K10 variable code to question
K10_prop$Variable <- subset(variables_df, Code %in% K10_col_name, select=c("Question"))$Question

# rename response levels 
names(K10_prop) <- c("Variable", "None of the time", "A little of the time", "Some of the time","Most of the time", "All of the time")

# Plot the responses 
HH::likert(Variable~.,K10_prop, positive.order=TRUE,as.percent = TRUE,
           main="K10 responses",
           xlab="Percentage", ylab="Variable")

### Factor structure of the K10 ### 

# Parallel analysis (with training data) - suggests the extraction of three factors. This is probably because some of the items are paired. 
fa.parallel(DF.DIS.1[train, K10_col_name] , cor = "poly", fm="wls", fa="fa",   main = "Parallel analysis")

# Confirmatory analysis with training data with correlated error terms for paired items.
CFA_1 <- 'K10 =~ K10_1 + K10_2 + K10_3 + K10_4 + K10_5 + K10_6 + K10_7 + K10_8 + K10_9 + K10_10 
K10_2 ~~ K10_3
K10_5 ~~ K10_6 
K10_7 ~~ K10_8
'

# With training data 
CFA_mod1 <- cfa(CFA_1, data = DF.DIS.1[train, K10_col_name])

# RMSEA 
fitmeasures(CFA_mod1)["rmsea"]

# Factor loadings 
summary(CFA_mod1)

# With test data
CFA_mod2 <- cfa(CFA_1, data = DF.DIS.1[-train, K10_col_name])

# RMSEA 
fitmeasures(CFA_mod2)["rmsea"]

# Factor loadings 
summary(CFA_mod2)

# With complete dataset
CFA_mod3 <- cfa(CFA_1, data = DF.DIS.1[K10_col_name])

# RMSEA  
fitmeasures(CFA_mod3)["rmsea"]

###### 2.2) Age ######
hist(DF.DIS.1$age_year)

###### 2.3) Situational optimism ######

# Subset to situational optimism items 
SO_DF <- DF.DIS.1[SO_names]

# create a DF with the frequency for the first GP variable 
SO_freq <- data.frame(Variable = SO_names[1], cbind(t(table(SO_DF[1]))))

# For loop appending the frequency for the remaining variables 
for (i in seq_along(2:(length(SO_names)))){
  SO_freq <- rbind(SO_freq, data.frame(Variable = SO_names[i+1], cbind(t(table(SO_DF[i+1])))))
}

# Rename SO variable code to question
SO_freq$Variable <- subset(variables_df, Code %in% SO_names, select=c("Question"))$Question

# rename response levels 
names(SO_freq) <- c("Variable", "Definitely won't", "Probably won't", "Probably will", "Definitely will")

# Plot the responses (this is levels of satisfaction for those goals respondents considered important)
HH::likert(Variable~.,SO_freq, positive.order=TRUE,as.percent = F,
           main="Situational optimism",
           xlab="Frequency", ylab="Variable")

###### Create a simple composite variable for goal progress satisfaction ######

# Simply take the mean collective goal progress satisfaction score (with the ordinal data being treated as equal-interval) across those items endorsed by each respondent (i.e., excluding NAs). 
DF.DIS.1$Goal_satisfaction <- rowMeans(DF.DIS.1[GP_b_sub], na.rm = T)

# Scale and centre 
DF.DIS.1$Goal_satisfaction_scaled <- as.numeric(scale(DF.DIS.1$Goal_satisfaction, scale = T, center = T))

# Plot a histogram of the mean collective goal progress satisfaction score
hist(DF.DIS.1$Goal_satisfaction_scaled)


###### 2.4) Effort-reward imbalance ######
### Effort ####
# Subset to effort items 
ERI_eff_DF <- DF.DIS.1[ERI_effort]

# create a DF with the frequency for the first GP variable 
eff_prop <- data.frame(Variable = ERI_effort[1], cbind(t(prop.table(table(ERI_eff_DF[1])))))

# For loop appending the frequency for the remaining variables 
for (i in seq_along(2:(length(ERI_effort)))){
  eff_prop <- rbind(eff_prop, data.frame(Variable = ERI_effort[i+1], cbind(t(prop.table(table(ERI_eff_DF[i+1]))))))
}

# Rename GP variable code to question
eff_prop$Variable <- subset(variables_df, Code %in% ERI_effort, select=c("Question"))$Question

# rename response levels 
names(eff_prop) <- c("Variable", "Strongly disagree", "Disagree", "Agree","Strongly agree")

# Plot the responses (this is levels of satisfaction for those goals respondents considered important)
HH::likert(Variable~.,eff_prop, positive.order=TRUE,as.percent = F,
           main="Effort items",
           xlab="Frequency", ylab="Variable")

### Reward ####
# Subset to effort items 
ERI_rew_DF <- DF.DIS.1[ERI_reward]

# Reverse the reverse coded items ("ERI_5", "ERI_6", "ERI_7")
ERI_rew_DF[ERI_reverse] <- apply(ERI_rew_DF[ERI_reverse], 2, function(x) as.numeric(plyr::revalue(as.character(x), c("1" = "4", "2" = "3", "3" = "2", "4" = "1"))))

# create a DF with the frequency for the first GP variable 
rew_prop <- data.frame(Variable = ERI_reward[1], cbind(t(prop.table(table(ERI_rew_DF[1])))))

# For loop appending the frequency for the remaining variables 
for (i in seq_along(2:(length(ERI_reward)))){
  rew_prop <- rbind(rew_prop, data.frame(Variable = ERI_reward[i+1], cbind(t(prop.table(table(ERI_rew_DF[i+1]))))))
}

# Rename GP variable code to question
rew_prop$Variable <- subset(variables_df, Code %in% ERI_reward, select=c("Question"))$Question

# rename response levels 
names(rew_prop) <- c("Variable", "Strongly disagree", "Disagree", "Agree","Strongly agree")

# Plot the responses (this is levels of satisfaction for those goals respondents considered important)
HH::likert(Variable~.,rew_prop, positive.order=TRUE,as.percent = F,
           main="Reward items",
           xlab="Frequency", ylab="Variable")

### Calculate the effort-reward imbalance ratio ###
# ERI ratio function 
ERI_func <- function(x,y){
  Effort <- rowSums(x, na.rm = T) # Sum score of efforts
  Reward <- rowSums(y, na.rm = T) # Sum score of rewards (with reverse coded items correctly coded)
  correct <- rowSums(!is.na(x))/rowSums(!is.na(y)) # A correction factor, corresponding to the number of items in the numerator and denominator
  ERI <- Effort/(Reward*correct) # The effort-reward imbalance ratio, corrected for differences in the number of items respondents answer 
  return(ERI)
}

# ERI ratio - A value of 1 indicates a balance between efforts and rewards, values approaching 0 indicate high reward vs low effort, and values above 1 indicate high effort vs low reward
DF.DIS.1$ERI_ratio <- ERI_func(DF.DIS.1[ERI_effort], DF.DIS.1[ERI_reward])

# Scale and centre
DF.DIS.1$ERI_ratio_scaled <- as.numeric(scale(DF.DIS.1$ERI_ratio, center = T, scale = T))

# Histogram of ERI-ratios 
hist(DF.DIS.1$ERI_ratio_scaled)
abline(v = 1, col="blue") # 1 indicates a balance

###### 2.5) Dispositional optimism ######
# Dispositional optimism was estimated using the Life Orientation Test-Revised (Scheier et al., 1994), using the structure used in other literature (Vecchione et al., 2014).

# The structure of the DO model 
model_DO_method <- '
### Dispositional optimism 
# Dispositional optimism 
OP =~ LOTR_1 + LOTR_2 + LOTR_3 +  LOTR_4 + LOTR_5 + LOTR_6

# The method effect 
method =~  LOTR_1 + LOTR_3 + LOTR_6

# OP and the method effect are orthognal 
OP ~~0*method
'

# Run the SEM on all imputed DF 
model_GP_1 <- lavaan::sem(model = model_DO_method, data = DF.DIS.1, estimator = "WLSMVS")

# RMSEA - <.08
fitmeasures(model_GP_1)["rmsea"]

# Comparative Fit Index - ≥.90
fitmeasures(model_GP_1)["cfi"]

# SRMR - <0.08
fitmeasures(model_GP_1)["srmr"]

# Extract and inspect plausible values (just for interest)
pv_DO <- plausibleValues(model_GP_1, nDraws = 10, seed = 123)
for (i in seq_along(1:length(pv_DO))) {pv_DO[[i]]$Iteration <- as.character(i)}

# Create single DF
pv_DO_df <- do.call(rbind, pv_DO)

# Plot
ggplot(pv_DO_df, aes(x=OP, color=Iteration)) +
  geom_density()

###### 2.6) Health ######
# 1 = very bad, 5 = very good 
# Ordinal scale but treated as numeric exogenous variable 
table(DF.DIS.1$health)

###### 2.7) Social support ######
# 1 = very dissatisfied, 5 = 	Very satisfied
# Ordinal scale but treated as numeric exogenous variable 
table(DF.DIS.1$SS1) # … your personal relationships?
table(DF.DIS.1$SS2) # … the support you get from your friends and family? 
table(DF.DIS.1$SS3) # … the amount of time you are able to spend with friends and family?

###### 2.8) Personal security ######
# 1 = strongly disagree, 5 = strongly agree
table(DF.DIS.1$PS_1) # It is dangerous to go outside at night alone 
table(DF.DIS.1$PS_2) # My work puts me in dangerous situations
table(DF.DIS.1$PS_3) # I do not feel safe, even where I live

###### 2.9) Years in conservation ######
hist(DF.DIS.1$age_year)

###### 2.10) Position ######
table(DF.DIS.1$position_simple)

###### 2.11) Gender ######
table(DF.DIS.1$gender)

###### 2.12) Education ######
table(DF.DIS.1$education_simple)

###### 2.13) National/non-national ######
# 1 = national, 0 = non-nation of the country where the respondent works 
table(DF.DIS.1$national.nnational)

###### 2.14) Work hours ######
hist(DF.DIS.1$WH)
DF.DIS.1$WH_scaled <- as.numeric(DF.DIS.1$WH_scaled)

###


###### 3) Selected bivariate associations ######
# 'Continuous' variables
key_vars_num <- c("age_year_scaled", "years_cons_scaled", "WH_scaled", "Goal_satisfaction_scaled" ,"ERI_ratio_scaled")

# Exploration DF (tedious step because of apparent issue with ggplot)
explor_df <- DF.DIS.1
colnames(explor_df) <- make.unique(names(explor_df))

# Correlations between all continuous data (or ordinal variables treated as continuous).
plot_correlation(na.omit(explor_df[key_vars_num]), maxcat = 5L)
# Unsurprisingly, a strong association between age and years in conservation - drop age_year_scaled

# Ordinal variables (apart from the K10 variables) 
key_vars_ord <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6","ERI_1", "ERI_2", "ERI_3", "ADD1", "ADD2", "ADD3", "ERI_4", "ERI_5", "ERI_6", "ERI_7",
              "ERI_8", "ERI_9", "ERI_10",  "ADD5",  "ADD4","PS_1", "PS_2", "PS_3", "health", "SS1", "SS2", 
              "SS3")

# Calculate polychoric correlation 
poly_cor = polychoric(DF.DIS.1[, key_vars_ord])

# Plot the correlation 
cor.plot(poly_cor$rho, numbers=F, upper=FALSE, main = "Polychoric correlation", show.legend = T)

# Exclude variable that have a moderate or strong covariance 
abs(poly_cor$rho) > 0.6
# ERI_1 (I have constant time pressure due to a heavy workload) + ERI2 (I have many interruptions and disturbances while performing my job) = 0.601363546
# ADD1 (I do not have the resources I need to archive my work goals) + ADD2 (The organisation I work for does not have enough funding to achieve its main aims) = 0.601363546
# ERI4 (I receive the respect I deserve from my boss and work colleagues) + ERI8 (Considering all my efforts and achievements, I receive the respect and prestige I deserve at work) = 0.74831155 
# ERI5 (My job promotion or advancement prospects are poor (reverse coding)) + ERI9 (Considering all my efforts and achievements, my job promotion or advancement prospects are adequate) = 0.70745796  
# PS1 (It is dangerous to go outside at night alone) + PS3 (I do not feel safe, even where I live) = 0.68426446 
# (Drop ERI2, ADD2,  ERI4, ERI5, and PS1 from the analysis)

###### 4) Multiple imputation ######
### Subset to the imputed variables ###
# Imputed variables 
imptuted_variables <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6","age_year_scaled", "years_cons_scaled", 
"gender" , "CV", "position_simple", "education_simple", "SO_Region" , "WH_scaled",  "national.nnational", 
"ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10","ADD1","ADD2", "ADD3","ADD4","ADD5",
"K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10", 
"PS_1","PS_2","PS_3","SS1","SS2", "SS3","health", "ERI_ratio_scaled",
"SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10", "Goal_satisfaction_scaled" )

# Ordinal variables 
ordinal_vars <- c("LOTR_1","LOTR_2","LOTR_3","LOTR_4", "LOTR_5","LOTR_6", "ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10","ADD1","ADD2", "ADD3","ADD4","ADD5",
                  "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9", "K10_10", 
                  "PS_1","PS_2","PS_3","SS1","SS2", "SS3","health",
                  "SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")

# Numeric variables 
numeric_vars <- c("age_year_scaled", "years_cons_scaled", "WH_scaled", "ERI_ratio_scaled", "Goal_satisfaction_scaled" )
  
# Factors 
factor_vars <- c("gender" , "CV", "position_simple", "education_simple", "SO_Region", "national.nnational")

# Exogenous variables 
exo_vars <- c("ERI_1", "ERI_2", "ERI_3", "ADD1", "ADD2", "ADD3", "ERI_4", "ERI_5", "ERI_6", "ERI_7",
              "ERI_8", "ERI_9", "ERI_10", "ADD4", "ADD5", "PS_1", "PS_2", "PS_3", "health", "SS1", "SS2", 
              "SS3", "SO_1","SO_2", "SO_3", "SO_4", "SO_5", "SO_6", "SO_7", "SO_8", "SO_9", "SO_10")

# Did I miss any variable? 
setdiff(imptuted_variables, c(ordinal_vars,numeric_vars,factor_vars))

# Subset the DF to include only the variables to be imputed
DF.DIS.1_imp <- DF.DIS.1[imptuted_variables]

### Patterns of missing data ### 
naniar::vis_miss(DF.DIS.1_imp)+ 
  theme(axis.text.x = element_text(angle = 90)) + theme(text = element_text(size=8)) + coord_flip() 

# Save plot - jpeg
ggsave(
  "Figure_missing_1.jpeg",
  width = 180,
  height = 250,
  units = c("mm"),
  dpi = 800
)

# Percentage of missing values
(table(is.na(DF.DIS.1_imp))[2]/(table(is.na(DF.DIS.1_imp))[1]+table(is.na(DF.DIS.1_imp))[2]))*100

# Overview pattern of missingness 
naniar::gg_miss_var(DF.DIS.1_imp, show_pct = TRUE) + theme(text = element_text(size=8))

# Save plot - jpeg
ggsave(
  "Figure_missing_2.jpeg",
  width = 100,
  height = 120,
  units = c("mm"),
  dpi = 800
)


### Removing observations with incomplete K10 scores ### 
DF.DIS.2_imp<- DF.DIS.1_imp[complete.cases(DF.DIS.1_imp[K10_col_name]), ] 

# How many observation were dropped?
nrow(DF.DIS.1_imp) - nrow(DF.DIS.2_imp)

### Function that ensures the data is in the correct format, drops "other" as a category from gender (because there are too few observations for stable estimation), determines the imputation method from the data type, and performs the imputation using MICE.
MI_function_1 <-function(DF1) {
  
  ### Correct data type ###
  # As ordinal
  DF1[, ordinal_vars] <- lapply(DF1[, ordinal_vars], as.ordered)
  
  # As numeric
  DF1[, numeric_vars] <- lapply(DF1[, numeric_vars], as.numeric)
  
  # As factor 
  DF1[, factor_vars] <- lapply(DF1[, factor_vars], as.factor)
  
  ### Drop "other" as a category in gender (there were too few observations to model)
  DF1<-DF1[!(DF1$gender=="Other"),]
  DF1<- droplevels(DF1)
  
  ### Number of imputed DF ###
  # 10 imputed DF
  N.Imp = 10
  
  ### determine the imputation method from the data type ##
  # The data type for each variable 
  str_out <- data.frame(capture.output(str(DF1)))
  
  # Delete the first and last row
  str_out <- data.frame(str_output = matrix(str_out[2:(nrow(str_out)-1),]))
  
  # Create a column that contain the appropriate model for each variable - this only works if the variable is of the correct data type in the first place 
  str_out$type <- ifelse(grepl("Ord.factor", str_out$str_output, fixed = TRUE)==T, "polr", 
                         ifelse(grepl("num", str_out$str_output, fixed = TRUE)==T, "pmm", 
                                ifelse(grepl("Factor", str_out$str_output, fixed = TRUE)==T, "polyreg",
                                       ifelse(grepl("int", str_out$str_output, fixed = TRUE)==T, "logreg", "ERROR"))))
  
  # Conduct the MI - with the number of datasets specified by N.Imp, and the estimation type specified by str_out$type (derived from the above)
  DF1_imp <- mice(DF1, m = N.Imp, method = str_out$type )
  
  # Print the first 50 logged events, if they occur 
  print(head(DF1_imp$loggedEvents, 50))
  
  # Return the imputed data
  return(DF1_imp)
}

### Conduct the imputation ### 
# Conduct the imputation using DF.DIS.1 (excluding country code)
mice.imp.DIS_int <- MI_function_1(DF = DF.DIS.1_imp)

# Save the imputed data
save(mice.imp.DIS_int, file = "mice.imp.DIS_int.Rdata")
# load("mice.imp.DIS_int.Rdata") 


###


###### 5) Post imputation manipulation ######
### Function to add goal endorsement and satisfaction variable, SO_CountryCode & respondent ID, change the reference level for region
MI_function_2 <-function(DF1, DF_imp) {
  
  ### Number of imputed DF ###
  # 10 imputed DF
  N.Imp = 10
  
  ### Drop "other" as a category in gender (there were too few observations to model)
  DF1<-DF1[!(DF1$gender=="Other"),]
  DF1<- droplevels(DF1)
  
  ### Extract each imputed dataset and perform additional manipulation ###
  # Create a list to store the imputed datasets 
  mice.imp <- list()
  
  # For i in each dataset
  for(i in 1:N.Imp) {
    
    ### Extract the imputed data
    mice.imp[[i]] <- mice::complete(DF_imp, action= i, inc=FALSE)
    
    # Add country code 
    mice.imp[[i]]$SO_CountryCode <- DF1$SO_CountryCode
    
    # Add respondent ID
    mice.imp[[i]]$ID <- DF1$ID
    
    # Change the reference level for region
    mice.imp[[i]]$SO_Region <- as.factor(mice.imp[[i]]$SO_Region)
    mice.imp[[i]]$SO_Region <- relevel(mice.imp[[i]]$SO_Region, ref="Europe and Northern America")
    
    ### Turn factor into series of binary variables and remove special characters 
    # Create the dummy columns 
    mice.imp[[i]] <- dummy_cols(mice.imp[[i]], select_columns = c("gender", "education_simple", "position_simple", "SO_Region", "national.nnational"))
    
    # Remove spaces and other characters on column names
    colnames( mice.imp[[i]]) <- gsub(" ", "", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("/", "", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("-", "_", colnames( mice.imp[[i]]), fixed = TRUE)
    colnames( mice.imp[[i]]) <- gsub("&", "_", colnames( mice.imp[[i]]), fixed = TRUE)
    
    # Add the 'Conservation' variable - which indicates if individuals currently consider themselves to be working or studying in conservation 
    mice.imp[[i]]$Conservation <- DF1$Conservation
    
    # ERI ratio - A value of 1 indicates a balance between efforts and rewards, values approaching 0 indicate high reward vs low effort, and values above 1 indicate high effort vs low reward
    mice.imp[[i]][ERI_effort_o] <- apply(mice.imp[[i]][ERI_effort_o], 2, as.numeric)
    mice.imp[[i]][ERI_reward_o] <- apply(mice.imp[[i]][ERI_reward_o], 2, as.numeric)
    mice.imp[[i]]$ERI_ratio_o <- ERI_func(mice.imp[[i]][ERI_effort_o], mice.imp[[i]][ERI_reward_o])
    
    # Scale and centre
    mice.imp[[i]]$ERI_ratio_o_scaled <- as.numeric(scale(mice.imp[[i]]$ERI_ratio_o, center = T, scale = T))
    

    # Add back the unscaled items 
    mice.imp[[i]]$years_cons <- DF1$years_cons
    mice.imp[[i]]$age_year <- DF1$age_year
    mice.imp[[i]]$WH    <- DF1$WH
    
    # # Convert exogenous ordinal variables to numeric 
    if(i == 1) { print(str(mice.imp[[i]][exo_vars][1]))} # Check the 
    if(i == 1) { print(table(mice.imp[[i]][exo_vars][1]))} # Check the 
    mice.imp[[i]][exo_vars] <- apply(mice.imp[[i]][exo_vars], 2, as.character)
    mice.imp[[i]][exo_vars] <- apply(mice.imp[[i]][exo_vars], 2, as.numeric)
    if(i == 1) { print(str(mice.imp[[i]][exo_vars][1]))}
    if(i == 1) { print(table(mice.imp[[i]][exo_vars][1]))}
  }
  
  # Return the manipulated DF 
  return(mice.imp)
}

# Implement the function 
mice.imp.DIS <- MI_function_2(DF1 = DF.DIS.1, DF_imp = mice.imp.DIS_int)

# Save the data 
save(mice.imp.DIS, file = "mice.imp.DIS.Rdata")

###

