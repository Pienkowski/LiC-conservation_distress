######################################################################
######### Life in Conservation: Mental health and well-being #########
######################################################################

# Label re-coder 
label_recoder <- function(x){
  x <-  as.factor(ifelse(x == "years_cons_scaled", "Years in conservation",
                         ifelse(x == "OP", "Dispositional optimism", 
                                ifelse(x %in% c("gender_Male" , "genderMale"), "Men", 
                                       ifelse(x == "gender_Unknown", "Gender: unknown", 
                                              ifelse(x %in% c("position_simple_Practice" , "position_simplePractice"), "Practice/policy", 
                                                     ifelse(x == "position_simple_Unknown.other", "Position: unknown", 
                                                            ifelse(x == "years_cons_scaled", "Years in conservation",
                                                                   ifelse(x %in% c("Environment_CC","EnvironmentCC" ), "Cross-cutting",
                                                                          ifelse(x %in% c("Environment_Marine", "EnvironmentMarine") , "Marine",
                                                                                 ifelse(x == "Environment_Unknown", "Environment: unknown",
                                                                                        ifelse(x == "education_simple_Non_university", "Non-university",
                                                                                               ifelse(x %in%  c("SO_Region_CentralandSouthernAsia", "SO_RegionCentral and Southern Asia" ) ,"C. & S. Asia",
                                                                                                      ifelse(x  %in%  c("SO_Region_EasternandSouth_EasternAsia", "SO_RegionEastern and South-Eastern Asia" ),"E. & S.E. Asia", 
                                                                                                             ifelse(x  %in%  c("SO_Region_EuropeandNorthernAmerica"),"Europe & N. America", 
                                                                                                                    ifelse(x  %in%  c("SO_Region_LatinAmericaandtheCaribbean", "SO_RegionLatin America and the Caribbean"),"Lat. America & Carib.", 
                                                                                                                           ifelse(x  %in%  c("SO_Region_NorthernAfricaandWesternAsia", "SO_RegionNorthern Africa and Western Asia"),"N. Africa & W. Asia", 
                                                                                                                                  ifelse(x  %in%  c("SO_Region_Oceania", "SO_RegionOceania"),"Oceania", 
                                                                                                                                         ifelse(x %in%  c("SO_Region_Sub_SaharanAfrica", "SO_RegionSub-Saharan Africa"),"Sub-Saharan Africa", 
                                                                                                                                                ifelse(x == "ERI_ratio_scaled","ERI-score",
                                                                                                                                                       ifelse(x == "PS_2","Dangerous situations",
                                                                                                                                                              ifelse(x == "PS_3","Not feeling safe",
                                                                                                                                                                     ifelse(x == "WH_scaled","Work hours", 
                                                                                                                                                                            ifelse(x == "Goal_satisfaction_scaled","Goal progress satisfaction", 
                                                                                                                                                                                   ifelse(x == "national.nnational_0","Non-national",
                                                                                                                                                                                          ifelse(x == "SS1", "Personal relationships" ,
                                                                                                                                                                                                 ifelse(x == "SS2", "Friends and family support" ,
                                                                                                                                                                                                        ifelse(x == "SS3", "Friends and family time" , 
                                                                                                                                                                                                               ifelse(x == "ERI_1", "Heavy workload" , 
                                                                                                                                                                                                                      ifelse(x == "ERI_2", "Many disturbances" , 
                                                                                                                                                                                                                             ifelse(x == "ERI_3", "Increasingly demanding job" , 
                                                                                                                                                                                                                                    ifelse(x == "ADD1", "Not enough resources" , 
                                                                                                                                                                                                                                           ifelse(x == "ADD2", "Not enough funding" , 
                                                                                                                                                                                                                                                  ifelse(x == "ADD3", "Organisational might not exist" , 
                                                                                                                                                                                                                                                         ifelse(x == "ERI_6", "Do not expect undesirable job change" , 
                                                                                                                                                                                                                                                                ifelse(x == "ERI_7", "Good job security" , 
                                                                                                                                                                                                                                                                       ifelse(x == "ERI_8", "Respect and prestige" , 
                                                                                                                                                                                                                                                                              ifelse(x == "ERI_9", "Job advancement" , 
                                                                                                                                                                                                                                                                                     ifelse(x == "ERI_10", "Income is alright" , 
                                                                                                                                                                                                                                                                                            ifelse(x == "ADD4", "Contribution to conservation" , 
                                                                                                                                                                                                                                                                                                   ifelse(x == "ADD5", "Social pride" ,
                                                                                                                                                                                                                                                                                                          ifelse(x == "health", "Physical health" ,
                                                                                                                                                                                                                                                                                                                 ifelse(x == "SO", "Situational optimism" ,
                                                                                                                                                                                                                                                                                                                        ifelse(x == "age_year_scaled", "Age" , x
                                                                                                                                                                                                 
                                                                                                                                                                                   )))))))))))))))))))))))))))))))))))))))))))) }



# Progress function 
prog_funct <- function(x, col_name){
  out <- as.data.frame(table(x))
  out$question <- col_name
  return(out)
}
