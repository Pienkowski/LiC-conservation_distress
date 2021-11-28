# LiC-distress
 
This repository contains the R code used to prepare and analyse the data presented in the manuscript Protecting those who protect nature by supporting conservationists’ mental wellbeing. The repository contains: 

1. LiC-distress_process.R: This takes the dataset DF.DIS.1.Rdata (available at: https://figshare.com/s/24ba94884bc779cdb8aa), creates the composite and latent variables used in the analysis, and generates the ten imputed datasets. This code must be run to generate the imputed datasets before the LiC-distress_analysis.R can be implemented. 
2. LiC-distress_functions.R: This contains some functions used in the other two scripts. 
3. LiC-distress_analysis.R: The implements the primary and supplementary analysis. 
4. Variables.csv: This contains the questions and variable codes, used when visualising the data in LiC-distress_process.R. 
