#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Woody cover
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
library(lavaan)



# dataset:
#browseURL("https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/edit?usp=sharing")
# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQlvRxkFscmASy0CSa_QARDe03owcFQZAdnLa8URySwBfdxus62ZP4_T4FBpNszmG42Rusxq9uaRRQr/pub?gid=1832176472&single=true&output=csv") 
SEMdata
names(SEMdata)
  

# standardize all variables to mean 0 and standard deviation 1
SEMstd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMstd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% dplyr::select(dist2river,elevation,rainfall,cec,burnfreq,hills,woody ),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMstd %>% dplyr::select(dist2river,elevation,rainfall,cec,burnfreq,hills,woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std<-lm(woody~dist2river+elevation+rainfall+cec+burnfreq+hills, data=SEMstd)
summary(multreg_std)
#you have not yet accounted for internal relationships between variables

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Woody_model <- 'woody ~ dist2river + elevation + rainfall + hills + cec + burnfreq 
                  cec~burnfreq+dist2river
                  elevation~hills+dist2river'
Woody_model
Woody_fit <- lavaan::sem(Woody_model, data=SEMstd)

# show the model results
summary(Woody_fit, fit.measures = T, standardized = T, rsquare = T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

Woody_model1 <- 'woody ~ dist2river + elevation + rainfall + hills + cec + burnfreq 
                  cec~burnfreq+rainfall
                  dist2river~rainfall
                  burnfreq~rainfall+dist2river
                  rainfall~elevation
                  elevation~hills'
Woody_model1
Woody_fit1 <- lavaan::sem(Woody_model, data=SEMstd)

# show the model results
summary(Woody_fit1, fit.measures = T, standardized = T, rsquare = T)
# goodness of fit (should be >0.9): CFI and TLI
