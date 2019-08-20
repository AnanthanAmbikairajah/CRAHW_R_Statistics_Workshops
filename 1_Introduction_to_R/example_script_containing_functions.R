# Example script containing functions
# E. Walsh
# Last edited: 16/01/2019
# ----------
library(psych)
getwd()
setwd("/Users/Ambi/Desktop/Google Drive/PhD/R/CRAHW R Workshops/1. Introduction to R")
  
## Functions
# Adds two numbers together
    simple_function<-function(number,anothernumber){number + anothernumber}

# Takes two numbers, returns their sum multiplied by the first input.
# Relies on: simple_function
    nested_function<-function(number,anothernumber){added<-simple_function(number,anothernumber)
    multiplied<-added * number
    return(multiplied)}
    
sensornet_dat<- read.csv("sensornet-01__16_1_2019_new_version.csv")
    
    temp_humidity_cor_before<-describe(temperature_transformed)
    
    temp_humidity_cor_after<-describe(sensornet_dat$temperature)
    
    temp_humidity_cor_before
    
    temp_humidity_cor_after
    
    str(sensornet_dat)
    