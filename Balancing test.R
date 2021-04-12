library(haven)
library(dplyr)
library(data.table)
library(fastDummies)
library(readstata13)
library(beepr)

df <- read.dta13("Data/CORECTPANEL.dta"); beep(8)

# How to compute averages --> PS topics lm() 

#testing if averages match :: using df_t1 as need a df to test

mean(df_t1$femme)
lm(data = df_t1, controle ~ femme -1 ) 

lm(data = df_t1, traitement ~ femme -1 ) 
