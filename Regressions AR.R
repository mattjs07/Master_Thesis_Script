
#use of part time unemployment --> cumulative number of months with work while on claim "IAR" indemnise activ reduite
#--> cumulative number od hours while on claim 
#--> cum earnings euro while on claim 
# Extensive margin (for those who worked at least one day)  --> compare "activation effect" 
#  Number of days of unemployemt

# number of months with at least one day of unemployment
# exit from unempoyment toward employment for at least three months
# Prob to be out of unemployment in last quarter of PBD  / in last month 


library(dplyr)
library(data.table)
library(beepr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep(8)

names(df)


group_by(df, treated) %>% summarise(meanar = mean(ar, na.rm = TRUE), ar_cum = mean(ar_cum, na.rm = TRUE), iar = mean(iar, na.rm = TRUE))


# 3month, 12 month, 36 months 









