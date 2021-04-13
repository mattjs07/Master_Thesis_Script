library(dplyr)
library(data.table)
library(beepr)
library(fastDummies)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_29_03.csv "); beep()

df <- df %>% mutate(PBD = kpjdxp)
df <- df %>% mutate(SJR = kqcsjp)
df <- df %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
df <- df %>%  filter(!is.na(ouverture3))
#second mail sent 31st of March --> 686
df <- df %>%  filter( date == 686)
df <-  df %>%  filter( erreur1 == 0, erreur2 == 0, erreur3 == 0)
# can filter out the wrong mails sent

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Probit ouverture/GLM_computerv2.R")

G3 <- GLM_computer( dependant = "ouverture3", df =df, add_var = c("ouverture1", "ouverture2"))
G3













