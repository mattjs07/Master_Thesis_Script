library(data.table)
library(beepr)
library(fastDummies)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(dplyr)
library(ggplot2)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep()

df <- df %>% mutate(PBD = kpjdxp)
df <- df %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
df <- df %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
df <- df %>%  filter(!is.na(ouverture2))
#second mail sent 28th of Februrary --> 685
df <- df %>%  filter( date == 685)
df <-  df %>%  filter( erreur1 == 0, erreur2 == 0)
# can filter out the wrong mails sent

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Probit ouverture/GLM_computerv2.R")

G2 <- GLM_computer( dependant = "ouverture2", df =df, add_var = "ouverture1")
G2