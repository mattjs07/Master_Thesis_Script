library(data.table)
library(beepr)
library(fastDummies)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(dplyr)
library(ggplot2)
library(stargazer)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Regressions Outcomes/LM_computerv2.R")

setDTthreads(threads = 0)
data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_29_03.csv "); beep()
data <- data %>% mutate(PBD = kpjdxp)
data <- data %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
data <- data %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
data <- data %>%  filter(!is.na(ouverture1))
data <-  data %>%  filter( erreur1 == 0)



df687 <- data %>%  filter( date == 687)

LM687_h <- LM_computer(dependant = "heures_iar_cum", df = df687)


stargazer(LM687_h$lm_df, LM687_h$lm_N,LM687_h$lm_F,LM687_h$lm_dif,LM687_h$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM687_h$region)
stargazer(LM687_h$lm_dif2, LM687_h$lm_B2,LM687_h$lm_B3,LM687_h$lm_B4, type = "text", omit = LM687_h$region, column.labels = rep("All",4))
stargazer(LM687_h$lm_MD1, LM687_h$lm_MD2, LM687_h$lm_MD3,LM687_h$lm_MD4, type ="text", omit =LM687_h$region, column.labels = rep("D + M",4))



df696 <- data %>%  filter( date == 696)

LM696_h <- LM_computer(dependant = "heures_iar_cum", df = df696)

stargazer(LM696_h$lm_df, LM696_h$lm_N,LM696_h$lm_F,LM696_h$lm_dif,LM696_h$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM696_h$region)
stargazer(LM696_h$lm_dif2, LM696_h$lm_B2,LM696_h$lm_B3,LM696_h$lm_B4, type = "text", omit = LM696_h$region, column.labels = rep("All",4))
stargazer(LM696_h$lm_MD1, LM696_h$lm_MD2, LM696_h$lm_MD3,LM696_h$lm_MD4, type ="text", omit =LM696_h$region, column.labels = rep("D + M",4))




df720 <- data %>%  filter( date == 720)

LM720_h <- LM_computer(dependant = "heures_iar_cum", df = df720)

stargazer(LM720_h$lm_df, LM720_h$lm_N,LM720_h$lm_F,LM720_h$lm_dif,LM720_h$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM720_h$region)
stargazer(LM720_h$lm_dif2, LM720_h$lm_B2,LM720_h$lm_B3,LM720_h$lm_B4, type = "text", omit = LM720_h$region, column.labels = rep("All",4))
stargazer(LM720_h$lm_MD1, LM720_h$lm_MD2, LM720_h$lm_MD3,LM720_h$lm_MD4, type ="text", omit =LM720_h$region, column.labels = rep("D + M",4))














