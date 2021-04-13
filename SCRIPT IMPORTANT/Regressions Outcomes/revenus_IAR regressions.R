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

LM687_r <- LM_computer(dependant = "revenu_iar_cum", df = df687)


stargazer(LM687_r$lm_df, LM687_r$lm_N,LM687_r$lm_F,LM687_r$lm_dif,LM687_r$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM687_r$region)
stargazer(LM687_r$lm_dif2, LM687_r$lm_B2,LM687_r$lm_B3,LM687_r$lm_B4, type = "text", omit = LM687_r$region, column.labels = rep("All",4))
stargazer(LM687_r$lm_MD1, LM687_r$lm_MD2, LM687_r$lm_MD3,LM687_r$lm_MD4, type ="text", omit =LM687_r$region, column.labels = rep("D + M",4))



df696 <- data %>%  filter( date == 696)

LM696_r <- LM_computer(dependant = "revenu_iar_cum", df = df696)

stargazer(LM696_r$lm_df, LM696_r$lm_N,LM696_r$lm_F,LM696_r$lm_dif,LM696_r$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM696_r$region)
stargazer(LM696_r$lm_dif2, LM696_r$lm_B2,LM696_r$lm_B3,LM696_r$lm_B4, type = "text", omit = LM696_r$region, column.labels = rep("All",4))
stargazer(LM696_r$lm_MD1, LM696_r$lm_MD2, LM696_r$lm_MD3,LM696_r$lm_MD4, type ="text", omit =LM696_r$region, column.labels = rep("D + M",4))



df720 <- data %>%  filter( date == 720)

LM720_r <- LM_computer(dependant = "revenu_iar_cum", df = df720)

stargazer(LM720_r$lm_df, LM720_r$lm_N,LM720_r$lm_F,LM720_r$lm_dif,LM720_r$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM720_r$region)
stargazer(LM720_r$lm_dif2, LM720_r$lm_B2,LM720_r$lm_B3,LM720_r$lm_B4, type = "text", omit = LM720_r$region, column.labels = rep("All",4))
stargazer(LM720_r$lm_MD1, LM720_r$lm_MD2, LM720_r$lm_MD3,LM720_r$lm_MD4, type ="text", omit =LM720_r$region, column.labels = rep("D + M",4))













