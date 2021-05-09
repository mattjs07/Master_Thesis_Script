library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_finalv2.csv", nThread = 8)

df <- data
df <- df %>%  filter(!is.na(ouverture1))
#second mail sent 28th of Februrary --> 685
df <- df %>%  filter( date == 685)
df <-  df %>%  filter( erreur1 == 0, erreur2 == 0)
# can filter out the wrong mails sent

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT_IMPORTANT/Probit_ouverture/GLM_computerv5.R")

G2 <- GLM_computer( dependant = "ouverture2", df =df, add_var = "ouverture1")
G2

stargazer(G2$glm_df, G2$glm_N,G2$glm_F,G2$glm_dif,G2$glm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = G2$region)
stargazer(G2$glm_dif2, G2$glm_B2,G2$glm_MD1, G2$glm_MD2, type = "text", omit = G2$region, column.labels = rep(c("All","D + M"),each =2))




G2.2 <- GLM_computer( dependant = "ouverture2", df =df, add_var = c("ouverture1","anciennete^2"))
G2.2