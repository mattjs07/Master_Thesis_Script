library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_final.csv", nThread = 8)

df <- data
df <- df %>%  filter(!is.na(ouverture1))
#second mail sent 28th of Februrary --> 685
df <- df %>%  filter( date == 685)
df <-  df %>%  filter( erreur1 == 0, erreur2 == 0)
# can filter out the wrong mails sent

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Probit ouverture/GLM_computerv4.R")

G2 <- GLM_computer( dependant = "ouverture2", df =df, add_var = "ouverture1")
G2

G2.2 <- GLM_computer( dependant = "ouverture2", df =df, add_var = c("ouverture1","anciennete^2"))
G2.2