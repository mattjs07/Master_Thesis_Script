library(pkgloadr)
library(latex2exp)
library(gtools)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_finalv2.csv", nThread = 8)


df <- data

df <- df %>%  filter( date == 684)#first mail sent the 31 january --> 684
df <- df %>%  filter(!is.na(ouverture1))
df <-  df %>%  filter( erreur1 == 0) # filter out the wrong mails sent
df[, age2 := age^2]

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT_IMPORTANT/Probit_ouverture/GLM_comp_final.R")

L1 <- GLM_computer( dependant = "ouverture1", df =df)


stargazer(L1$glm_M, L1$glm_D,L1$glm_MD1,L1$glm_MD2, type = "text", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c("episode_rac_numero_mois","episode_rac_numero_mois:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X") ) )


######## Stratification ###### 

vars2 <- c("femme", "age","age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "episode_rac_numero_mois", "indemnisation", "PBD",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", 
           "married", "primaire","secondaire", "cdi", "lic")

vars2 <- paste(vars2, collapse = "+")

df_framed = df[Framed == 1 ]

#Anciennete
df_framed$quant_anciennete = as.integer(quantcut(df_framed$episode_rac_numero_mois, 3))

for( i in c(1,3)){
  g <- lm(data = df_framed[quant_anciennete == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""), family = binomial(link = "probit"))
  stargazer(g, type = "latex", keep = "Duration")
  
}

# PBD
df_framed$quant_PBD = as.integer(quantcut(df_framed$PBD, 3))

for( i in c(1,3)){
  g <- glm(data = df_framed[quant_PBD == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""), family = binomial(link = "probit"))
  stargazer(g, type = "latex", keep = "Duration")
  
}

#Time_Left
df_framed[, T_left := PBD - episode_rac_numero_mois*30.4]
df_framed[, quant_T_left := as.integer(quantcut(T_left,3))]

for( i in c(1,3)){
  g <- glm(data = df_framed[quant_T_left == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""), family = binomial(link = "probit"))
  stargazer(g, type = "latex", keep = "Duration")
  
}

#relative time left
df_framed[, T_left_rel := (PBD - episode_rac_numero_mois*30.4)/PBD]
df_framed[, quant_T_left_rel := as.integer(quantcut(T_left_rel,3))]

for( i in c(1,3)){
  g <- glm(data = df_framed[quant_T_left_rel == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""), family = binomial(link = "probit"))
  stargazer(g, type = "latex", keep = "Duration")
  
}

#taux de chomage
df_framed[, quant_tx_chge := as.integer(quantcut(tx_chge,3))]

for( i in c(1,3)){
  g <- glm(data = df_framed[quant_tx_chge == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""), family = binomial(link = "probit"))
  stargazer(g, type = "latex", keep = "Duration")
  
} 
