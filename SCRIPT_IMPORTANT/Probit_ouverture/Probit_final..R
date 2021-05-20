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


stargazer(L1$glm_M$reg, L1$glm_D$reg, L1$glm_MD1$reg, L1$glm_MD2$reg, type = "latex", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c("episode_rac_numero_mois","episode_rac_numero_mois:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X"),
                            c( "Obs", L1$glm_M$n, L1$glm_D$n,L1$glm_MD1$n, L1$glm_MD2$n) ))


######## Stratification ###### 

vars2 <- c("femme", "age","age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "episode_rac_numero_mois", "indemnisation", "PBD",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", 
           "married", "primaire","secondaire", "cdi", "lic")

vars2 <- paste(vars2, collapse = "+")

df_framed = df[Framed == 1 ]

glm.obs <- function(dependant,variables, data){
  if(!is.character(dependant)) stop("dependant variable must be of type character")
  if(!is.data.frame(data)) stop("data is not of type data frame")
  if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
  g <- glm(data = data, paste( dependant, "~", variables, collapse = ""), family = binomial(link = "probit"))
  n <- nobs(g)
  g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
  return(list(reg =g,n =n)) }


#Anciennete
df_framed$quant_anciennete = as.integer(quantcut(df_framed$episode_rac_numero_mois, 3))

for( i in c(1,3)){
  g <- glm.obs(data = df_framed[quant_anciennete == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "latex", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}


# PBD
df_framed$quant_PBD = as.integer(quantcut(df_framed$PBD, 3))

for( i in c(1,3)){
  g <- glm.obs(data = df_framed[quant_PBD == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "latex", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#Time_Left
df_framed[, T_left := PBD - episode_rac_numero_mois*30.4]
df_framed[, quant_T_left := as.integer(quantcut(T_left,3))]

for( i in c(1,3)){
  g <- glm.obs(data = df_framed[quant_T_left == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "latex", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#relative time left
df_framed[, T_left_rel := (PBD - episode_rac_numero_mois*30.4)/PBD]
df_framed[, quant_T_left_rel := as.integer(quantcut(T_left_rel,3))]

for( i in c(1,3)){
  g <- glm.obs(data = df_framed[quant_T_left_rel == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "latex", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#taux de chomage
df_framed[, quant_tx_chge := as.integer(quantcut(tx_chge,3))]

for( i in c(1,3)){
  g <- glm.obs(data = df_framed[quant_tx_chge == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "latex", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}
