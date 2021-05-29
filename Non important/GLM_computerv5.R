
GLM_computer <- function(dependant, df, add_var = NULL, rm_var = NULL, lien = "probit"){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  library(fastDummies)
  
  print("preparing for duty !")
  
  pb <- txtProgressBar(min = 0, max = 10, style = 3)
  
  vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
  if( !is.null(add_var)){ vars2 <- c(vars2, add_var)}

  
  if( !is.null(rm_var)){
    test <- !(rm_var %in% vars2) %>% as.integer() %>% sum()
    if( test > 0){ stop("Trying to remove an inexistant var")}
    
    else{keeper2 <- which(!(vars2 %in% rm_var))
    vars2 <- vars2[keeper2]
    }
  }
  
  n = length(df)
  df<- dummy_columns(df, select_columns = "region", remove_first_dummy = TRUE)
  region <- names(df[, (n+1):length(df)])
  FE_region <- region %>% paste(collapse = "+")
  
  sub_Neutral <- df %>% filter(Neutral == 1)
  sub_Framed <- df %>% filter(Framed == 1)
  
  
  GLM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- glm(data = data, paste( dependant, "~", variables, collapse = ""), family = binomial(link = lien))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
  
  vars2p <- paste(vars2, collapse = "+" )
  
  glm_df <-GLM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = df)
  setTxtProgressBar(pb, 1)
  
  glm_N <- GLM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = sub_Neutral)
  setTxtProgressBar(pb, 2)
  
  glm_F <- GLM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = sub_Framed)
  setTxtProgressBar(pb, 3)
  
  glm_dif <- GLM.clustered(variables = paste(vars2p, FE_region,"Framed", sep = "+"), data = df)
  setTxtProgressBar(pb, 4)
  
  varsint <- paste(vars2, collapse = "*Framed +")
  varsint <- paste(varsint, "*Framed", sep = "")
  varsintplus <- paste(vars2p, varsint, FE_region, sep = "+")
  
  glm_dif2 <- GLM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
  setTxtProgressBar(pb, 5)
  
  ####### Now looking at subsamples inside framed ######## 
  
  glm_dif3 <- GLM.clustered(variables = paste(vars2p, FE_region, "Framed + Money", sep ="+"), data = df)
  setTxtProgressBar(pb, 6)
  
  ############################## 
  ##### BOTH VS DURATION  ##### 
  ##############################
  
  glm_B1 <- GLM.clustered(variables = paste(vars2p, FE_region, "Money + Duration", sep ="+"), data = df)
  setTxtProgressBar(pb, 7)
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars2, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars2p, varsint, varsint2, FE_region, sep = "+")
  
  
  glm_B2 <- GLM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  setTxtProgressBar(pb, 8)
  
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD <- df %>%  filter(Money == 1 | Duration == 1)
  
  
  glm_MD1 <- GLM.clustered(variables = paste(vars2p, FE_region, "Money", sep ="+"), data = sub_MD)
  setTxtProgressBar(pb, 9)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars2p, varsint, FE_region,sep = "+")
  
  
  glm_MD2 <-  GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  setTxtProgressBar(pb, 10)
  

  print("There you go !"); beepr::beep()
  
  return(list(glm_df =glm_df, glm_N = glm_N, glm_F = glm_F, glm_dif =glm_dif,glm_dif2 = glm_dif2, glm_dif3 = glm_dif3, glm_B1 = glm_B1, glm_B2 = glm_B2,
              glm_MD1 =  glm_MD1, glm_MD2 =glm_MD2, region = region))
}
