
GLM_computer <- function(dependant, df, add_var = NULL, rm_var = NULL){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  library(fastDummies)
  
  print("preparing for duty !")
  
  vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
  if( !is.null(add_var)){ vars2 <- c(vars2, add_var)}
  
  vars3 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_left")
  if( !is.null(add_var) ){ vars3 <- c(vars3, add_var)}

  
  vars4 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_anciennete")
  if( !is.null(add_var) ){ vars4 <- c(vars4, add_var)}
  
  if( !is.null(rm_var)){
    test <- !(rm_var %in% c(vars2, vars3, vars4)) %>% as.integer() %>% sum()
    if( test > 0){ stop("Trying to remove an inexistant var")}
      
      else{keeper2 <- which(!(vars2 %in% rm_var))
           keeper3 <- which(!(vars3 %in% rm_var))
           keeper4 <- which(!(vars4 %in% rm_var))
           
           vars2 <- vars2[keeper2]
           vars3 <- vars3[keeper3]
           vars4 <- vars4[keeper4]
      }
  }
  
  n = length(df)
  df<- dummy_columns(df, select_columns = "region", remove_first_dummy = TRUE)
  region <- names(df[, (n+1):length(df)])
  FE_region <- region %>% paste(collapse = "+")
  
  n = length(df)
  df<- dummy_columns(df, select_columns = "date_odd", remove_first_dummy = TRUE)
  dateodd <- names(df[, (n+1):length(df)])
  FE_ODD <-  dateodd  %>% paste(collapse = "+")
  
  Fixed_effect <- paste(FE_region, FE_ODD, sep = "+")
  FE <- c(region, dateodd)
  
  sub_Neutral <- df %>% filter(Neutral == 1)
  sub_Framed <- df %>% filter(Framed == 1)
  
  
  GLM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- glm(data = data, paste( dependant, "~", variables, sep = ""), family = binomial(link = "probit"))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
    
  vars <- paste(vars2, collapse = "+" )
  
  print("Running regressions")
  
  glm_df <-GLM.clustered(variables = paste(vars, Fixed_effect, sep = "+"), data = df)
  
  glm_N <- GLM.clustered(variables = paste(vars, Fixed_effect, sep = "+"), data = sub_Neutral)
  
  glm_F <- GLM.clustered(variables = paste(vars, Fixed_effect, sep = "+"), data = sub_Framed)
  
  glm_dif <- GLM.clustered(variables = paste(vars, Fixed_effect,"Framed", sep = "+"), data = df)
  
  varsint <- paste(vars2, collapse = "*Framed +")
  varsint <- paste(varsint, "*Framed", sep = "")
  varsintplus <- paste(vars, varsint, Fixed_effect, sep = "+")
  
  glm_dif2 <- GLM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
  
  ####### Now looking at subsamples inside framed ######## 
  
  df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0)) 
  
  glm_dif3 <- GLM.clustered(variables = paste(vars, Fixed_effect, "Framed + Money", sep ="+"), data = df)
  
  
  ############################## 
  ##### BOTH VS DURATION  ##### 
  ##############################
  
  print("Both vs duration")
  
  glm_B1 <- GLM.clustered(variables = paste(vars, Fixed_effect, "Money + Duration", sep ="+"), data = df)
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars2, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, Fixed_effect, sep = "+")
  
  
  glm_B2 <- GLM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  

  
  varsint <- paste(vars3, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars3, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, Fixed_effect, sep = "+")
  
  glm_B3 <- GLM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  
  

  
  varsint <- paste(vars4, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars4, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, Fixed_effect, sep = "+")
  
  
  glm_B4 <- GLM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  print("Money vs duration")
  
  sub_MD <- df %>%  filter(Money == 1 | Duration == 1)
  
  
  glm_MD1 <- GLM.clustered(variables = paste(vars, Fixed_effect, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars, varsint, Fixed_effect,sep = "+")
  
  
  glm_MD2 <-  GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars3, collapse = "*Money+")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars3, varsint, Fixed_effect, sep = "+")
  
  
  glm_MD3 <- GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars4, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars4, varsint, Fixed_effect, sep = "+")
  
  
  glm_MD4 <- GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  
  ### anciennete has a positive impact on opening rate (10%), anciennete has a positive impact (5%)
  
  print("here you go!"); beepr::beep()
  return(list(glm_df =glm_df, glm_N = glm_N, glm_F = glm_F, glm_dif =glm_dif,glm_dif2 = glm_dif2, glm_dif3 = glm_dif3, glm_B1 = glm_B1, glm_B2 = glm_B2,
              glm_B3 = glm_B3, glm_B4 = glm_B4, glm_MD1 =  glm_MD1, glm_MD2 =glm_MD2,glm_MD3=glm_MD3, glm_MD4 = glm_MD4, FE = FE))
}
 