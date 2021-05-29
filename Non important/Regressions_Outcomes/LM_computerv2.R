
LM_computer <- function(dependant, df, add_var = NULL){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  
  df<- dummy_columns(df, select_columns = "region", remove_first_dummy = TRUE)
  FE_region <- names(df[, region_2:region_28])
  FE_region <- FE_region %>% paste(collapse = "+")
  region <- names(df[, region_2:region_28])
  
  sub_Neutral <- df %>% filter(Neutral == 1)
  sub_Framed <- df %>% filter(Framed == 1)
  
  
  LM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- lm(data = data, paste( dependant, "~", variables, sep = ""))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
  
  
  vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
  if( !is.null(add_var) == 1){ vars2 <- c(vars2, add_var)}
  
  vars <- paste(vars2, collapse = "+" )
  
  lm_df <-LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = df)
  
  lm_N <- LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Neutral)
  
  lm_F <- LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Framed)
  
  lm_dif <- LM.clustered(variables = paste(vars, FE_region,"Framed", sep = "+"), data = df)
  
  varsint <- paste(vars2, collapse = "*Framed +")
  varsint <- paste(varsint, "*Framed", sep = "")
  varsintplus <- paste(vars, varsint, FE_region, sep = "+")
  
  lm_dif2 <- LM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
  
  ####### Now looking at subsamples inside framed ######## 
  
  df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0))
  
  ############################## 
  ##### BOTH VS DURATION  ##### 
  ##############################
  
  lm_B1 <- LM.clustered(variables = paste(vars, FE_region, "Money + Duration", sep ="+"), data = df)
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars2, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, FE_region, sep = "+")
  
  
  lm_B2 <- LM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  
  
  vars3 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_left")
  if( !is.null(add_var) == 1){ vars3 <- c(vars3, add_var)}
  
  varsint <- paste(vars3, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars3, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, FE_region, sep = "+")
  
  lm_B3 <- LM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  
  
  vars4 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_anciennete")
  if( !is.null(add_var) == 1){ vars4 <- c(vars4, add_var)}
  
  varsint <- paste(vars4, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars4, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars, varsint, varsint2, FE_region, sep = "+")
  
  
  lm_B4 <- LM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD <- df %>%  filter(Money == 1 | Duration == 1)
  
  
  lm_MD1 <- LM.clustered(variables = paste(vars, FE_region, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars, varsint, FE_region,sep = "+")
  
  
  lm_MD2 <-  LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars3, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsintplus <- paste(vars3, varsint, FE_region, sep = "+")
  
  
  lm_MD3 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars4, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsintplus <- paste(vars4, varsint, FE_region, sep = "+")
  
  
  lm_MD4 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)
  
  ### anciennete has a positive impact on opening rate (10%), anciennete has a positive impact (5%)
  
  return(list(lm_df =lm_df, lm_N = lm_N, lm_F = lm_F, lm_dif =lm_dif,lm_dif2 = lm_dif2, lm_B1 = lm_B1, lm_B2 = lm_B2,
              lm_B3 = lm_B3, lm_B4 = lm_B4, lm_MD1 =  lm_MD1, lm_MD2 =lm_MD2,lm_MD3=lm_MD3, lm_MD4 = lm_MD4, region = region))
}
