
LM_computer <- function(dependant, df, add_var = NULL, rm_var = NULL){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  library(fastDummies)
  
  print("preparing for duty !")
  
  pb <- txtProgressBar(min = 0, max = 10, style = 3)
  
  vars2 <- c("ouverture1", "femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
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
  
  
  LM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- lm(data = data, paste( dependant, "~", variables, collapse = ""))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
  
  vars2p <- paste(vars2, collapse = "+" )
  
  lm_df <-LM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = df)
  setTxtProgressBar(pb, 1)
  
  lm_N <- LM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = sub_Neutral)
  setTxtProgressBar(pb, 2)
  
  lm_F <- LM.clustered(variables = paste(vars2p, FE_region, sep = "+"), data = sub_Framed)
  setTxtProgressBar(pb, 3)
  
  lm_dif <- LM.clustered(variables = paste(vars2p, FE_region,"Framed", sep = "+"), data = df)
  setTxtProgressBar(pb, 4)
  
  varsint <- paste(vars2, collapse = "*Framed +")
  varsint <- paste(varsint, "*Framed", sep = "")
  varsintplus <- paste(vars2p, varsint, FE_region, sep = "+")
  
  lm_dif2 <- LM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
  setTxtProgressBar(pb, 5)
  
  ####### Now looking at subsamples inside framed ######## 
  
  lm_dif3 <- LM.clustered(variables = paste(vars2p, FE_region, "Framed + Money", sep ="+"), data = df)
  setTxtProgressBar(pb, 6)
  
  ############################## 
  ##### BOTH VS DURATION  ##### 
  ##############################
  
  lm_B1 <- LM.clustered(variables = paste(vars2p, FE_region, "Money + Duration", sep ="+"), data = df)
  setTxtProgressBar(pb, 7)
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint2 <- paste(vars2, collapse = "*Money +")
  varsint2 <- paste(varsint2, "*Money", sep = "")
  varsintplusplus <- paste(vars2p, varsint, varsint2, FE_region, sep = "+")
  
  
  lm_B2 <- LM.clustered(variables = paste(varsintplusplus, "Money + Duration ", sep ="+"), data = df)
  setTxtProgressBar(pb, 8)

  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD <- df %>%  filter(Money == 1 | Duration == 1)
  
  
  lm_MD1 <- LM.clustered(variables = paste(vars2p, FE_region, "Money", sep ="+"), data = sub_MD)
  setTxtProgressBar(pb, 9)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsintplus <- paste(vars2p, varsint, FE_region,sep = "+")
  
  
  lm_MD2 <-  LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  setTxtProgressBar(pb, 10)
  
  
  print("There you go !"); beepr::beep()
  
  return(list(lm_df =lm_df, lm_N = lm_N, lm_F = lm_F, lm_dif =lm_dif,lm_dif2 = lm_dif2, lm_dif3 = lm_dif3, lm_B1 = lm_B1, lm_B2 = lm_B2,
              lm_MD1 =  lm_MD1, lm_MD2 =lm_MD2, region = region))
}
