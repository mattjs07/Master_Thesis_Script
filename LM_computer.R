
LM_computer <- function(dependant, df){
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
    g <- lm(data = data, paste( dependant, "~", variables, sep = ""), family = binomial(link = "probit"))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
  vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
  
  vars <- paste(vars2, collapse = "+" )
  
  lm_df <-LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = df)
  
  lm_N <- LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Neutral)
  
  lm_F <- LM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Framed)
  
  lm_dif <- LM.clustered(variables = paste(vars, FE_region,"Framed", sep = "+"), data = df)
  
  # lm dif shows us, that even when controlling for characteristics, Framed individuals tend to open LESS the mail !
  
  
  varsint <- paste(vars2, collapse = "*Framed +")
  varsint <- paste(varsint, "*Framed", sep = "")
  varsint
  
  varsintplus <- paste(vars, varsint, FE_region, sep = "+")
  
  lm_dif2 <- LM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
  
  ####### Now looking at subsamples inside framed ######## 
  
  df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0)) 
  
  lm_dif3 <- LM.clustered(variables = paste(vars, FE_region, "Framed + Money", sep ="+"), data = df)
  
  lm_dif4 <- LM.clustered(variables = paste(vars, FE_region, "Framed + Duration", sep ="+"), data = df)
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD <- df %>%  filter(Money == 1 | Duration == 1)
  
  
  lm_MD <- LM.clustered(variables = paste(vars, FE_region, "Money", sep ="+"), data = sub_MD)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsint
  varsintplus <- paste(vars, varsint, FE_region,sep = "+")
  
  
  lm_MD2 <-  LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)
  
  
  vars3 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_left")
  
  varsint <- paste(vars3, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint
  varsintplus <- paste(vars3, varsint, FE_region, sep = "+")
  
  
  lm_MD3 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)
  
  vars4 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_anciennete")
  
  
  varsint <- paste(vars4, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint
  varsintplus <- paste(vars4, varsint, FE_region, sep = "+")
  
  
  lm_MD4 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)
  
  
  ##############################
  ##### MONEY VS NEUTRAL #######
  ##############################
  
  sub_MN <- df %>%  filter(Money == 1 | Neutral == 1)
  
  
  lm_MN <- LM.clustered(variables = paste(vars, FE_region, "Money", sep ="+"), data = sub_MN)
  
  varsint <- paste(vars2, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsint
  varsintplus <- paste(vars, varsint, FE_region, sep = "+")
  
  
  lm_MN2 <- LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)
  
  
  
  varsint <- paste(vars3, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsint
  varsintplus <- paste(paste(vars3, collapse = "+"), varsint, FE_region, sep = "+")
  
  
  lm_MN3 <- LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)
  
  
  varsint <- paste(vars4, collapse = "*Money +")
  varsint <- paste(varsint, "*Money", sep = "")
  varsint
  varsintplus <- paste(vars4, varsint, FE_region, sep = "+")
  
  
  lm_MN4 <- LM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)
  
  
  ### Same result : Higher educ means lower opening in MOney. And, higher anciennete (length in PBD) means lower interest in Money
  
  ##############################
  ##### DURATION VS NEUTRAL #######
  ##############################
  
  
  sub_DN <- df %>%  filter(Duration == 1 | Neutral == 1)
  
  
  lm_DN <- LM.clustered(variables = paste(vars, FE_region, "Duration",sep ="+"), data = sub_DN)
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint
  varsintplus <- paste(vars, varsint, FE_region, sep = "+")
  
  
  lm_DN2 <- LM.clustered(variables = paste(varsintplus, "Duration",sep ="+"), data = sub_DN)
  
  
  varsint <- paste(vars3, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint
  varsintplus <- paste(vars3, varsint, FE_region, sep = "+")
  
  
  lm_DN3 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_DN)
  
  varsint <- paste(vars4, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsint
  varsintplus <- paste(vars4, varsint, FE_region, sep = "+")
  
  
  lm_DN4 <- LM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_DN)
  
  
  
  ### anciennete has a positive impact on opening rate (10%), anciennete has a positive impact (5%)
  
  return(list(lm_N = lm_N, lm_F = lm_F, lm_df =lm_df,  lm_dif =lm_dif,lm_dif2 = lm_dif2,lm_MN = lm_MN, lm_DN = lm_DN,
              lm_dif2 = lm_dif2, lm_MD2 =lm_MD2, lm_MN2 =lm_MN2, lm_DN2=lm_DN2,lm_MD3=lm_MD3, lm_MN3=lm_MN3, lm_DN3=lm_DN3,
              lm_MD4 = lm_MD4, lm_MN4=lm_MN4, lm_DN4 =lm_DN4, region = region))
}
