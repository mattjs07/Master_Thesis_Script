
LPM_computer <- function(dependant, df, add_var = NULL, rm_var = NULL){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  library(fastDummies)
  
  vars2 <- c("episode_rac_numero_mois", "episode_rac_numero","femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "factor(region)")
  if( !is.null(add_var)){ vars2 <- c(vars2, add_var)}
  
  
  if( !is.null(rm_var)){
    test <- !(rm_var %in% vars2) %>% as.integer() %>% sum()
    if( test > 0){ stop("Trying to remove an inexistant var")}
    
    else{keeper2 <- which(!(vars2 %in% rm_var))
    vars2 <- vars2[keeper2]
    }
  }
  
  LPM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- lm(data = data, paste( dependant, "~", variables, collapse = ""))
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(g) }
  
  
  vars2p <- paste(vars2, collapse = "+" )
  vars2 <- vars2[-length(vars2)]
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD<- df[Framed == 1]
  sub_Money <- df[Money == 1] 
  sub_Duration <- df[Duration == 1] 
  
  lpm_M <- LPM.clustered(variables = vars2p, data = sub_Money)
  lpm_D <- LPM.clustered(variables = vars2p, data = sub_Duration)
  
  lpm_MD1 <- LPM.clustered(variables = paste(vars2p, "Duration + anciennete*Duration", sep ="+"), data = sub_MD)
  

  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsintplus <- paste(vars2p, varsint, sep = "+")
  
  
  lpm_MD2 <-  LPM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)

  
  
  beepr::beep()
  
  return(list(lpm_M, lpm_D, lpm_MD1, lpm_MD2))
}
