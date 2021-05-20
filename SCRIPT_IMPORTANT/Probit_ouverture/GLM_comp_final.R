
GLM_computer <- function(dependant, df, add_var = NULL, rm_var = NULL, lien = "probit"){
  library(sjmisc)
  library(lmtest)
  library(multiwayvcov)
  library(dplyr)
  library(data.table)
  library(fastDummies)
  
  vars2 <- c("episode_rac_numero_mois", "episode_rac_numero","femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes", "primaire","secondaire", "cdi", "lic",
             "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "factor(region)")
  if( !is.null(add_var)){ vars2 <- c(vars2, add_var)}
  
  
  if( !is.null(rm_var)){
    test <- !(rm_var %in% vars2) %>% as.integer() %>% sum()
    if( test > 0){ stop("Trying to remove an inexistant var")}
    
    else{keeper2 <- which(!(vars2 %in% rm_var))
    vars2 <- vars2[keeper2]
    }
  }
  
  GLM.clustered <- function(variables, data){
    if(!is.character(dependant)) stop("dependant variable must be of type character")
    if(!is.data.frame(data)) stop("data is not of type data frame")
    if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
    g <- glm(data = data, paste( dependant, "~", variables, collapse = ""), family = binomial(link = lien))
    n <- nobs(g)
    g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
    return(list(reg =g,n =n)) }
  
  
  vars2p <- paste(vars2, collapse = "+" )
  vars2 <- vars2[-length(vars2)]
  
  ############################## 
  ##### MONEY VS DURATION  ##### 
  ##############################
  
  
  sub_MD<- df[Framed == 1]
  sub_Money <- df[Money == 1] 
  sub_Duration <- df[Duration == 1] 
  
  glm_M <- GLM.clustered(variables = vars2p, data = sub_Money)
  glm_D <- GLM.clustered(variables = vars2p, data = sub_Duration)
  
  glm_MD1 <- GLM.clustered(variables = paste(vars2p, "Duration + episode_rac_numero_mois*Duration", sep ="+"), data = sub_MD)
  
  
  
  varsint <- paste(vars2, collapse = "*Duration +")
  varsint <- paste(varsint, "*Duration", sep = "")
  varsintplus <- paste(vars2p, varsint, sep = "+")
  
  
  glm_MD2 <-  GLM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)
  
  
  
  beepr::beep()
  
  return(list(glm_M =glm_M, glm_D =glm_D, glm_MD1 =glm_MD1, glm_MD2 =glm_MD2))
}
