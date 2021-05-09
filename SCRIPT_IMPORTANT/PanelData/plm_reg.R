library(pkgloadr)
library(plm)
setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

setDTthreads(threads = 8)
data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/dataframe_finalv2.csv ", nThread = 8 ); beep()


data <- data %>%  filter(date >= 684 , !is.na(ouverture1))
data <- data %>%  mutate( T_left = (PBD/30.417) - episode_rac_numero_mois)


#####################
n <- length(data)
data <- fastDummies::dummy_cols(data, select_columns = c("date") , remove_first_dummy = TRUE)
date_FE <- names(data[,(n+1):length(data)])

data$indiv <- data$indiv %>%  as.factor()

n <- length(data)
data <- fastDummies::dummy_cols(data, select_columns = c("indiv"))
date_FE <- names(data[,(n+1):length(data)])
dummies::dummy.data.frame(data = data, names = "indiv")


######################

data$indemnisation <- data$indemnisation %>%  as.numeric()
data$T_left <- data$T_left %>%  as.numeric()
data$episode_rac_numero <- data$episode_rac_numero %>% as.numeric()
data$iar <- data$iar %>% as.numeric()
data$indiv <- data$indiv %>% as.factor()
data$date <- data$date %>% as.factor()

plm(data = data, iar ~  indemnisation, index = c("indiv", "date"), model = "pooling")

library(estimatr)

lm_robust(iar ~ T_left + indemnisation + episode_rac_numero, data = data, clusters = kcala, se_type = "stata", fixed_effects = ~ date + indiv )


data <- data  %>% as.data.frame()

pdata <- pdata.frame(data, index = c("indiv", "date"))






dtnew <- data[, lapply(cols, as.numeric)]


cols <- c("iar", "PBD", "indemnisation", "episode_rac_numero")

data[, cols, with = FALSE] <- sapply(data[, cols, with = FALSE] , as.numeric )



data$


plm(data = pdata, iar ~  PBD, model = "within", effect = "twoways"  )




plm(data = pdata, iar ~ PBD + T_left + indemnisation + episode_rac_numero, model = "within", effect = "twoways"  )



LM.clustered <- function(variables, data){
  if(!is.character(dependant)) stop("dependant variable must be of type character")
  if(!is.data.frame(data)) stop("data is not of type data frame")
  if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
  g <- lm(data = data, paste( dependant, "~", variables, collapse = ""))
  g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
  return(g) }




data <- as.data.table(data)



sapply(data[, ..cols],as.numeric)
