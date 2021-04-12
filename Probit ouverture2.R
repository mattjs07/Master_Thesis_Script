library(dplyr)
library(data.table)
library(beepr)
library(fastDummies)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep()

## !! --> the mails were sent 31st january, 28 February, 31st march. 
## WHich month to use ? month preceding or following ? 
## QUestion : when are measured the variables ? (dependant on the variable, most are fixed)
# anciennete = anciennete at 1st January 
# Could, increment it, but would not change the results (gap would remain the same ?)
#time dependant vars are :: indemnisation, age, 
#time invariant : tx and proportion
#What matters = conditions BEFORE OPENING ==> NEED VAR IN PRECEDING MONTH (CURRENT MONTH AT WHICH RECEIVED MAIL)

#CONCLUSION :: EXACT SAME REGRESSIONS AS BEFORE, ONLY CHANGE DATE FROM 684 to 685 and ouverture1 to ouverture 2 


df <- df %>% mutate(PBD = kpjdxp)
df <- df %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
df <- df %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
df <- df %>%  filter(!is.na(ouverture2))
#second mail sent 28th of Februrary --> 685
df <- df %>%  filter( date == 685)
df <-  df %>%  filter( erreur1 == 0, erreur2 == 0)
# can filter out the wrong mails sent


df<- dummy_columns(df, select_columns = "region", remove_first_dummy = TRUE)
FE_region <- names(df[, region_2:region_28])
FE_region <- FE_region %>% paste(collapse = "+")
region <- names(df[, region_2:region_28])

sub_Neutral <- df %>% filter(Neutral == 1)
sub_Framed <- df %>% filter(Framed == 1)


# vars <- c("femme", "age", "jeune", "age_intermediaire", "senior", "lower_2nd_edu", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
#             "PBD_inf730", "PBD_sup730", "kqcsjp", "SJR_infmean", "SJR_supmean", "anciennete", "anciennete_inf3", "anciennete_4_6", "indemnisation", "PBD", "SJR",
#         "single", "married", "divorced", "foreigner", "cdi", "lic")


### !!! Might want to include Fixed effects for region, ALE, etc !! 

vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "anciennete_high", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
          "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "ouverture1")

vars <- paste(vars2, collapse = "+" )

library(lmtest)
library(multiwayvcov)



GLM.clustered <- function(variables, data){
  g <- glm(data = data, paste( "ouverture2 ~", variables, sep = ""), family = binomial(link = "probit"))
  g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
  return(g)
}

glm_df <-GLM.clustered(variables = paste(vars, FE_region, sep = "+"), data = df)


glm_N <- GLM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Neutral)

glm_F <- GLM.clustered(variables = paste(vars, FE_region, sep = "+"), data = sub_Framed)

glm_dif <- GLM.clustered(variables = paste(vars, FE_region,"Framed", sep = "+"), data = df)

# glm dif shows us, that even when controlling for characteristics, Framed individuals tend to open LESS the mail !

library(stargazer)

stargazer(glm_df, glm_N, glm_F, glm_dif, column.labels = c("df", "Neutral", "Framed", "F - N"), type = "text", omit = region)

varsint <- paste(vars2, collapse = "*Framed +")
varsint <- paste(varsint, "*Framed", sep = "")
varsint

varsintplus <- paste(vars, varsint, FE_region, sep = "+")

glm_dif2 <- GLM.clustered(variables = paste(varsintplus, "Framed", sep ="+"), data = df)
#if we interact the characteristics with Framed, we observe that :

library(broom) #for glance() 
nobs <- lapply(function(x){glance(x)["nobs"] %>%  as.character}, X = list(glm_N, glm_F, glm_df,  glm_dif,glm_dif2)) %>% as.character()


# labs <- c("women", "age", "upper secondary education", "higher education", "last contract < 12 months", "last contract <3 months", "time since entry in unemployment", "Benefits", "PBD", "Daily Reference wage", "married", "foreigner")
# labsX <- paste(labs, "X Framed", sep = " ")
# LABS <- c(labs, "Framed", labsX)

stargazer(glm_N, glm_F, glm_df,  glm_dif,glm_dif2, 
          omit = region,
          column.labels = c("Neutral", "Framed","All",  "All" ,"All"),
          dep.var.labels = c("First mail opening"),
          add.lines = list(c( "Observations",nobs)),
          type = "text",
          header = FALSE)


####### Now looking at subsamples inside framed ######## 

df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0)) 

glm_dif3 <- GLM.clustered(variables = paste(vars, FE_region, "Framed + Money", sep ="+"), data = df)

glm_dif4 <- GLM.clustered(variables = paste(vars, FE_region, "Framed + Duration", sep ="+"), data = df)

stargazer(glm_dif, glm_dif3, glm_dif4, type = "text", omit = region) # We observe that the entire impact of "Framed" on opening rate is driven by the negative impact of Money

#What want to investigate = are they different from one group to the other ? WHo opens in Neutral vs Money vs Duration 

############################## 
##### MONEY VS DURATION  ##### 
##############################


sub_MD <- df %>%  filter(Money == 1 | Duration == 1)


glm_MD <- GLM.clustered(variables = paste(vars, FE_region, "Money", sep ="+"), data = sub_MD)

varsint <- paste(vars2, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region,sep = "+")


glm_MD2 <-  GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MD)


vars3 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_left")

varsint <- paste(vars3, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars3, varsint, FE_region, sep = "+")


glm_MD3 <- GLM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)

vars4 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "indemnisation", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", "rel_anciennete")


varsint <- paste(vars4, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars4, varsint, FE_region, sep = "+")


glm_MD4 <- GLM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_MD)


##############################
##### MONEY VS NEUTRAL #######
##############################

sub_MN <- df %>%  filter(Money == 1 | Neutral == 1)


glm_MN <- GLM.clustered(variables = paste(vars, FE_region, "Money", sep ="+"), data = sub_MN)

varsint <- paste(vars2, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region, sep = "+")


glm_MN2 <- GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)

varsint <- paste(vars3, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(paste(vars3, collapse = "+"), varsint, FE_region, sep = "+")


glm_MN3 <- GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)


varsint <- paste(vars4, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(vars4, varsint, FE_region, sep = "+")


glm_MN4 <- GLM.clustered(variables = paste(varsintplus, "Money", sep ="+"), data = sub_MN)


### Same result : Higher educ means lower opening in MOney. And, higher anciennete (length in PBD) means lower interest in Money

##############################
##### DURATION VS NEUTRAL #######
##############################


sub_DN <- df %>%  filter(Duration == 1 | Neutral == 1)


glm_DN <- GLM.clustered(variables = paste(vars, FE_region, "Duration",sep ="+"), data = sub_DN)

varsint <- paste(vars2, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region, sep = "+")


glm_DN2 <- GLM.clustered(variables = paste(varsintplus, "Duration",sep ="+"), data = sub_DN)


varsint <- paste(vars3, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars3, varsint, FE_region, sep = "+")


glm_DN3 <- GLM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_DN)

varsint <- paste(vars4, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars4, varsint, FE_region, sep = "+")


glm_DN4 <- GLM.clustered(variables = paste(varsintplus, "Duration", sep ="+"), data = sub_DN)



### anciennete has a positive impact on opening rate (10%), anciennete has a positive impact (5%)


stargazer(glm_MN, glm_DN, type = "text", column.labels = c("Money", "Duration"), omit = region)
stargazer(glm_dif2, glm_MD2, glm_MN2, glm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = region)
 stargazer(glm_MD3, glm_MN3, glm_DN3, type ="text", omit = region )
stargazer(glm_MD4, glm_MN4, glm_DN4, type ="text", omit = region )































