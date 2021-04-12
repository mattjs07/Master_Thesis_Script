library(dplyr)
library(data.table)
library(beepr)
library(fastDummies)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep(8)

###INTRO####
df %>% group_by(objet1) %>% summarise(m = mean(ouverture1))
############


df <- df %>% mutate(PBD = kpjdxp)
df <- df %>% mutate(SJR = kqcsjp)
df <- df %>%  filter(!is.na(ouverture1))
#first mail sent the 31 january --> 684
df <- df %>%  filter( date == 684)
df <-  df %>%  filter( erreur1 == 0)
# can filter out the wrong mails sent


df<- dummy_columns(df, select_columns = "region", remove_first_dummy = TRUE)
FE_region <- names(df[, region_2:region_28])
FE_region <- FE_region %>% paste(collapse = "+")

sub_Neutral <- df %>% filter(Neutral == 1)
sub_Framed <- df %>% filter(Framed == 1)


# vars <- c("femme", "age", "jeune", "age_intermediaire", "senior", "lower_2nd_edu", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
#             "PBD_inf730", "PBD_sup730", "kqcsjp", "SJR_infmean", "SJR_supmean", "anciennete", "anciennete_inf3", "anciennete_4_6", "indemnisation", "PBD", "SJR",
#         "single", "married", "divorced", "foreigner", "cdi", "lic")


### !!! Might want to include Fixed effects for region, ALE, etc !! 

vars <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
          "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")

vars <- paste(vars, collapse = "+" )

library(lmtest)
library(multiwayvcov)


glm_df <- glm(data = df, paste( "ouverture1 ~", paste(vars, FE_region, sep = "+"), sep = ""), family = binomial(link = "probit"))
glm_df <- glm_df %>%  coeftest( vcov. = cluster.vcov( glm_df, cluster = df$kcala, stata_fe_model_rank = TRUE))

glm_N <- glm(data = sub_Neutral, paste( "ouverture1 ~", paste(vars, FE_region, sep = "+"), sep = ""), family = binomial(link = "probit")) 
glm_N <- glm_N %>%  coeftest( vcov. = cluster.vcov( glm_N, cluster = sub_Neutral$kcala, stata_fe_model_rank = TRUE))

glm_F <- glm(data = sub_Framed, paste( "ouverture1 ~", paste(vars, FE_region, sep = "+"), sep = ""), family = binomial(link = "probit"))
glm_F <- glm_F %>%  coeftest( vcov. = cluster.vcov( glm_F, cluster = sub_Framed$kcala, stata_fe_model_rank = TRUE))

glm_dif <- glm(data = df, paste( "ouverture1 ~", paste(vars, FE_region, sep = "+"), "+ Framed", sep = ""), family = binomial(link = "probit"))
glm_dif <- glm_dif %>%  coeftest( vcov. = cluster.vcov( glm_dif, cluster = df$kcala, stata_fe_model_rank = TRUE))
# glm dif shows us, that even when controlling for characteristics, Framed individuals tend to open LESS the mail !


library(stargazer)

stargazer(glm_df, glm_N, glm_F, glm_dif, column.labels = c("df", "Neutral", "Framed", "F - N"), type = "latex")

vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
  "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
  "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")


varsint <- paste(vars2, collapse = "*Framed +")
varsint <- paste(varsint, "*Framed", sep = "")
varsint

varsintplus <- paste(vars, varsint, FE_region, sep = "+")

glm_dif2 <- glm(data = df, paste( "ouverture1 ~", varsintplus, sep = ""), family = binomial(link = "probit"))
glm_dif2 <- glm_dif2 %>%  coeftest( vcov. = cluster.vcov( glm_dif2, cluster = df$kcala, stata_fe_model_rank = TRUE))
glm_dif2
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
          type = "latex",
          header = FALSE)


####### Now looking at subsamples inside framed ######## 

df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0)) 

glm_dif3 <- glm(data = df, paste( "ouverture1 ~", paste(vars, FE_region,sep ="+"), "+ Framed + Money", sep = ""), family = binomial(link = "probit"))
glm_dif3 <- glm_dif3 %>%  coeftest( vcov. = cluster.vcov( glm_dif3, cluster = df$kcala, stata_fe_model_rank = TRUE))


glm_dif4 <- glm(data = df, paste( "ouverture1 ~", paste(vars, FE_region,sep ="+"), "+ Framed + Duration", sep = ""), family = binomial(link = "probit"))
glm_dif4 <- glm_dif4%>%  coeftest( vcov. = cluster.vcov( glm_dif4, cluster = df$kcala, stata_fe_model_rank = TRUE))

stargazer(glm_dif, glm_dif3, glm_dif4, type = "text") # We observe that the entire impact of "Framed" on opening rate is driven by the negative impact of Money

#What want to investigate = are they different from one group to the other ? WHo opens in Neutral vs Money vs Duration 

############################## 
##### MONEY VS DURATION  ##### 
##############################


sub_MD <- df %>%  filter(Money == 1 | Duration == 1)


glm_MD <- glm(data = sub_MD, paste( "ouverture1 ~", paste(vars, FE_region,sep ="+"), "+ Money", sep = ""), family = binomial(link = "probit"))
glm_mD <- glm_MD %>%  coeftest( vcov. = cluster.vcov( glm_MD, cluster = sub_MD$kcala, stata_fe_model_rank = TRUE))


varsint <- paste(vars2, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region,sep = "+")


glm_MD2 <- glm(data = sub_MD, paste( "ouverture1 ~", varsintplus, "+ Money", sep = ""), family = binomial(link = "probit"))
glm_mD2 <- glm_MD2 %>%  coeftest( vcov. = cluster.vcov( glm_MD2, cluster = sub_MD$kcala, stata_fe_model_rank = TRUE))

##############################
##### MONEY VS NEUTRAL #######
##############################

sub_MN <- df %>%  filter(Money == 1 | Neutral == 1)


glm_MN <- glm(data = sub_MN , paste( "ouverture1 ~", paste(vars, FE_region,sep ="+"), "+ Money", sep = ""), family = binomial(link = "probit"))
glm_MN <- glm_MN %>%  coeftest( vcov. = cluster.vcov( glm_MN, cluster = sub_MN$kcala, stata_fe_model_rank = TRUE))


varsint <- paste(vars2, collapse = "*Money +")
varsint <- paste(varsint, "*Money", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region, sep = "+")


glm_MN2 <- glm(data = sub_MN , paste( "ouverture1 ~", varsintplus, "+ Money", sep = ""), family = binomial(link = "probit"))
glm_MN2 <- glm_MD2 %>%  coeftest( vcov. = cluster.vcov( glm_MN2, cluster = sub_MN$kcala, stata_fe_model_rank = TRUE))
### Same result : Higher educ means lower opening in MOney. And, higher anciennete (length in PBD) means lower interest in Money

##############################
##### DURATION VS NEUTRAL #######
##############################


sub_DN <- df %>%  filter(Duration == 1 | Neutral == 1)


glm_DN <- glm(data = sub_DN, paste( "ouverture1 ~", paste(vars, FE_region,sep ="+"), "+ Duration", sep = ""), family = binomial(link = "probit"))
glm_DN <- glm_DN %>%  coeftest( vcov. = cluster.vcov( glm_DN, cluster = sub_DN$kcala, stata_fe_model_rank = TRUE))


varsint <- paste(vars2, collapse = "*Duration +")
varsint <- paste(varsint, "*Duration", sep = "")
varsint
varsintplus <- paste(vars, varsint, FE_region, sep = "+")


glm_DN2 <- glm(data = sub_DN , paste( "ouverture1 ~", varsintplus, "+ Duration", sep = ""), family = binomial(link = "probit"))
glm_DN2 <- glm_DN2 %>%  coeftest( vcov. = cluster.vcov( glm_DN2, cluster = sub_DN$kcala, stata_fe_model_rank = TRUE), save = TRUE)
### anciennete has a positive impact on opening rate (10%), anciennete has a positive impact (5%)


region <- names(df[, region_2:region_28])

stargazer(glm_MN, glm_DN, type = "text", column.labels = c("Money", "Duration"), omit = region)
stargazer(glm_dif2, glm_MD2, glm_MN2, glm_DN2, type = "latex", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = region)





#### Looking at subset susceptible to drive results ### 

df <- df %>% mutate(anciennete_sup =  ifelse(anciennete >= mean(anciennete),1,0), anciennete_inf = ifelse(anciennete < mean(anciennete), 1,0) )

Anc_inf <- filter(df,anciennete_inf == 1)
Anc_sup <- filter(df,anciennete_sup == 1)


varsint <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
             "anciennete", "indemnisation", "PBD", "SJR", "married", "foreigner")


varsint <- paste(varsint, collapse = "*Framed +")
varsint <- paste(varsint, "*Framed", sep = "")
varsint

varsintplus <- paste(vars, varsint, sep = "+")

glm_inf <- glm(data = Anc_inf, paste( "ouverture1 ~", vars,"+Framed", sep = ""), family = binomial(link = "probit"))
glm_inf <- glm_inf %>%  coeftest( vcov. = cluster.vcov( glm_inf, cluster = Anc_inf$kcala, stata_fe_model_rank = TRUE))
glm_inf

glm_sup <- glm(data = Anc_sup, paste( "ouverture1 ~",vars,"+Framed", sep = ""), family = binomial(link = "probit"))
glm_sup <- glm_sup %>%  coeftest( vcov. = cluster.vcov( glm_sup, cluster = Anc_sup$kcala, stata_fe_model_rank = TRUE))
glm_sup



GLM.clustered <- function(variables, data){
  g <- glm(data = data, paste( "ouverture1 ~", variables, sep = ""), family = binomial(link = "probit"))
  g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
  return(g)
}

GLM.clustered(data = Anc_plus, variables = paste(vars, "+ Framed", sep = ""))







# Could build a fonction that returns a list with list(glm.clustered, P values ), so that I can feed P val to stargazer  







