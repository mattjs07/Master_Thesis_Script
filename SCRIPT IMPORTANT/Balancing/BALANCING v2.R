library(dplyr)
library(data.table)
library(tidyr)
library(fastDummies)
library(beepr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("df_new_variables_29_03.csv"); beep()


########################################
######## MAIN SAMPLES BY DATE ########


df_684 <- filter(df, date == 684)
df_683 <- filter(df, date == 683)

nrow(df_684) == nrow(df_683)

table(df_684$sum1); table(df_684$sum2); table(df_684$sum3); table(df_684$sum2 == 3); table(df_684$sum3 == 3) # --> All have a mixture of 2 and 3 
table(df_684$erreur1);table(df_684$erreur2); table(df_684$erreur3)

table(df_684$main_duration);table(df_684$main_money)

########################################


########################################
######## BUILDING SUB SAMPLES #########


#Date 684
T_C_684 <- filter(df_684, treated == 1 | controle == 1)

T_C_SC_684 <- df_684

T_SC_684 <- filter(df_684, treated == 1 | supercontrole == 1)

N_F_684 <- filter(df_684, Neutral == 1 | Framed == 1)

N_D_684 <- filter(df_684, Neutral == 1 | Duration == 1)

N_M_684 <- filter(df_684, Neutral == 1 | Money == 1)

#Date 683 
T_C_683 <- filter(df_683, treated == 1 | controle == 1)

T_C_SC_683 <- df_683

T_SC_683 <- filter(df_683, treated == 1 | supercontrole == 1)

N_F_683 <- filter(df_683, Neutral == 1 | Framed == 1)

N_D_683 <- filter(df_683, Neutral == 1 | Duration == 1)

N_M_683 <- filter(df_683, Neutral == 1 | Money == 1)

## Other subsamples

#684
obs_mail_684 <- filter( df_684, !is.na(ouverture1))

treated_684 <- filter( df_684, treated == 1)

controle_684 <- filter( df_684, controle == 1)

sup_controle_684 <- filter( df_684, supercontrole == 1)

Neutral_684 <- filter(df_684, Neutral == 1)

Framed_684 <- filter(df_684, Framed == 1)

Money_684 <- filter(df_684, Money == 1)

Duration_684 <- filter(df_684, Duration == 1)

#683

obs_mail_683 <- filter( df_683, !is.na(ouverture1))

treated_683 <- filter( df_683, treated == 1)

controle_683 <- filter( df_683, controle == 1)

sup_controle_683 <- filter( df_683, supercontrole == 1)

Neutral_683 <- filter(df_683, Neutral == 1)

Framed_683 <- filter(df_683, Framed == 1)

Money_683 <- filter(df_683, Money == 1)

Duration_683 <- filter(df_683, Duration == 1)



vars684 <- c("femme", "age", "jeune", "age_intermediaire", "senior", "lower_2nd_edu", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois", "kpjdxp",
             "PBD_inf730", "PBD_sup730", "kqcsjp", "SJR_infmean", "SJR_supmean", "anciennete", "anciennete_inf3", "anciennete_4_6" )

vars683 <- c( "tx_chge", "tx_chge_jeunes","proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")


#######################
####Computing means####

Means <- function(x,y){ 
  lapply( FUN = function(y){mean(x[[y]], na.rm = TRUE)}, X = y)
}

means_Neutral <- Means(x = Neutral_684, vars684) %>% append(Means(x = Neutral_683, vars683)) %>% unlist() 
means_Framed <- Means(x = Framed_684, vars684) %>% append(Means(x = Framed_683, vars683)) %>% unlist()
means_mailed <- Means(x = obs_mail_684, vars684) %>% append(Means(x = obs_mail_683, vars683)) %>% unlist()
means_treated <- Means(x = treated_684, vars684) %>% append(Means(x = treated_683, vars683)) %>% unlist()
means_control <- Means(x = controle_684, vars684) %>%  append(Means(x = controle_683, vars683)) %>%unlist()
means_All <- Means(x = df_684, vars684) %>% append(Means(x = df_683, vars683)) %>%unlist()
means_sup_control <- Means(x = sup_controle_684, vars684) %>%  append(Means(x = sup_controle_683, vars683)) %>%unlist()
means_Money <- Means(x = Money_684, vars684) %>% append(Means(x = Money_683, vars683)) %>% unlist() 
means_Duration <- Means(x = Duration_684, vars684) %>% append(Means(x = Duration_683, vars683)) %>% unlist() 

# !!!! Last part of the table is computed using DECEMBER 2016 a.k.a 683
table_means <- cbind(means_Money, means_Duration, means_Neutral,means_Framed  , means_All,means_treated, means_control, means_sup_control ) %>% as.data.frame()
table_means

options(scipen = 100)
table_means

######################


#########################
#### Computing T-stat ####

library(multiwayvcov)
library(lmtest)

cl.test <- function(x, y, z){ 
  W <- function(X) { ww <- lm( eval(as.symbol(X)) ~ eval(as.symbol(y)),data = z) 
  coeftest(ww, vcov. = cluster.vcov(ww, cluster = z$kcala, stata_fe_model_rank = TRUE))[2,4]
  }
  lapply(x, W)
}  

cl_T_C <- cl.test(vars684, "treated", T_C_684) %>%  append( cl.test(vars683, "treated", T_C_683))

cl_N_F <- cl.test(vars684, "Framed", N_F_684) %>%  append( cl.test(vars683, "Framed", N_F_683))

cl_T_CSC <- cl.test(vars684, "treated", T_C_SC_684) %>%  append( cl.test(vars683, "treated", T_C_SC_683))

cl_T_SC <- cl.test(vars684, "treated", T_SC_684) %>%  append( cl.test(vars683, "treated", T_SC_683))

cl_N_M <- cl.test(vars684, "Money", N_M_684) %>%  append( cl.test(vars683, "Money", N_M_683))

cl_N_D <- cl.test(vars684, "Duration", N_D_684) %>%  append( cl.test(vars683, "Duration", N_D_683))


table_cl <- cbind(cl_N_M, cl_N_D, cl_T_C,cl_N_F, cl_T_CSC, cl_T_SC) %>%  as.data.frame()
table_cl 

#only significant is for "proportion_de_ar", "proportion_de_ld" in N vs F

#########################

TAB <- cbind(table_means, table_cl) %>%  mutate_all(unlist) %>% mutate_all( ~round(.,7)) ; TAB

row.names(TAB) <- c(vars684, vars683); TAB

nrow(Money_684); nrow(Duration_684);nrow(Neutral_684); nrow(Framed_684); nrow(df_684); nrow(treated_684); nrow(controle_684); nrow(sup_controle_684)


library(xtable)
xtable(TAB, digits = 2, )
