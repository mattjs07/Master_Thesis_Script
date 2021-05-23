library(dplyr)
library(data.table)
library(tidyr)
library(fastDummies)
library(beepr)
library(ggplot2)
setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("dataframe_finalv2.csv", nThread = 8); beep()


df <- mutate(df, PBD_inf730 = ifelse(kpjdxp < 730,1,0) , PBD_sup730 = ifelse(kpjdxp >= 730,1,0),
             SJR_infmean= ifelse(kqcsjp < mean(df$kqcsjp),1,0),SJR_supmean =ifelse(kqcsjp >= mean(df$kqcsjp),1,0),
             anciennete_inf3 = ifelse(anciennete <= 92, 1, 0) , anciennete_4_6 = ifelse(anciennete %in% 122:183, 1,0 )) 

########################################
######## MAIN SAMPLES BY DATE ########


df_684 <- filter(df, date == 684)
df_683 <- filter(df, date == 683)

nrow(df_684) == nrow(df_683)

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

M_D_684 <- filter(df_684, Duration == 1 | Money == 1)

#Date 683 
T_C_683 <- filter(df_683, treated == 1 | controle == 1)

T_C_SC_683 <- df_683

T_SC_683 <- filter(df_683, treated == 1 | supercontrole == 1)

N_F_683 <- filter(df_683, Neutral == 1 | Framed == 1)

N_D_683 <- filter(df_683, Neutral == 1 | Duration == 1)

N_M_683 <- filter(df_683, Neutral == 1 | Money == 1)

M_D_683 <- filter(df_684, Duration == 1 | Money == 1)

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

means_Neutral <- Means(x = Neutral_684, vars684) %>% append(Means(x = Neutral_683, vars683)) 
means_Framed <- Means(x = Framed_684, vars684) %>% append(Means(x = Framed_683, vars683)) 
means_treated <- Means(x = treated_684, vars684) %>% append(Means(x = treated_683, vars683)) 
means_control <- Means(x = controle_684, vars684) %>%  append(Means(x = controle_683, vars683)) 
means_All <- Means(x = df_684, vars684) %>% append(Means(x = df_683, vars683)) 
means_sup_control <- Means(x = sup_controle_684, vars684) %>%  append(Means(x = sup_controle_683, vars683)) 
means_Money <- Means(x = Money_684, vars684) %>% append(Means(x = Money_683, vars683)) 
means_Duration <- Means(x = Duration_684, vars684) %>% append(Means(x = Duration_683, vars683)) 

# !!!! Last part of the table is computed using DECEMBER 2016 a.k.a 683
table_means <- cbind(means_Money, means_Duration, means_Neutral,means_Framed  , means_All,means_treated, means_control, means_sup_control ) %>% as.data.table()
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

cl_T_C <- cl.test(vars684, "treated", T_C_684) %>%  append( cl.test(vars683, "treated", T_C_683)) %>% unlist

cl_N_F <- cl.test(vars684, "Framed", N_F_684) %>%  append( cl.test(vars683, "Framed", N_F_683)) %>% unlist

cl_T_CSC <- cl.test(vars684, "treated", T_C_SC_684) %>%  append( cl.test(vars683, "treated", T_C_SC_683)) %>% unlist

cl_T_SC <- cl.test(vars684, "treated", T_SC_684) %>%  append( cl.test(vars683, "treated", T_SC_683)) %>% unlist

cl_N_M <- cl.test(vars684, "Money", N_M_684) %>%  append( cl.test(vars683, "Money", N_M_683)) %>% unlist

cl_N_D <- cl.test(vars684, "Duration", N_D_684) %>%  append( cl.test(vars683, "Duration", N_D_683)) %>% unlist

cl_M_D <- cl.test(vars684, "Duration", M_D_684) %>%  append( cl.test(vars683, "Duration", M_D_683)) %>% unlist

table_cl <- cbind(cl_M_D, cl_N_M, cl_N_D, cl_T_C,cl_N_F, cl_T_CSC, cl_T_SC) %>%  as.data.table()
table_cl 
which(table_cl < 0.1)

#########################

TAB <- cbind(table_means, table_cl) %>%  mutate_all(unlist) %>% mutate_all( ~round(.,7)) %>% as.data.table; TAB


row.names(TAB) <- c(vars684, vars683); TAB

nrow(Money_684); nrow(Duration_684);nrow(Neutral_684); nrow(Framed_684); nrow(df_684); nrow(treated_684); nrow(controle_684); nrow(sup_controle_684)


library(xtable)
xtable(TAB, digits = 2, )


############   HISTO PBD ##############

df <- df[, experimental_group := as.factor(ifelse(supercontrole == 1, "Supercontrol", ifelse(controle == 1,"Control","Treatment")))]
df684 = df[date == 684]
ggplot(data = df684, aes(x = PBD )) + geom_histogram(aes(y = ..prop.., fill = experimental_group), alpha = 0.2)) 


ggplot(data = df684, aes(x = PBD, color = as.factor(experimental_group)), alpha = 1) + geom_density(alpha = 0.2, size =1)+
  labs(title = "Distribution of PBD per Group", color = "Group") + theme(plot.title = element_text(hjust = 0.5))

