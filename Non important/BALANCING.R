library(dplyr)
library(data.table)
library(tidyr)
library(fastDummies)
library(beepr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")


df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/finale_merge_filtered_csv.csv "); beep()

#Can direclty load the built df : 

df <- fread("df_new_variables_29_03.csv"); beep()

########################################
######## BUILDING VARIABLES ######## #########

df <- mutate(df, PBD_inf730 = ifelse(kpjdxp < 730,1,0) , PBD_sup730 = ifelse(kpjdxp >= 730,1,0),
             SJR_infmean= ifelse(kqcsjp < mean(df$kqcsjp),1,0),SJR_supmean =ifelse(kqcsjp >= mean(df$kqcsjp),1,0),
             anciennete_inf3 = ifelse(anciennete <= 92, 1, 0) , anciennete_4_6 = ifelse(anciennete %in% 122:183, 1,0 ))  #On PBD

#Create a dummy for Neutral vs Framed 

df <- dummy_cols(df, select_columns = c("objet1","objet2" ,"objet3"), ignore_na = TRUE) 
## Good to know, can add the argument "remove_first_dummy", or "remove_most_frequent_dummy" 
## ignore_na = TRUE --> NAs, not an additional column for NAs
### There are NA in object2 :  921 NAs
# In any case if an obs is NA --> its dummy will be NA !  
#table(df_684$objet1_1, exclude = NULL)   #uncomment

df <- mutate(df, sum1 = objet1_1 + objet2_1 + objet3_1,    #Sums of objects category by indiv
                       sum2= objet1_2 + objet2_2 + objet3_2,
                       sum3 = objet1_3 + objet2_3 + objet3_3)


df <- df %>% mutate(Neutral =  ifelse(sum1 == 3, 1, 0) )
df <- df %>% mutate(Framed =  ifelse(sum1 == 0, 1, 0) )
df <- df %>% mutate( Money = ifelse(objet1 == 3, 1, 0), Duration = ifelse(objet1 == 2, 1, 0)) 
#!!!! Remember the variable error !!!! When they sent to wrong adress

df <- mutate(df, full_received = ifelse(erreur1 == 0 & erreur2 ==0 & erreur3 == 0, 1,0),
             partially_received = ifelse(erreur1 == 0 | erreur2 ==0| erreur3 == 0, 1,0),
             main_duration = ifelse(sum2 == 2, 1,0), main_money = ifelse(sum3 == 2, 1,0) )
#Several options there :: filter anyone who did not receive the mail at all. Or did not received it once



#Can create some more subsamples, usefull for latter: 




########################################


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

obs_nomail_684 <- filter( df_684, treated == 1 , is.na(ouverture1))

treated_684 <- filter( df_684, treated == 1)

controle_684 <- filter( df_684, controle == 1)

sup_controle_684 <- filter( df_684, supercontrole == 1)

full_received_684 <- df_684 %>% filter(erreur1 == 0, erreur2 ==0, erreur3 == 0)
nrow(full_received_684) # filters out 2510indiv --> down to 39093 indiv

partially_received_684 <- df_684 %>% filter(erreur1 == 0 | erreur2 ==0| erreur3 == 0) # --> this filters out 1466 indiv, down to 40137indiv

Neutral_684 <- filter(df_684, Neutral == 1)

Framed_684 <- filter(df_684, Framed == 1)

Money_684 <- filter(df_684, Money == 1)

Duration_684 <- filter(df_684, Duration == 1)

nrow(obs_mail_684); nrow(obs_nomail_684); nrow(controle_684) #Base sample will thereby be 41603 individuals

#683

obs_mail_683 <- filter( df_683, !is.na(ouverture1))

obs_nomail_683 <- filter( df_683, treated == 1 , is.na(ouverture1))

treated_683 <- filter( df_683, treated == 1)

controle_683 <- filter( df_683, controle == 1)

sup_controle_683 <- filter( df_683, supercontrole == 1)

full_received_683 <- df_683 %>% filter(erreur1 == 0, erreur2 ==0, erreur3 == 0)

partially_received_683 <- df_683 %>% filter(erreur1 == 0 | erreur2 ==0| erreur3 == 0) # --> this filters out 1466 indiv, down to 40137indiv

Neutral_683 <- filter(df_683, Neutral == 1)

Framed_683 <- filter(df_683, Framed == 1)


Money_683 <- filter(df_683, Money == 1)

Duration_683 <- filter(df_683, Duration == 1)


nrow(df_684); nrow(treated_684); nrow(controle_684); nrow(sup_controle_684)
########################################


###############################
####### FIRST LOOK ############


#Sexe
mean(Neutral$femme); mean(Framed$femme)
#Age
mean(Neutral$age); mean(Framed$age)
mean(Neutral$jeune); mean(Framed$jeune)
mean(Neutral$age_intermediaire); mean(Framed$age_intermediaire)
mean(Neutral$senior); mean(Framed$senior)
#Educ
mean(Neutral$lower_2nd_edu, na.rm = TRUE); mean(Framed$lower_2nd_edu, na.rm = TRUE)
mean(Neutral$upper_2nd_edu, na.rm = TRUE); mean(Framed$upper_2nd_edu, na.rm = TRUE)
mean(Neutral$higher_edu, na.rm = TRUE); mean(Framed$higher_edu, na.rm = TRUE)
#Contract Duration
mean(Neutral$contrat_moins_12mois); mean(Framed$contrat_moins_12mois)
mean(Neutral$contrat_moins_3mois); mean(Framed$contrat_moins_3mois)
#Potential Benefit duration
mean(Neutral$kpjdxp); mean(Framed$kpjdxp)
mean(Neutral$PBD_inf730); mean(Framed$PBD_inf730)
mean(Neutral$PBD_sup730); mean(Framed$PBD_sup730)
#Daily reference wage: 
mean(Neutral$kqcsjp); mean(Framed$kqcsjp)
mean(Neutral$SJR_infmean ); mean(Framed$SJR_infmean )
mean(Neutral$SJR_supmean); mean(Framed$SJR_supmean)
#Days since entry in unemployment 
mean(Neutral$anciennete); mean(Framed$anciennete)
mean(Neutral$anciennete_inf3); mean(Framed$anciennete_inf3)
mean(Neutral$anciennete_4_6); mean(Framed$anciennete_4_6)
#Unemployment rate 
mean(Neutral$tx_chge); mean(Framed$tx_chge, na.rm = TRUE)
mean(Neutral$tx_chge_jeunes, na.rm = TRUE); mean(Framed$tx_chge_jeunes, na.rm = TRUE)
#Share part time U
mean(Neutral$proportion_de_ar); mean(Framed$proportion_de_ar)
# Share of long-term unemp
mean(Neutral$proportion_de_ld); mean(Framed$proportion_de_ld)
# Exit rate from unemp
mean(Neutral$proportion_de_sortants); mean(Framed$proportion_de_sortants)
# Number of claimants 
mean(Neutral$nombre_de); mean(Framed$nombre_de)
#Number of participants
mean(Neutral$nombre_de_rct); mean(Framed$nombre_de_rct)

################################


######################################
##### Perform T statistics test ##### 


#This is inside the treated group, also need All, All-treated, Control, SUper-Control.
# T = S = SC   ---> multiple hypothesis testing. 


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

#### #### #### #### #### 
#### clustered SE ####
#### #### #### #### 


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

cl_N_M <- cl.test(vars684, "treated", N_M_684) %>%  append( cl.test(vars683, "treated", N_M_683))

cl_N_D <- cl.test(vars684, "treated", N_D_684) %>%  append( cl.test(vars683, "treated", N_D_683))

table_cl <- cbind(cl_N_M, cl_N_D, cl_T_C,cl_N_F, cl_T_CSC, cl_T_SC) %>%  as.data.frame()
table_cl 

#only significant is for "proportion_de_ar", "proportion_de_ld" in N vs F

    #########################
TAB <- cbind(table_means, table_cl) %>% mutate_all(unlist) %>% mutate_all( ~round(.,4)); TAB

row.names(TAB) <- c(vars684, vars683); TAB


library(xtable)
xtable(TAB, digits = 2)

map(list(Money_684,Duration_684,Neutral_684, Framed_684, df_684, treated_684, controle_684, sup_controle_684), nrow)
