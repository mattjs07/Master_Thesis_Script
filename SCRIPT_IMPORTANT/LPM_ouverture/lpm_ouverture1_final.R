library(pkgloadr)
library(latex2exp)
library(gtools)
library(sjmisc)
setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_finalv2.csv", nThread = 8)


df <- data

df <- df %>%  filter( date == 684)#first mail sent the 31 january --> 684
df <- df %>%  filter(!is.na(ouverture1))
df <-  df %>%  filter( erreur1 == 0) # filter out the wrong mails sent
df[, age2 := age^2]

###INTRO####
g1 <- df %>% group_by(objet1) %>% summarise(ouverture = mean(ouverture1))

ggplot(g1, aes(x= as.factor(objet1), y = ouverture)) +geom_col(aes(fill = as.factor(objet1))) + coord_cartesian(ylim = c(0.7,0.82)) + 
 labs(x = "Group", y = "Opening rate") + theme( legend.position = 'none') +
  theme( plot.subtitle = element_text(hjust = 0.5), legend.position = 'none') + scale_x_discrete(labels = c("Neutral","Duration","Income"))

g1 <- select(g1, -objet1)

g2 <- data %>% filter( erreur2 == 0) %>% group_by(objet2) %>% summarise(ouverture = mean(ouverture2))
g2 <- select(g2, -objet2)

g3 <- data %>% filter( erreur3 == 0) %>% group_by(objet3) %>% summarise(ouverture = mean(ouverture3))
g3 <- select(g3, -objet3)

G <- data.frame(objet = as.factor(rep(1:3, 3)), rbind(g1,g2,g3), time = rep(1:3, each = 3))

ggplot(data = G, aes(x = time, y = ouverture, color = objet, )) +geom_line(size = 1) + 
  scale_x_discrete(name = "Sending", limits = c("1","2","3")) + labs(title = "Evolution of opening rate", subtitle = "by group and sending")+
  ylab("Opening rate") + theme(plot.title = element_text(hjust =0.5), plot.subtitle = element_text(hjust =0.5))

ggplot(data = G, aes(x = objet, y = ouverture, fill = objet)) +geom_col() + facet_wrap(~time, labeller = labeller( .cols = c("1" = "1st sending","2" = "2nd sending", "3" ="3rd sending"))) +
  ylab("Opening rate") + xlab("") + labs(title = "Opening rates by sending by group") + theme(plot.title = element_text(hjust = 0.5)) 

############

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT_IMPORTANT/LPM_ouverture/LPM_comp_final.R")

L1 <- LPM_computer( dependant = "ouverture1", df =df)


stargazer(L1$lpm_M$reg, L1$lpm_D$reg, L1$lpm_MD1$reg, L1$lpm_MD2$reg, type = "text", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c("episode_rac_numero_mois","episode_rac_numero_mois:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X"),
                           c( "Obs", L1$lpm_M$n, L1$lpm_D$n,L1$lpm_MD1$n, L1$lpm_MD2$n) ), report= 'vc*sp')

L2 <- LPM_computer( dependant = "ouverture1", interest= "tx_chge", df =df, rm_var = c("tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct"))


stargazer(L2$lpm_M$reg, L2$lpm_D$reg, L2$lpm_MD1$reg, L2$lpm_MD2$reg, type = "text", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c("tx_chge","tx_chge:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X"),
                            c( "Obs", L2$lpm_M$n, L2$lpm_D$n,L2$lpm_MD1$n, L2$lpm_MD2$n) ), report= 'vc*sp')

df_framed = df[Framed == 1 ]
df_framed[, quant_tx_chge := as.integer(quantcut(tx_chge,3))]

L3 <- LPM_computer( dependant = "ouverture1", df =df_framed[quant_tx_chge == 1])


stargazer(L3$lpm_M$reg, L3$lpm_D$reg, L3$lpm_MD1$reg, L3$lpm_MD2$reg, type = "text", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c( "Duration","episode_rac_numero_mois","episode_rac_numero_mois:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X"),
                            c( "Obs", L3$lpm_M$n, L3$lpm_D$n,L3$lpm_MD1$n, L3$lpm_MD2$n) ), report= 'vc*sp')

L4 <- LPM_computer( dependant = "ouverture1", df =df_framed[quant_tx_chge == 3])


stargazer(L4$lpm_M$reg, L4$lpm_D$reg, L4$lpm_MD1$reg, L4$lpm_MD2$reg, type = "text", column.labels = c("Money", "Duration", "Both","Both"), 
          keep = c("Duration","episode_rac_numero_mois","episode_rac_numero_mois:Duration"),
          add.lines = list( c("Controls","X","X","X","X"), c("Fully Interacted", "","","","X"),
                            c( "Obs", L4$lpm_M$n, L4$lpm_D$n,L4$lpm_MD1$n, L4$lpm_MD2$n) ), report= 'vc*sp')

######## Stratification ###### 

vars2 <- c("femme", "age","age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "episode_rac_numero_mois", "indemnisation", "PBD",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", 
           "married", "primaire","secondaire", "cdi", "lic", "factor(region)")

vars2 <- paste(vars2, collapse = "+")

df_framed = df[Framed == 1 ]

lpm.obs <- function(dependant,variables, data){
  if(!is.character(dependant)) stop("dependant variable must be of type character")
  if(!is.data.frame(data)) stop("data is not of type data frame")
  if( str_contains(variables, "+") == FALSE) stop("variables must be formatted as a character vector such as 'x1 + x2 + x3' ")
  g <- lm(data = data, paste( dependant, "~", variables, collapse = ""))
  n <- nobs(g)
  g <- g %>%  coeftest( vcov. = cluster.vcov( g, cluster = data$kcala, stata_fe_model_rank = TRUE))
  return(list(reg =g,n =n)) }


#Anciennete
df_framed$quant_anciennete = as.integer(quantcut(df_framed$episode_rac_numero_mois, 3))

for( i in c(1,3)){
  g <- lpm.obs(data = df_framed[quant_anciennete == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "text", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}


# PBD
df_framed$quant_PBD = as.integer(quantcut(df_framed$PBD, 3))

for( i in c(1,3)){
  g <- lpm.obs(data = df_framed[quant_PBD == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "text", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#Time_Left
df_framed[, T_left := PBD - episode_rac_numero_mois*30.4]
df_framed[, quant_T_left := as.integer(quantcut(T_left,3))]

for( i in c(1,3)){
  g <- lpm.obs(data = df_framed[quant_T_left == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "text", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#relative time left
df_framed[, T_left_rel := (PBD - episode_rac_numero_mois*30.4)/PBD]
df_framed[, quant_T_left_rel := as.integer(quantcut(T_left_rel,3))]

for( i in c(1,3)){
  g <- lpm.obs(data = df_framed[quant_T_left_rel == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "text", keep = "Duration", add.lines = list(c("Obs", g$n)))
  
}

#taux de chomage
df_framed[, quant_tx_chge := as.integer(quantcut(tx_chge,3))]

for( i in c(1,3)){
  g <- lpm.obs(data = df_framed[quant_tx_chge == i], dependant = "ouverture1", variables = paste("Duration +",vars2, collapse = "" ))
  stargazer(g$reg, type = "text", keep = c("Duration", "episode_rac_numero_mois","Duration*episode_rac_numero_mois"), add.lines = list(c("Obs", g$n)))
  
}

########### Sub groups ###################

sub_Money <- df[Money == 1] 
sub_Money[, old_anc := ifelse(episode_rac_numero_mois > mean(episode_rac_numero_mois, na.rm = TRUE), 1, 0)]

vars3 <- c("femme", "age","age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "old_anc", "indemnisation", "PBD",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", 
           "married", "primaire","secondaire", "cdi", "lic")

vars3 <- paste(vars3, collapse = "+")


  g <- lpm.obs(data = sub_Money, dependant = "ouverture1", variables = vars3)
  stargazer(g$reg, type = "latex", keep = "old_anc", add.lines = list(c("Obs", g$n)), title = "Subset Money")
  

sub_Duration <- df[Duration == 1] 
sub_Duration[, old_anc := ifelse(episode_rac_numero_mois > mean(episode_rac_numero_mois, na.rm = TRUE), 1, 0)]


g <- lpm.obs(data = sub_Duration, dependant = "ouverture1", variables = vars3)
stargazer(g$reg, type = "latex", keep = "old_anc", add.lines = list(c("Obs", g$n)), title = "Subset Duration")


