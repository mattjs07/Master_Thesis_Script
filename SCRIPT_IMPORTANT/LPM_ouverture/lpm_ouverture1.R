library(pkgloadr)
library(latex2exp)
library(gtools)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_finalv2.csv", nThread = 8)


df <- data

df <- df %>%  filter( date == 684)#first mail sent the 31 january --> 684
df <- df %>%  filter(!is.na(ouverture1))
df <-  df %>%  filter( erreur1 == 0) # filter out the wrong mails sent

###INTRO####
g1 <- data %>% filter( erreur1 == 0) %>% group_by(objet1) %>% summarise(ouverture = mean(ouverture1))

ggplot(g1, aes(x= as.factor(objet1), y = ouverture)) +geom_col(aes(fill = as.factor(objet1))) + coord_cartesian(ylim = c(0.7,0.82)) + 
  labs( title = "Opening rate First sending", subtitle = "For those who received the mail", x = "Group", y = "Opening rate") + theme( plot.title = element_text(hjust = 0.5), legend.position = 'none') +
  theme( plot.subtitle = element_text(hjust = 0.5), legend.position = 'none') + scale_x_discrete(labels = c("Neutral","Duration","Money"))

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

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT_IMPORTANT/LPM_ouverture/LPM_computer.R")

L1 <- LPM_computer( dependant = "ouverture1", df =df)


stargazer(L1$lpm_df, L1$lpm_N,L1$lpm_F,L1$lpm_dif,L1$lpm_B1, type = "html", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = L1$region)
stargazer(L1$lpm_dif2, L1$lpm_B2,L1$lpm_MD1, L1$lpm_MD2, type = "html", omit = L1$region, column.labels = rep(c("All","D + M"),each =2))

# mean(df$anciennete)*-0.0003 =  -0.033. At mean anciennete : 3.3% less likely to open Money against Duration
# An increase of one month in anciennete, decreses the probability of opening of -0.009 points. = a 0.1% decrease in the opening rate

df[, age2 :=  age^2 ]

# df <- mutate(df, anciennete_norm = (anciennete - mean(df$anciennete))/ sqrt(var(df$anciennete)) )
L1_1 <- LPM_computer("ouverture1", df, add_var = c( "age2", "married", "primaire","secondaire", "cdi", "lic")) #here can ignore the #3 and #4 regressions

stargazer(L1_1$lpm_df, L1_1$lpm_N,L1_1$lpm_F,L1_1$lpm_dif,L1_1$lpm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = L1_1$region)
stargazer(L1_1$lpm_dif2, L1_1$lpm_B2,L1_1$lpm_MD1, L1_1$lpm_MD2, type = "text", omit = L1_1$region, column.labels = rep(c("All","D + M"),each =2))
# One SD increase in anciennete : -0.012** --> the probability to open decreases of -0.012 points


######## Stratification ###### 

vars2 <- c("femme", "age","age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "episode_rac_numero_mois", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct", 
            "married", "primaire","secondaire", "cdi", "lic")

vars2 <- paste(vars2, collapse = "+")

df_framed = df[Framed == 1 ]
df_framed$quant_anciennete = as.integer(quantcut(df_framed$episode_rac_numero_mois, 3))

for( i in c(1,3)){
  g <- lm(data = df_framed[quant_anciennete == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))
  stargazer(g, type = "text", keep = "Duration")
  
}


df_framed$quant_PBD = as.integer(quantcut(df_framed$PBD, 3))

for( i in c(1,3)){
  g <- lm(data = df_framed[quant_PBD == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))
  stargazer(g, type = "text", keep = "Duration")
  
}

df_framed[, T_left := PBD - episode_rac_numero_mois*30.4]
df_framed[, quant_T_left := as.integer(quantcut(T_left,3))]

for( i in c(1,3)){
  g <- lm(data = df_framed[quant_T_left == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))
  stargazer(g, type = "text", keep = "Duration")
  
}


reg_femme <- lm(data = df_framed[femme == 1], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))
reg_homme <- lm(data = df_framed[femme == 0], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))

stargazer(reg_femme, reg_homme, column.labels = c("women", "men"), type = "text", keep = "Duration") #Whereas women are supposed to be more pessimistic, they open less duration !
  

###### Ouverutre 3 ######


df <- data

df <- df %>%  filter(!is.na(ouverture1))
df <- df %>%  filter( date == 686)#first mail sent the 31 january --> 684
df <-  df %>%  filter( erreur3 == 0) # filter out the wrong mails sent

L3 <- LPM_computer( dependant = "ouverture3", df =df)


stargazer(L3$lpm_df, L3$lpm_N,L3$lpm_F,L3$lpm_dif,L3$lpm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = L3$region)
stargazer(L3$lpm_dif2, L3$lpm_B2,L3$lpm_MD1, L3$lpm_MD2, type = "text", omit = L3$region, column.labels = rep(c("All","D + M"),each =2))
 


