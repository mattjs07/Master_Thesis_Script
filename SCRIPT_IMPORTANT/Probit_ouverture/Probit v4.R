library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("dataframe_finalv2.csv", nThread = 8)


df <- data

df <- df %>%  filter(!is.na(ouverture1))
df <- df %>%  filter( date == 684)#first mail sent the 31 january --> 684
df <-  df %>%  filter( erreur1 == 0) # filter out the wrong mails sent

###INTRO####
g1 <- df %>% group_by(objet1) %>% summarise(m = mean(ouverture1))
g1 <- g1[-4,]
g1$objet1 <- as.factor(g1$objet1)
ggplot(g1, aes(x= objet1, y =m, fill =)) +geom_col(aes(fill = objet1)) + coord_cartesian(ylim = c(0.7,0.82)) + 
  labs( title = "Opening rate First sending", subtitle = "For those who received the mail", x = "Group", y = "Opening rate") + theme( plot.title = element_text(hjust = 0.5), legend.position = 'none') +
  theme( plot.subtitle = element_text(hjust = 0.5), legend.position = 'none') + scale_x_discrete(labels = c("Neutral","Duration","Money"))
############

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Probit_ouverture/GLM_computerv4.R")

df <- fastDummies::dummy_cols(df, select_columns = "groupe")
G1 <- GLM_computer( dependant = "ouverture1", df =df, add_var = "groupe_NET")
G1

G1 <- GLM_computer( dependant = "ouverture1", df =df)
G1


stargazer(G1$glm_df, G1$glm_N,G1$glm_F,G1$glm_dif,G1$glm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = G1$region)
stargazer(G1$glm_dif2, G1$glm_B2,G1$glm_MD1, G1$glm_MD2, type = "text", omit = G1$region, column.labels = rep(c("All","D + M"),each =2))





df <- mutate(df, anciennete_norm = (anciennete - mean(df$anciennete))/ sqrt(var(df$anciennete)) )
G1_1 <- GLM_computer("ouverture1", df, rm_var = c("anciennete", "rel_left", "rel_anciennete"), add_var = "anciennete_norm") #here can ignore the #3 and #4 regressions


stargazer(G1_1$glm_df, G1_1$glm_N,G1_1$glm_F,G1_1$glm_dif,G1_1$glm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = G1_1$region)
stargazer(G1_1$glm_dif2, G1_1$glm_B2,G1_1$glm_B3,G1_1$glm_B4, type = "text", omit = G1_1$region, column.labels = rep("All",4))
stargazer(G1_1$glm_MD1, G1_1$glm_MD2, G1_1$glm_MD3,G1_1$glm_MD4, type ="text", omit =G1_1$region, column.labels = rep("D + M",4))







df$date_odd <- as.Date(df$date_odd, "%d%b%Y")
t = mutate(df, timeee = as.Date(anciennete, origin = date_odd)) %>% select(timeee)
which(t != "2017-01-01")
 






