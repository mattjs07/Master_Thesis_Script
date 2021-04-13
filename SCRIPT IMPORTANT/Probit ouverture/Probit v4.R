library(data.table)
library(beepr)
library(fastDummies)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(dplyr)
library(ggplot2)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_29_03.csv "); beep()

###INTRO####
g1 <- df %>% group_by(objet1) %>% summarise(m = mean(ouverture1))
g1 <- g1[-4,]
g1$objet1 <- as.factor(g1$objet1)
ggplot(g1, aes(x= objet1, y =m, fill =)) +geom_col(aes(fill = objet1)) + coord_cartesian(ylim = c(0.7,0.77)) + 
  labs( title = "Opening rate First sending", x = "Group", y = "Opening rate") + theme( plot.title = element_text(hjust = 0.5), legend.position = 'none') +
  scale_x_discrete(labels = c("Neutral","Duration","Money"))
############


df <- df %>% mutate(PBD = kpjdxp)
df <- df %>% mutate(SJR = kqcsjp)
df <- df %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
df <- df %>%  filter(!is.na(ouverture1))
df <- df %>%  filter( date == 684)#first mail sent the 31 january --> 684
df <-  df %>%  filter( erreur1 == 0) # filter out the wrong mails sent


source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT IMPORTANT/Probit ouverture/GLM_computerv2.R")

G1 <- GLM_computer( dependant = "ouverture1", df =df)
G1
