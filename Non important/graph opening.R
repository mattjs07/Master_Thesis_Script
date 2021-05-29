library(dplyr)
library(data.table)
library(beepr)
library(fastDummies)
library(haven)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- read_dta("mail.dta")

names <- c("Objet", "ouverture")
t1 <- df %>% group_by(objet1) %>% summarise(m = mean(ouverture1))
t2 <- df %>% group_by(objet2) %>% summarise(m = mean(ouverture2))
t3 <- df %>% group_by(objet3) %>% summarise(m = mean(ouverture3))

names(t1) = names
names(t2) = names
names(t3) = names



d = rbind(t1,t2,t3)

d = filter(d, !is.na(Objet)) 
d = mutate(d, t = rep(c(1,2,3), each = 3))

library(ggplot2)

ggplot(data = d, aes(x = t, y = ouverture)) + geom_point(aes(colour = factor(Objet))) + 
  scale_color_discrete(name = "Group", labels = c("Neutral","Duration","Money")) + geom_line(aes(colour = factor(Objet))) + 
  labs(title = "Opening rate per group") +
  theme( plot.title = element_text(hjust = 0.5))


df2 <- filter(df, ouverture1 == 1)

df %>%  group_by(objet1) %>% summarise(OR1 = mean(ouverture1),OR2 = mean(ouverture2, na.rm = TRUE),OR3 = mean(ouverture3))


df2 %>%  group_by(objet1) %>% summarise(OR2 = mean(ouverture2, na.rm = TRUE),OR3 = mean(ouverture3))

df20 <- filter(df, ouverture1 == 0)
df20 %>%  group_by(objet1) %>% summarise(OR2 = mean(ouverture2, na.rm = TRUE),OR3 = mean(ouverture3))


