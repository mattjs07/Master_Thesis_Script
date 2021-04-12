setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

source("C:/Users/matti/Desktop/Thesis/Data/R/R script/LM_computer.R")

data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep()
data <- data %>% mutate(PBD = kpjdxp)
data <- data %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
data <- data %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
data <- data %>%  filter(!is.na(ouverture1))
data <-  data %>%  filter( erreur1 == 0)



df687 <- data %>%  filter( date == 687)

LM687_h <- LM_computer(dependant = "heures_iar_cum", df = df687)

library(stargazer)
stargazer(LM687_h$lm_MN, LM687_h$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM687_h$region)
stargazer(LM687_h$lm_dif2, LM687_h$lm_MD2, LM687_h$lm_MN2, LM687_h$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM687_h$region)
stargazer(LM687_h$lm_MD3, LM687_h$lm_MN3, LM687_h$lm_DN3, type ="text", omit =LM687_h$region )
stargazer(LM687_h$lm_MD4, LM687_h$lm_MN4, LM687_h$lm_DN4, type ="text", omit = LM687_h$region )

stargazer(LM687_h$lm_df, LM687_h$lm_dif, omit = LM687_h$region , type = "text")



df696 <- data %>%  filter( date == 696)

LM696_h <- LM_computer(dependant = "heures_iar_cum", df = df696)

stargazer(LM696_h$lm_MN, LM696_h$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM696_h$region)
stargazer(LM696_h$lm_dif2, LM696_h$lm_MD2, LM696_h$lm_MN2, LM696_h$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM696_h$region)
stargazer(LM696_h$lm_MD3, LM696_h$lm_MN3, LM696_h$lm_DN3, type ="text", omit = LM696_h$region )
stargazer(LM696_h$lm_MD4, LM696_h$lm_MN4, LM696_h$lm_DN4, type ="text", omit = LM696_h$region )



df720 <- data %>%  filter( date == 720)

LM720_h <- LM_computer(dependant = "heures_iar_cum", df = df720)

stargazer(LM720_h$lm_MN, LM720_h$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM720_h$region)
stargazer(LM720_h$lm_dif2, LM720_h$lm_MD2, LM720_h$lm_MN2, LM720_h$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM720_h$region)
stargazer(LM720_h$lm_MD3, LM720_h$lm_MN3, LM720_h$lm_DN3, type ="text", omit = LM720_h$region )
stargazer(LM720_h$lm_MD4, LM720_h$lm_MN4, LM720_h$lm_DN4, type ="text", omit = LM720_h$region )














