setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

source("C:/Users/matti/Desktop/Thesis/Data/R/R script/LM_computer.R")

data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep()
data <- data %>% mutate(PBD = kpjdxp)
data <- data %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
data <- data %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
data <- data %>%  filter(!is.na(ouverture1))
data <-  data %>%  filter( erreur1 == 0)



df687 <- data %>%  filter( date == 687)

LM687_r <- LM_computer(dependant = "revenu_iar_cum", df = df687)

library(stargazer)
stargazer(LM687_r$lm_MN, LM687_r$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM687_r$region)
stargazer(LM687_r$lm_dif2, LM687_r$lm_MD2, LM687_r$lm_MN2, LM687_r$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM687_r$region)
stargazer(LM687_r$lm_MD3, LM687_r$lm_MN3, LM687_r$lm_DN3, type ="text", omit =LM687_r$region )
stargazer(LM687_r$lm_MD4, LM687_r$lm_MN4, LM687_r$lm_DN4, type ="text", omit = LM687_r$region )

stargazer(LM687_r$lm_df, LM687_r$lm_dif, omit = LM687_r$region , type = "text")



df696 <- data %>%  filter( date == 696)

LM696_r <- LM_computer(dependant = "revenu_iar_cum", df = df696)

stargazer(LM696_r$lm_MN, LM696_r$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM696_r$region)
stargazer(LM696_r$lm_dif2, LM696_r$lm_MD2, LM696_r$lm_MN2, LM696_r$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM696_r$region)
stargazer(LM696_r$lm_MD3, LM696_r$lm_MN3, LM696_r$lm_DN3, type ="text", omit = LM696_r$region )
stargazer(LM696_r$lm_MD4, LM696_r$lm_MN4, LM696_r$lm_DN4, type ="text", omit = LM696_r$region )



df720 <- data %>%  filter( date == 720)

LM720_r <- LM_computer(dependant = "revenu_iar_cum", df = df720)

stargazer(LM720_r$lm_MN, LM720_r$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM720_r$region)
stargazer(LM720_r$lm_dif2, LM720_r$lm_MD2, LM720_r$lm_MN2, LM720_r$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM720_r$region)
stargazer(LM720_r$lm_MD3, LM720_r$lm_MN3, LM720_r$lm_DN3, type ="text", omit = LM720_r$region )
stargazer(LM720_r$lm_MD4, LM720_r$lm_MN4, LM720_r$lm_DN4, type ="text", omit = LM720_r$region )














