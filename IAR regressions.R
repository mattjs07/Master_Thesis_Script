setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

source("C:/Users/matti/Desktop/Thesis/Data/R/R script/LM_computer.R")
source("C:/Users/matti/Desktop/Thesis/Data/R/R script/GLM_computer.R")



data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/df_new_variables_23_03.csv "); beep()
data <- data %>% mutate(PBD = kpjdxp)
data <- data %>% mutate(SJR = kqcsjp, anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
data <- data %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)
data <- data %>%  filter(!is.na(ouverture1))
data <-  data %>%  filter( erreur1 == 0)



df687 <- data %>%  filter( date == 687)

LM687 <- LM_computer(dependant = "iar_cum")

library(stargazer)
stargazer(LM687$lm_MN, LM687$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM687$region)
stargazer(LM687$lm_dif2, LM687$lm_MD2, LM687$lm_MN2, LM687$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM687$region)
stargazer(LM687$lm_MD3, LM687$lm_MN3, LM687$lm_DN3, type ="text", omit =LM687$region )
stargazer(LM687$lm_MD4, LM687$lm_MN4, LM687$lm_DN4, type ="text", omit = LM687$region )





df696 <- data %>%  filter( date == 696)

LM696 <- LM_computer(dependant = "iar_cum", df = df696)

stargazer(LM696$lm_MN, LM696$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM696$region)
stargazer(LM696$lm_dif2, LM696$lm_MD2, LM696$lm_MN2, LM696$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM696$region)
stargazer(LM696$lm_MD3, LM696$lm_MN3, LM696$lm_DN3, type ="text", omit = LM696$region )
stargazer(LM696$lm_MD4, LM696$lm_MN4, LM696$lm_DN4, type ="text", omit = LM696$region )



df720 <- data %>%  filter( date == 720)

LM720 <- LM_computer(dependant = "iar_cum", df = df720)

stargazer(LM720$lm_MN, LM720$lm_DN, type = "text", column.labels = c("Money", "Duration"), omit = LM720$region)
stargazer(LM720$lm_dif2, LM720$lm_MD2, LM720$lm_MN2, LM720$lm_DN2, type = "text", column.labels = c("N vs F", "M vs D", "M vs N", "D vs N"), header = TRUE, omit = LM720$region)
stargazer(LM720$lm_MD3, LM720$lm_MN3, LM720$lm_DN3, type ="text", omit = LM720$region )
stargazer(LM720$lm_MD4, LM720$lm_MN4, LM720$lm_DN4, type ="text", omit = LM720$region )














