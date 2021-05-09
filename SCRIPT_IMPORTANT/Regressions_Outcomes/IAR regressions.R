library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

source("C:/Users/matti/Desktop/Thesis/Data/R/R_script/SCRIPT_IMPORTANT/Regressions_Outcomes/LM_computerv5.R")



setDTthreads(threads = 8)
data <- fread("dataframe_finalv2.csv", nThread = 8); beep()
df <- data 
df <- df %>% mutate(anciennete_high = ifelse(anciennete > mean(anciennete),1,0))
df <- df %>%  filter(!is.na(ouverture1))
df <-  df %>%  filter( erreur1 == 0)



df687 <- df %>%  filter( date == 687)

LM687 <- LM_computer(dependant = "iar_cum", df = df687)


stargazer(LM687$lm_df, LM687$lm_N,LM687$lm_F,LM687$lm_dif,LM687$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM687$region)
stargazer(LM687$lm_dif2, LM687$lm_B2,LM687$lm_MD1, LM687$lm_MD2, type = "text", omit = LM687$region, column.labels = rep(c("All", "D+M"), each = 2))


 df696 <- df %>%  filter( date == 696)

LM696 <- LM_computer(dependant = "iar_cum", df = df696)

stargazer(LM696$lm_df, LM696$lm_N,LM696$lm_F,LM696$lm_dif,LM696$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM696$region)
stargazer(LM696$lm_dif2, LM696$lm_B2,LM696$lm_B3,LM696$lm_B4, type = "text", omit = LM696$region, column.labels = rep("All",4))
stargazer(LM696$lm_MD1, LM696$lm_MD2, LM696$lm_MD3,LM696$lm_MD4, type ="text", omit =LM696$region, column.labels = rep("D + M",4))



df720 <- df %>%  filter( date == 720)

LM720 <- LM_computer(dependant = "iar_cum", df = df720)

stargazer(LM720$lm_df, LM720$lm_N,LM720$lm_F,LM720$lm_dif,LM720$lm_B1, type = "text", column.labels = c("All", "Neutral", "Framed", "All", "All"), omit = LM720$region)
stargazer(LM720$lm_dif2, LM720$lm_B2,LM720$lm_B3,LM720$lm_B4, type = "text", omit = LM720$region, column.labels = rep("All",4))
stargazer(LM720$lm_MD1, LM720$lm_MD2, LM720$lm_MD3,LM720$lm_MD4, type ="text", omit =LM720$region, column.labels = rep("D + M",4))













