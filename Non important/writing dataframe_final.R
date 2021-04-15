library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/dataframe_final.csv"); beep()
data <- data %>% mutate(Neutral = ifelse(objet1 == 1, 1,0), Framed = ifelse(objet1 == 2 | objet1 == 3,1,0))
data <- data %>% mutate(PBD = kpjdxp)
data <- data %>% mutate(SJR = kqcsjp)
data <- data %>% mutate(abs_left = PBD - anciennete, rel_left = (PBD - anciennete)/PBD, rel_anciennete = anciennete / PBD)

fwrite(data, "dataframe_final.csv")

