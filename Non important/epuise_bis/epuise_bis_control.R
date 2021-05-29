library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("data_frame_abadie.csv", nThread = 8)
data <- data[date == 684]
data[, age2 :=  age^2]
data= data[supercontrole ==1 & PBD >= 730]


vars <- c("episode_rac_numero_mois" ,"femme", "age", "age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes", "proportion_de_ar", "proportion_de_ld", 
          "proportion_de_sortants", "nombre_de", "nombre_de_rct", "episode_rac_numero", "divorced", "primaire","secondaire", "cdi", "lic", "factor(region)")
vars <- paste(vars, collapse = "+")


reg1 <- lm(data = subset(data, supercontrole ==1), paste( "epuise_bis", "~", vars, collapse = ""))
reg_1 = reg1
stat1 <- summary(reg1)[c("r.squared","adj.r.squared")]
reg1 <- reg1 %>%  coeftest( vcov. = cluster.vcov( reg1, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))

linearHypothesis(reg_1, c("tx_chge_jeunes = 0", "proportion_de_ar = 0", "proportion_de_ld = 0", "proportion_de_sortants = 0", "nombre_de = 0",
                          "nombre_de_rct = 0"), vcov. = cluster.vcov( reg_1, cluster = subset(data, supercontrole ==1)$kcala, 
                                                                      stata_fe_model_rank = TRUE) )
# Are jointly insignificant -->  DROPING THEM 

vars2 <- c("episode_rac_numero_mois" ,"femme", "age", "age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "episode_rac_numero", "divorced", 
          "primaire","secondaire", "cdi", "lic", "factor(region)")
vars2 <- paste(vars2, collapse = "+")


reg2 <- lm(data = subset(data, supercontrole ==1), paste( "epuise_bis", "~", vars2, collapse = ""))
reg_2 = reg2
stat2 <- summary(reg2)[c("r.squared","adj.r.squared")]
reg2 <- reg2 %>%  coeftest( vcov. = cluster.vcov( reg2, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))



STATS <- rbind( stat2) %>% as.data.frame()
rsq <- c("Rsq", paste(STATS$r.squared))
rsq.adj <- c("Rsq.adj", paste(STATS$adj.r.squared))

stargazer(reg2,  type = "text", omit = "region",
          add.lines = list(rsq, rsq.adj), title = "Average unemployment spell length for Supercontrol group")



covariate.labels = c("sex","age","upper 2nd education", "higher education", "last contract <12m", "last contract <3m", 
                     "time since entry current spell","indemnisation","PBD","DRW", "married", "foreigner")
