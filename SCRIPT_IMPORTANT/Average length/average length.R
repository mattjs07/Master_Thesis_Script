library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

data <- fread("data_frame_abadie.csv", nThread = 8)
data <- data[date == 684]
data[, c("age2 ", "episode_rac_numero_mois2" )  :=  .(age^2 , episode_rac_numero_mois^2)]

vars <- c("episode_rac_numero_mois" ,"femme", "age", "age2", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes", "proportion_de_ar", "proportion_de_ld", 
          "proportion_de_sortants", "nombre_de", "nombre_de_rct", "episode_rac_numero", "divorced", "primaire","secondaire", "cdi", "lic", "factor(region)")
vars <- paste(vars, collapse = "+")

reg1 <- lm(data = subset(data, supercontrole ==1), paste( "av_spell_12m", "~", vars, collapse = ""))
stat1 <- summary(reg1)[c("r.squared","adj.r.squared")]
reg1 <- reg1 %>%  coeftest( vcov. = cluster.vcov( reg1, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))

reg2 <- lm(data = subset(data, supercontrole ==1), paste( "av_spell_24m", "~", vars, collapse = ""))
reg_24 = reg2
stat2 <- summary(reg2)[c("r.squared","adj.r.squared")]
reg2 <- reg2 %>%  coeftest( vcov. = cluster.vcov( reg2, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))

reg3 <- lm(data = subset(data, supercontrole ==1), paste( "av_spell_36m", "~", vars, collapse = ""))
stat3 <- summary(reg3)[c("r.squared","adj.r.squared")]
reg3 <- reg3 %>%  coeftest( vcov. = cluster.vcov( reg3, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))

STATS <- rbind(stat1, stat2, stat3) %>% as.data.frame()
rsq <- c("Rsq", paste(STATS$r.squared))
rsq.adj <- c("Rsq.adj", paste(STATS$adj.r.squared))

stargazer(reg1, reg2, reg3, type = "text", column.labels = c("12m", "24m", "36m"), 
          omit = c("Constant", "tx_chge","tx_chge_jeunes","proportion_de_ar",
                   "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct"),
          add.lines = list(rsq, rsq.adj), title = "Average unemployment spell length for Supercontrol group")



covariate.labels = c("sex","age","upper 2nd education", "higher education", "last contract <12m", "last contract <3m", 
                     "time since entry current spell","indemnisation","PBD","DRW", "married", "foreigner")


qqplot(data[supercontrole ==1]$av_spell_24m, fitted(reg_24), col = "gray", pch = 20, main = "QQ-plot fitted against true values: average spell at 24 months", xlab = "true value", ylab = "fitted value")
curve((x), col = "blue", add = TRUE)


#####################

data_treated = data[Framed == 1 & !is.na(ouverture1)]
data_treated$pred_av_spell = predict.lm(reg_24, data_treated)
library(gtools)
data_treated$quant_spell = as.integer(quantcut(data_treated$pred_av_spell, 3))


 g <-  map(1:3, function(i){lm(data = data_treated[quant_spell == i], paste( "ouverture1", "~", "Duration +" ,vars, collapse = ""))})
  stargazer(g, type = "text")
  
}


vars2 <- c("femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
vars2 <- paste(vars2, collapse = "+")
data_treated$quant_anciennete = as.integer(quantcut(data_treated$anciennete, 2))

for( i in 1:2){
  g <- lm(data = data_treated[quant_anciennete == i], paste( "ouverture1", "~", "Duration +",vars2, collapse = ""))
  stargazer(g, type = "text", keep = "Duration")
  
}



######################

n = length(data)
data<- dummy_columns(data, select_columns = "region", remove_first_dummy = TRUE)
region <- names(data[, (n+1):length(data)])
FE_region <- region %>% paste(collapse = "+")

supp_var <- paste(supp_var, collapse = "+")

VARS = paste(c(vars, supp_var, FE_region), collapse = "+")
reg4 <- lm(data = subset(data, supercontrole ==1), paste( "av_spell_12m", "~", VARS, collapse = ""))
stat1 <- summary(reg1)[c("r.squared","adj.r.squared")]
reg1 <- reg1 %>%  coeftest( vcov. = cluster.vcov( reg1, cluster = subset(data, supercontrole ==1)$kcala, stata_fe_model_rank = TRUE))

## No gains in terms of prediction. 
