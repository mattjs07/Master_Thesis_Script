
vars <- c("episode_rac_numero_mois" ,"femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes", "proportion_de_ar", "proportion_de_ld", 
          "proportion_de_sortants", "nombre_de", "nombre_de_rct", "episode_rac_numero", "divorced", "primaire","secondaire", "cdi", "lic", "factor(region)")
vars <- paste(vars, collapse = "+")


reg1 <- lm(data = df, paste( "ouverture1", "~", vars, collapse = ""))


linearHypothesis(reg1, c("tx_chge_jeunes = 0", "proportion_de_ar = 0", "proportion_de_ld = 0", "proportion_de_sortants = 0", "nombre_de = 0",
                          "nombre_de_rct = 0"), vcov. = cluster.vcov( reg1, cluster = df$kcala, 
                                                                      stata_fe_model_rank = TRUE) )

standardize <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

df[, macro_var := (standardize(tx_chge) + standardize(tx_chge_jeunes) + standardize(proportion_de_ar) +
     standardize(proportion_de_ld)+ standardize(proportion_de_sortants) +standardize(nombre_de) +standardize(nombre_de_rct))/7]

vars2 <- c("episode_rac_numero_mois" ,"femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
          "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "episode_rac_numero", "divorced", "primaire","secondaire", "cdi", "lic", "factor(region)")
vars2 <- paste(vars2, collapse = "+")


reg2 <- lm(data = df, paste( "ouverture1", "~", vars2, collapse = ""))



stargazer(reg1, reg2, type = "text")
