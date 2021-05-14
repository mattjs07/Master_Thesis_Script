

dd = data[date == 684]
dd= dd[Money ==1 | Duration ==1]

library(gtools)
dd$group = as.integer(quantcut(dd$av_spell_12m, q= 3))


vars2 <- c( "Duration","femme", "age", "upper_2nd_edu", "higher_edu", "contrat_moins_12mois", "contrat_moins_3mois",
           "anciennete", "indemnisation", "PBD", "SJR",  "married","foreigner", "tx_chge", "tx_chge_jeunes",
           "proportion_de_ar", "proportion_de_ld", "proportion_de_sortants", "nombre_de", "nombre_de_rct")
vars2 <- paste(vars2, collapse = "+")

n = length(dd)
dd<- dummy_columns(dd, select_columns = "region", remove_first_dummy = TRUE)
region <- names(dd[, (n+1):length(dd)])
FE_region <- region %>% paste(collapse = "+")



for( i in 1:3){
  g <- lm(data = dd[group == i], paste( "ouverture1", "~", vars2,"+",FE_region, collapse = ""))
  stargazer(g, type = "text", keep = "Duration")
  
}
 
lm(data = dd[Framed == 1], paste( "ouverture1", "~","av_spell_24m +","av_spell_24m*Duration +", vars2, "+",FE_region, collapse = "")) %>% stargazer(type = "text", keep = c("av_spell_24m","av_spell_24m*Duration "))



