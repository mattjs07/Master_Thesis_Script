library(pkgloadr)
library(randomForest)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("dataframe_finalv2.csv", nThread = 8)

df <- df[date == 684 & !is.na(ouverture1) & erreur1 == 0 ]


df[, ouverture1 := as.factor(ouverture1)]


######### Using randForest package ##################
fit <- randomForest(ouverture1 ~ Neutral+Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
               indemnisation+PBD+SJR+married+foreigner+married+primaire+secondaire+cdi+lic+tx_chge+tx_chge_jeunes+
                 proportion_de_ar+proportion_de_ld+proportion_de_sortants+nombre_de+nombre_de_rct, data = df, na.action = na.omit, ntree=500)
varImpPlot(fit)
imp <- fit$importance
names <- rownames(imp)
imp = as.data.table(imp)
imp[ ,var := names]
remove <- c("tx_chge", "remove","tx_chge_jeunes", "proportion_de_ar", "proportion_de_ld","proportion_de_sortants", "nombre_de", "nombre_de_rct")
imp = imp[!(var %in% remove)]
setorder(imp, MeanDecreaseGini)

ggplot(data=imp, aes(x= MeanDecreaseGini, y = reorder(var, MeanDecreaseGini))) + geom_point()

plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

df_framed = df[Framed == 1]
fit2 <- randomForest(ouverture1 ~ Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
                      indemnisation+PBD+SJR+married+foreigner+married+primaire+secondaire+cdi+lic+tx_chge+tx_chge_jeunes+
                      proportion_de_ar+proportion_de_ld+proportion_de_sortants+nombre_de+nombre_de_rct, data = df_framed, na.action = na.omit, ntree=500)


############# using CARET package #####################
library(caret)

#Seting up parallelization :
library(doParallel)
cl <- detectCores() %>% -1 %>% makeCluster
registerDoParallel(cl)


set.seed(123)
mod <- train(ouverture1 ~ Neutral+Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
               indemnisation+PBD+SJR+married+foreigner+married+primaire+secondaire+cdi+lic+tx_chge+tx_chge_jeunes+
               proportion_de_ar+proportion_de_ld+proportion_de_sortants+nombre_de+nombre_de_rct, data = df, method = "rf", na.action = na.omit)
stopCluster(cl)

print(mod)
print(mod$finalModel)

z = importance(mod$finalModel)
labels = c("Age", "DRW", "Indemnisation", "Youth unemployment rate", "Unemployment rate", "Number of Claimants", "Number of participants", "PBD",
           "Share of part time unemployment","Share of long-term unemployment", "Months since start of spell", "Higher education", "Exit rate from unemp",
           "Intermediate education", "Women", "Foreigner", "Duration", "Married", "Neutral", "Last contract < 12m", "Secondary Sector", "Last contract < 3m",
           "Primary Sector", "Fired", "CDI") %>% rev()

varImpPlot(mod$finalModel, main = "Random Forest variables ranking", labels = labels)
