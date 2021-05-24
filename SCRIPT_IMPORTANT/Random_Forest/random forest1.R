library(pkgloadr)
library(randomForest)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("dataframe_finalv2.csv", nThread = 8)

df <- df[date == 684 & !is.na(ouverture1) & erreur1 == 0 ]


df[, ouverture1 := as.factor(ouverture1)]


######### Using randForest package ##################
fit_all <- randomForest(ouverture1 ~ Neutral+Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
               episode_rac_numero+indemnisation+PBD+married+foreigner+primaire+secondaire+cdi+lic+tx_chge, data = df[Framed == 1], na.action = na.omit, ntree=500, mtry = 2)

"tx_chge_jeunes"


labels = c("Indemnisation", "Age", "Unemployment rate", "PBD", "Higher education", "Seniority", "Duration", "Female","Foreigner", "Upper secondary education",
           "Primary Sector", "Married",  "Last contract < 12m", "Secondary Sector", "Last contract < 3m" ,"Number of current spell", "CDI", "Fired","Neutral") %>% rev()



varImpPlot(fit_all, main = "", labels = labels)
p# imp <- fit$importance
# names <- rownames(imp)
# imp = as.data.table(imp)
# imp[ ,var := names]
# remove <- c("tx_chge", "remove","tx_chge_jeunes", "proportion_de_ar", "proportion_de_ld","proportion_de_sortants", "nombre_de", "nombre_de_rct")
# imp = imp[!(var %in% remove)]
# setorder(imp, MeanDecreaseGini)
# 
# ggplot(data=imp, aes(x= MeanDecreaseGini, y = reorder(var, MeanDecreaseGini))) + geom_point()
# 
# plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
# 
# df_framed = df[Framed == 1]
# fit2 <- randomForest(ouverture1 ~ Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
#                       indemnisation+PBD+SJR+married+foreigner+married+primaire+secondaire+cdi+lic+tx_chge+tx_chge_jeunes+
#                       proportion_de_ar+proportion_de_ld+proportion_de_sortants+nombre_de+nombre_de_rct, data = df_framed, na.action = na.omit, ntree=500)

fit_Duration <- randomForest(ouverture1 ~ femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
                               episode_rac_numero+indemnisation+PBD+married+foreigner+primaire+secondaire+cdi+lic+tx_chge, data = df[Duration == 1], na.action = na.omit, ntree=500, mtry =2)
labels = c("Age","Indemnisation" , "Unemployment rate", "PBD", "Higher education", "Seniority","Foreigner", "Female" , "Upper secondary education",
           "Primary Sector", "Married", "Secondary Sector", "Last contract < 12m",  "Last contract < 3m" ,"Number of current spell", "CDI", "Fired") %>% rev()

varImpPlot(fit_Duration, main = "", labels =labels)

fit_Money <- randomForest(ouverture1 ~ femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
                            episode_rac_numero+indemnisation+PBD+married+foreigner+primaire+secondaire+cdi+lic+tx_chge, data = df[Money== 1], na.action = na.omit, ntree=500, mtry = 2)
labels = c( "Indemnisation","Age", "Unemployment rate", "PBD", "Seniority", "Higher education",  "Upper secondary education", "Female", "Foreigner",
            "Married", "Primary Sector", "Last contract < 12m","Secondary Sector",   "Last contract < 3m" ,"Number of current spell", "CDI", "Fired") %>% rev()
varImpPlot(fit_Money, main = "", labels = labels)


############# using CARET package #####################
library(caret)

#Seting up parallelization :
library(doParallel)
cl <- detectCores() %>% -2 %>% makeCluster
registerDoParallel(cl)


set.seed(123)
mod <- train(ouverture1 ~ Framed+Duration+femme+age+upper_2nd_edu+higher_edu+contrat_moins_12mois+contrat_moins_3mois+episode_rac_numero_mois+
               episode_rac_numero+indemnisation+PBD+married+foreigner+primaire+secondaire+cdi+lic+tx_chge, data = df[Duration == 1], method = "rf", na.action = na.omit)
stopCluster(cl)

print(mod)
print(mod$finalModel)

labels = c("Age", "DRW", "Indemnisation", "Youth unemployment rate", "Unemployment rate", "Number of Claimants", "Number of participants", "PBD",
           "Share of part time unemployment","Share of long-term unemployment", "Months since start of spell", "Higher education", "Exit rate from unemp",
           "Intermediate education", "Women", "Foreigner", "Duration", "Married", "Neutral", "Last contract < 12m", "Secondary Sector", "Last contract < 3m",
           "Primary Sector", "Fired", "CDI") %>% rev()

varImpPlot(mod$finalModel, main = "Random Forest variables ranking")
