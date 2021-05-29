
setwd("D:/THESIS")

library(data.table)
library(beepr)
library(readstata13)
library(dplyr)

####### BUILDING AND SAVING THE MERGED DATASET #######

mail <- read.dta13("indicateurs_emails.dta")
mail <- as.data.table(mail)

panel <- read.dta13("exploitation_panel_mai20.dta")

panel <- panel  %>% filter(information_subset == 1)


df <- panel %>%  filter(date == 684)
df <- df %>%  as.data.table()
df$kcass <- df$kcass %>%  as.integer()
intersect(names(mail), names(df))

df_merged <- merge.data.table(df, mail, by = c("krin" , "kcass","treated" ), all.x = TRUE)

save.image("merging.RData")


keep <- setdiff(names(df_merged), names(panel))
keep <- c("indiv", keep)
paste(keep, collapse = ",")

df <- df_merged[, .(indiv,groupe,objet1,ouverture1,erreur1,clic1,objet2,ouverture2,erreur2,clic2,objet3,ouverture3,erreur3,clic3)]
df <- df[ order(indiv)]

df <- df %>% slice(rep(1:n(), each = 44))
df <- df[, !"indiv"]

panel <- arrange(panel, indiv, date)
  
panel <- cbind(panel, df)

fwrite(panel, "panel_merge_final_20_04.csv")

