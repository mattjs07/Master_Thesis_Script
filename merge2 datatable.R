
setwd("C:/Users/matti/Desktop/Thesis/Data/R")

library(data.table)
library(beepr)
library(readstata13)


####### BUILDING AND SAVING THE MERGED DATASET #######

mail <- read.dta13("mail.dta")
mail <- as.data.table(mail)

panel <- read.dta13("student.dta")
panel <- as.data.table(panel)

panel_merged <- merge.data.table(panel, mail, by = c("krin" , "kcass",  "ouverture1",   "erreur1" ,"ouverture2" ,"erreur2" , "ouverture3" , "erreur3","treated" ), all = TRUE)
save.dta13(panel_merged, file = "panel_merged_all.dta")



