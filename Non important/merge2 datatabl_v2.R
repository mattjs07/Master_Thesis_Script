
setwd("D:/THESIS")

library(data.table)
library(beepr)


####### BUILDING AND SAVING THE MERGED DATASET #######

setDTthreads(threads = 8)

mail <- fread("mail.csv")


panel <- fread("exploitation_panel_mai20.csv")


intersect(names(mail), names(panel))

panel_merged <- merge.data.table(panel, mail, by = c("krin" , "kcass","treated" ), all.x = TRUE)

fwrite(panel_merged, "merge_finalv1.csv")



