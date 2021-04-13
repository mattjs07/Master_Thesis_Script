library(haven)
library(dplyr)
library(data.table)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")


mail <- read_dta("mail.dta")

##########################################################################################################################################
################################################ LOOKING AT SHARES IN DIFFERENT SUBGROUPS ################################################
##########################################################################################################################################

# T1 by objet
mail_byOb <- mail %>% group_by(objet1) %>% summarise(t1 = mean(ouverture1))

# Every mail by objet and type B/N 
mail_Ob1 <- mail %>% group_by(objet1, groupe) %>% summarise(share_open = mean(ouverture1), .groups = "keep")
mail_Ob1 # 1 = "Information important", 2 = "Prolonger l'indemnisation au chozmage", 3 = " Augmenter ses revenus pendant le chômage"  # Observe that 3 has 7pp less open rate 

mail_Ob2 <- mail %>% group_by(objet2, groupe) %>% summarise(share_open = mean(ouverture2), .groups = "keep")
mail_Ob2 <- mail_Ob2[-c(7),]
mail_Ob2

mail_Ob3 <- mail %>% group_by(objet3, groupe) %>% summarise(share_open = mean(ouverture3), .groups = "keep")
mail_Ob3 

MAIL <- cbind(mail_Ob1, mail_Ob2, mail_Ob3)
names(MAIL)<- c("Objet","type","t1", "Ob_2","B/N2","t2", "Ob_3","B/N3","t3")
MAIL <- as.data.frame(MAIL) 
MAIL <- dplyr::select(MAIL, Objet, type, t1, t2, t3)
MAIL #Observe a declining pattern of opening in every subset. However, it is declining way less in neutral group --> since it is labelled as "important message (do not know it is the same one again )
# THIS IS STRANGE SINCE RE-RANDOMIZED AT EVERY PERIOD !!

### The effect of BRUT / NET isn't supposed to be observed with clics / take up rate (HOwever, Cahuc find no significant differneces)

mail_B.N <- mail %>% group_by(groupe) %>% summarise(t1_open = mean(ouverture1), t1_click = mean(clic1),
                                                    t2_open = mean(ouverture2, na.rm = TRUE), t2_click = mean(clic2, na.rm = TRUE),
                                                    t3_open = mean(ouverture3), t3_click = mean(clic3)) #Very small difference (0.1% at most)
# The click is a proxy of interest in part time, but actuall enrollment figures might be bigger. 


full1 <-  filter(mail, objet1 ==1 & objet2 == 1 & objet3 == 1 ) 

full2 <- filter(mail, objet1 ==2 & objet2 == 2 & objet3 == 2 )

full3 <- filter(mail, objet1 ==3 & objet2 ==3 & objet3 ==3 )

FULL <- rbind(full1, full2, full3)

length(full1$treated)
length(full2$treated)
length(full3$treated)
length(FULL$treated) #### 18509 OBS 


library(dummies)
library(fastDummies)

mail <- dummy_cols(mail, select_columns = c("objet1", "objet2", "objet3"), ignore_na = TRUE)  ## Good to know, can add the argument "remove_first_dummy", or "remove_most_frequent_dummy" 
                                                                                              ## ignore_na = TRUE --> NAs, not an additional column for NAs
mail <- mutate(mail, sum1 = objet1_1 + objet2_1 + objet3_1,
                      sum2= objet1_2 + objet2_2 + objet3_2,
                        sum3 = objet1_3 + objet2_3 + objet3_3)


table(mail$sum1); table(mail$sum2); table(mail$sum3) #Big majority of 1 received 3*1. No 2 or 3 received 3*2 or 3*3 --> FOr 2 and 3, evenlly splited between 0/1/2 occurences

# Question now is, are they indiv which received both 2 and 3 object ???? 
View(filter(mail, sum2 >0)) # --> There are indeed
z2 <- filter(mail, sum2 >0 & sum3 == 0) 
z3 <- filter(mail, sum3 >0 & sum2 == 0)   #There are none ............................................ for both case. Thereby impossible to identify causal effect
#There is always either full Neutral or a mixture of 2 and 3 ....... 



## MAKING THE DUMMIES 
mail <- as.data.table(mail)
mail <- mail[,":="(Neutral =0,Framed=0)] # creates 3 new columns with value 0

mail[ sum1 == 3, Neutral := 1 ]
mail[ sum1 == 0, Framed := 1 ]

mail <- mail[,":="(Duration = 0, Money = 0)]
mail[ sum2 == 2, Duration := 1 ]
mail[ sum2 == 1, Money := 1 ]

## Point == when did they enroll ? # To study the impact might split the sample by the 3 dates. Prb : already is a small effect to start with --> 

##########################################################################################################################################
############################################## BALANCING TESTS ###########################################################################
##########################################################################################################################################



mail <- mail %>%  mutate( group = ifelse(Neutral ==1, 1, ifelse(Duration == 1,2,3)) )
mail <- mail %>% mutate(open_sum = ouverture1 + ouverture2 + ouverture3)
GRP <- mail %>% group_by(group)

mail <- mail %>%  mutate( group2 = ifelse(Neutral ==1, 1, ifelse(Money == 1,0,NA)) )
mail <- mail %>%  mutate( group3 = ifelse(Money ==1, 1, ifelse(Framed == 1,0,NA)) )

mail %>% group_by(group) %>% summarise(t1 = mean(open_sum, na.rm = TRUE))


t.test(data = mail, open_sum ~ group3)



