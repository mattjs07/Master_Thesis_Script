
library(dplyr)
library(data.table)
library(fastDummies)
library(bit64)
library(beepr)

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/finale_merge_filtered_csv.csv "); beep(8)

dfgd <- df %>% group_by(date)
table(table(dfgd$individu)) # All have 44 T 

table(df$date) #678-721
table(table(df$date)) # 115707  :: There is 115707 individuals in the DF observed for 44 months 

library(xlsx)
library(xtable)

# DATE <- df %>% filter(indiv == 150187) %>% select(date, mois, annee) %>% arrange(date) 
# write.xlsx(DATE, "info_date.xlsx")

# sink('date_info.html')
# print(xtable(DATE),type='html')
# sink()
                              ######Exporting as excel and html table, uncomment



##  Need to filter the dataset, by getting rid of the individuals who were already in Activité réduite when experiment started
# 678 = Juillet 2016     +1 for each month


# randomization took place in December 2016
# We identiied job seekers who registered for the very first time for unemployment benefits between 1 July 2016 and 30 November 2016.
# fin de mois 678
#Nov = 682 

df %>% filter(date == 678) %>% select(episode_rac_numero) %>%  table(exclude = NULL)


####### TESTS FOR GETTING RID OF WORKERS WHO HAVE BEEN JSEEKER BEFORE THE EXPERIMENT


df_start <- filter(df, date %in% 678:682) ## take the period July 2016 to Novemeber 2016

df_group <- df_start %>% group_by(indiv) %>% filter(any(episode_rac_numero == 1))  # Delete the individuals which did not experience their first spell during this period

table(df_start$episode_rac_numero, exclude = NULL)
table(df_group$episode_rac_numero, exclude = NULL)

length(table(df_start$indiv)) - length(table(df_group$indiv)) # We filter out 730 indiv
setdiff(df_start, df_group) %>% View() # Indeed had to be excluded 

w <- c(which(df_group$episode_rac_numero %in% 4:10))
df_group %>% filter(indiv %in% w) %>% View() # This is empty !! WTF ?


`%notin%` = Negate(`%in%`)
#If they don't exist, this should not make a difference :: 

df_group2 <- df_group %>% group_by(indiv) %>% filter(all(episode_rac_numero %notin% 4:10)) #BUt number obs differ:
View(  arrange(setdiff( df_group, df_group2 ), indiv, date)    )

df_group %>% group_by(indiv) %>%  filter(any(episode_rac_numero %in% 5:10)) %>% select(indiv, date, episode_rac_numero) %>% arrange(indiv, date) %>%  View()
#still some strange behaviors :: ERN 10-1 

# Looks like all the indiv retained with ERN > 4 have an odd numbering of number of spells 
# Can check, same problem for 4 
# Even for ERN == 3, it is a common problem ... 


#### Once filtered the indiv to keep, need to filter the entire dataset (other periods) ###

keeper <- df_group2$indiv

df <- df %>% filter(indiv %in% keeper)

######## Exclude person with specific rules  ###########

table(df$kcemp) # --> takes several values, do they exclude them all ?

df <- filter(df, kcemp == "")
table(df$kcemp)


#### Filtering :  keeping indiv who were on claim an never experienced parti-time at date of first sending #### 
# first sending = 31 january --> so look at situation at end of the month "etat_fin_de_mois" in 684
                                                                  ### --> Need I NI HRAC ? INS? HINS ? 

#since we filtered to keep only those who started in period July-October, to know if EVER exp AR, need to know if exp AR since July !
# --> Need never AR in period 678- 684

df_78_84 = df %>%  filter(date %in% 678:684)




df <- df_group %>%  group_by(krin) %>% filter(all(episode_rac_numero < 4 | is.na(episode_rac_numero)))















