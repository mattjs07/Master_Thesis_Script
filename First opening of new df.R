library(data.table)
library(bit64)
library(dplyr)

df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/finale_merge_filtered_csv.csv ")

View( head(df,100) )

treatedmail <- df %>% filter(date == 678) %>% filter(!is.na(ouverture1))
table(treatedmail$krin) %>% length() # We got 8747 mail treated indiv
table(table(dfcsv$krin)) # This time not all indiv have obs for 40 periods. 
                              #Only because krin is not an uniquely identifier
table(table(df$indiv)) # --> All have 44 periods of observations !