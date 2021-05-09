library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")
#######
data <- fread("dataframe_finalv2.csv", nThread = 8)

dd = table(data$PBD) %>% as.data.frame() %>% arrange(desc(Freq))
dd[1,"Freq"] /sum(dd$Freq) #43% of All SPELLS have a PBD of 730 (max PBD for under 53yo, = 2ans )


df <- data %>%  filter(PBD == 730)
df <- df %>%  filter(!is.na(ouverture1))

fwrite(df, "df_KM_PBD730.csv")
#######

library(pkgloadr)

setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")

df <- fread("df_KM_PBD730.csv")
ID <- as.list(unique(df$indiv))

#### Histogram ####
time_builder <- function(x){
  if(!(x %in% ID)) stop("invalide identifier")
  d <- filter(df, indiv == x)
  n <- data.frame( t = 1:nrow(d))
  return(cbind(d,n))
}

plan(multisession, gc = TRUE, workers = 8)
options(future.globals.maxSize = 800 * 1024^2)

df730 <- future_map(ID, time_builder, .progress = TRUE) 
df730 <- rbindlist(df730)

df730 <- merge(df, df730)
fwrite(df730, "df730_forhist.csv")

df730 <- fread("df730_forhist.csv")

df730 <- df730 %>% filter(!is.na(iar))
ggplot(df730, aes(x = t, y = iar)) + geom_col(aes(fill = as.factor(Framed)))

sub_MD <- data %>%  filter(Framed == 1)
ggplot(sub_MD, aes(x = t, y = iar)) + geom_col(aes(fill = as.factor(Money))) + scale_fill_discrete(name = "Group", labels = c("Duration", "Money"))
#########

###### Kaplan_Meier 730 ####### 

KM_builder <- function(x, data = df){
  if(!(x %in% ID)) stop("invalid identifier")
  d <- filter(data, indiv == x)
  n <- nrow(d)
  e <- which(d$iar == 1)
  if(sum(e) == 0){ iar = 0; t = n}
  else{ iar =1; t =e[1]}
  return(data.frame(indiv = x, IAR = iar, time = t))
}

plan(multisession, gc = TRUE, workers = 8)

KM_df <- future_map(ID,KM_builder, .progress = TRUE)
KM_df <- rbindlist(KM_df)

df2 <- df %>% filter(date == 684) %>% arrange(indiv) %>% select(indiv, Neutral, Framed, Money, Duration, objet1)
KM_df <- merge(KM_df, df2)
cbind(KM_df, df2$objet1)

library(survival)

km.model2 <- survfit( Surv(time,IAR) ~ Framed, data = KM_df, type = "kaplan-meier")
km.model2
summary(km.model2) 
plot(km.model2, conf.int = F, xlab = "Time (months)", ylab = "%not in IAR = S(t)", main = "KM-model for 730days Spells", col = c("red", "blue"))
legend(33,0.95, legend=c("Neutral","Framed"), lty =1, lwd =2, col = c("red", "blue"), bty="", cex=0.7)  

KM_subMD <- filter(KM_df, Framed == 1)  
  
km.model3 <- survfit( Surv(time,IAR) ~ Money, data = KM_subMD, type = "kaplan-meier")
plot(km.model3, conf.int = F, xlab = "Time (months)", ylab = "%not in IAR = S(t)", main = "KM-model for 730days Spells", col = c("red", "blue"))
legend(33,0.95, legend=c("Duration","Money"), lty =1, lwd =2, col = c("red", "blue"), bty="", cex=0.7)  

km.model4 <- survfit( Surv(time,IAR) ~ objet1, data = KM_df, type = "kaplan-meier")

plot(km.model3, conf.int = F, xlab = "Time (months)", ylab = "%not in IAR = S(t)", main = "KM-model for 730days Spells", col = c("red", "blue", "green"))
legend(33,0.95, legend=c("Neutral","Duration","Money"), lty =1, lwd =2, col = c("red", "blue", "green"), bty="", cex=0.7)  
########


###### Kaplan Meier overall #####


















