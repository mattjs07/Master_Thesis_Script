library(gridExtra)
library(pkgloadr)

# salaire de référence / nombre de jours travailles *1.4
#Always take 5 days a week --> en moyenne, environ 20 jours ouvrés par mois
#Smic brut = 10,25 de l'heure. DOnc plein temps = 10.25*35*4 = 1535
# 1535/20*1.4 = 107.45

Alloc = function(SR){ SJR = SR/(20*1.4)
if(SR < 1191.42){ SJR*0.75*20}
else if( 1191.42 <= SR & SR < 1304.87){ 29.38*20 }
else if( 1304.88 <= SR & SR < 2207.94){ (SJR*0.404+12.05)*20 }
else if( 2207.95 <= SR & SR < 13712){ (SJR*0.57)*20 } }


#!!! Divisé par le nombre de jours travaillés :: en dessous de 1191.42
# 1191.42/10.25
# 116.2361/35 = 3.321031 

x <- seq(2205,2210, length.out = 100)
vect <- Vectorize(Alloc)
alloc <- vect(x)

plot(x, alloc, "l", main = "Allocation discontinuity, full time worker", xlab = "Salaire de Référence", ylab = "Allocation")

SR <- function(x){x/(20*1.4)*20} 
low <-  function(x){((x/(20*1.4))* 0.404 + 12.05)*20}
low <- low(x)
high <- function(x){((x/(20*1.4))* 0.75)*20}
high <- high(x)
SR <- SR(x)


df <- data.frame(x,alloc, SR)


df <- df %>% mutate(dif_abs = alloc - SR, dif_rel = (alloc - SR)/SR*100, SJR= SR/(20*1.4), lim_l = 29.38*20  )
df <- df %>%  mutate(lim_h = 0.75*SJR*20)

#interesting to show the different absolute and relative value drop ! 

g <- ggplot(data = df, show.legend = TRUE) 

p1 <- g + geom_line( aes(x = x, y = alloc), color = "blue") +
  labs( x = "Salaire de référence", y = "indemnisation", title = "French indemnisation scheme") +
  theme(plot.title = element_text(hjust = 0.5))
p1

p2 <- g + geom_line(aes(x= x, y = dif_abs), color = "red") +  
  labs( x = "Salaire de référence", y = "SR - indemnisation", title = "Difference absolu entre SR et indemnisation") +
  theme(plot.title = element_text(hjust = 0.5))

p3<- g +geom_line(aes(x= x, y =dif_rel), color = "orange") +
  labs( x = "Salaire de référence", y = "Perte relative en %", title = "Difference relative entre SR et indemnisation") +
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(p1, p2, p3, ncol=1)

#148,54 ??? / jour ou 4 518 ???


#durée d'indemnisation depend de l'age 
# durée indemnisation = 1.4 * jours travaillés dans la peiode de référence
#periode de référence --> 24 mois pour  < 53ans,36 mois > 53
# <53, max 730 jours( 24 mois), 53-54 +182 (6 mois), >55 36 mois(1095), 62 -> jusqu'a retraite max 67 ans 



ggplot( df = filter(df, indemnisation > 0)) + geom_histogram(aes(x= indemnisation), bins = 100) + xlim(0,3000)


ggplot( df = filter(df, indemnisation > 0), aes(x = kqcsjp, y = indemnisation )) + geom_smooth()







