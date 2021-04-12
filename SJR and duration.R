library(gridExtra)

# salaire de référence / nombre de jours travailles *1.4
# SUppose that month of 31 days 27 days worked 


Alloc = function(SR){ SJR = SR/(22*1.4)
                  if(SR < 1191.42){ SJR*0.75*30}
                    else if( 1191.42 <= SR & SR < 1304.87){ 29.38*30 }
                        else if( 1304.88 <= SR & SR < 2207.94){ (SJR*0.404+12.05)*30 }
                            else if( 2207.95 <= SR & SR < 13712){ (SJR*0.57)*30 } }

#!!! Divisé par le nombre de jours travaillés :: en dessous de 1191.42
# 1191.42/10.25
# 116.2361/35 = 3.321031 

x <- seq(500,3500,0.1)
vect <- Vectorize(Alloc)
alloc <- vect(seq(500,3500,0.1))

SR <- function(x){x/(27*1.4)*30} 
low <-  function(x){((x/(27*1.4))* 0.404 + 12.05)*30}
low <- low(seq(500,2500,0.1))
high <- function(x){((x/(27*1.4))* 0.75)*30}
high <- high(seq(500,2500,0.1))
SR <- SR(seq(500,3500,0.1))


data <- data.frame(x,alloc, SR )
data <- data %>% mutate(dif_abs = alloc - SR, dif_rel = (alloc - SR)/SR*100, SJR= SR/(25*1.4), lim_l = 29.38*30  )
data <- data %>%  mutate(lim_h = 0.75*SJR*30)

#interesting to show the different absolute and relative value drop ! 


library(ggplot2)


g <- ggplot(data = data, show.legend = TRUE) 

p1 <- g + geom_line( aes(x = x, y = alloc), color = "blue") + 
  geom_line(aes(x= x, y =SR), color = "black") +
  labs( x = "Salaire de référence", y = "indemnisation", title = "French indemnisation scheme 2222") +
  theme(plot.title = element_text(hjust = 0.5))
p1


+
  xlim(1000,4000)
geom_line(aes(x= x, y =lim_h), color = "pink")+
  geom_line(aes(x= x, y =lim_l), color = "pink")+

p2 <- g + geom_line(aes(x= x, y =dif_abs), color = "red")+ 
  labs( x = "Salaire de référence", y = "SR - indemnisation", title = "Difference absolu entre SR et indemnisation") +
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(1000,1500)

p3<- g +geom_line(aes(x= x, y =dif_rel), color = "orange") +
  labs( x = "Salaire de référence", y = "Perte relative en %", title = "Difference relative entre SR et indemnisation") +
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(1000,1500)


grid.arrange(p1, p2, p3, ncol=1)

#148,54 ??? / jour ou 4 518 ???


#durée d'indemnisation depend de l'age 
# durée indemnisation = 1.4 * jours travaillés dans la peiode de référence
#periode de référence --> 24 mois pour  < 53ans,36 mois > 53
# <53, max 730 jours( 24 mois), 53-54 +182 (6 mois), >55 36 mois(1095), 62 -> jusqu'a retraite max 67 ans 



ggplot( data = filter(df, indemnisation > 0)) + geom_histogram(aes(x= indemnisation), bins = 100) + xlim(0,3000)


ggplot( data = filter(df, indemnisation > 0), aes(x = kqcsjp, y = indemnisation )) + geom_smooth()







