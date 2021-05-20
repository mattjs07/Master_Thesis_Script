library(pkgloadr)
library(latex2exp)


t = seq(0,2, length.out = 100)

a_t = c(rep(1,70), rep(0,30))
a_t1 = c(rep(0,70), rep(1,30))
a_t_cum = cumsum(a_t)



a_t1_cum = cumsum(a_t1)

A_t = function(a){ exp(-0.01 * a )} 


survival = data.frame(t, a_t, a_t1)
survival = survival %>%  mutate(t1 = t - 1.41414141)
t - 1.41414141
survival = survival %>%  mutate(att = a_t * t, at1t = a_t1 * t1)
survival = survival %>%  mutate(a_t_cum = cumsum(att), a_t1_cum = cumsum(at1t))


survival = survival %>%  mutate( A_t = A_t(a=a_t_cum), A_t1 = A_t(a=a_t1_cum))
survival = survival %>%  mutate(F_t = 1 -A_t, F_t1 =1-A_t1)
g1 <- ggplot(data = survival) + geom_line(aes(x = t, y = A_t), color ="red",size = 1) + geom_line(aes(x = t, y = A_t1) , color ="blue",size = 1)
g2 <- ggplot(data = survival)+ geom_line(aes(x = t, y = a_t), color ="red", size = 1)+ geom_line(aes(x = t, y = a_t1), color ="blue", size = 1)
g3 <- ggplot(data = survival) + geom_line(aes(x = t, y = F_t), color ="red",size = 1) + geom_line(aes(x = t, y = F_t1) , color ="blue",size = 1)

grid.arrange(g1, g2, g3)

A_t = data.frame(survival$A_t)
names(A_t) = "A"
A_t1 = data.frame(survival$A_t1)
names(A_t1) = "A"
A = rbind(A_t,A_t1)

F_t = data.frame(survival$F_t)
names(F_t) = "F"
F_t1 = data.frame(survival$F_t1)
names(F_t1) = "F"
F = rbind(F_t,F_t1)


a_t = data.frame(survival$a_t)
names(a_t) = "a"
a_t1 = data.frame(survival$a_t1)
names(a_t1) = "a"
a = rbind(a_t,a_t1)

Message = rep(c("Money","Duration"), each = 100)

survival2 = data.frame(t = rep(t,2),Message = as.factor(Message), A, F, a)

g1 <- ggplot(data = survival2) + geom_line(aes(x = t, y = A, color = Message), size =1) + labs(title = "Attention Allocation Strategy") + 
  ylab(TeX("$A_{t}$")) + theme(plot.title = element_text(hjust = 0.5)) + xlab("") + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
g2 <- ggplot(data = survival2) + geom_line(aes(x = t, y = a, color = Message), size =1)  + ylab(TeX("$a_{t}$")) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + xlab("")
g3 <- ggplot(data = survival2) + geom_line(aes(x = t, y = F, color = Message), size =1)  + ylab(TeX("$1 -A_{t}$"))+
   scale_x_continuous(breaks = c(0,1, 1.414,2), labels = c("0","1",TeX(r'($\lambda$)'), "2"))  

grid.arrange(g1, g2, g3)

