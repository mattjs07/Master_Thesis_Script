

y = c(runif(10,0,1),runif(10,2,3),
      runif(10,10,20),runif(10,50,100),
      runif(10,200,500),runif(10,800,1200),
      rep(NA,60))


treatment = c( rep(rep(0:1, each = 10),3), rep(NA,60))


potent = rep(c(runif(10,0,1),runif(10,2,3),
           runif(10,10,20),runif(10,50,100),
           runif(10,200,500),runif(10,800,1200)), 2)

predi <- rep(c(runif(10,0,1),runif(10,2,3),
           runif(10,10,20),runif(10,50,100),
           runif(10,200,500),runif(10,800,1200)),2) 

dd = data.frame(y,treatment,potent,predi)
dd$controle = rep(c(0,1),each = 60)
dd

library(haven)
setwd("C:/Users/matti/Desktop/Thesis/Data/R/Data")
write_dta(dd, "fakedata2.dta")
