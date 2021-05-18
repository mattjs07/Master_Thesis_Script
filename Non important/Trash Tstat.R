
#### W test ####
W.test <- function(x, y, z){ 
  W <- function(X) { ww <- wilcox.test( eval(as.symbol(X)) ~ eval(as.symbol(y)),data = z) 
  ww[[3]]
  }
  lapply(x, W)
}
W_T_C <- W.test(vars684, "treated", T_C_684) %>%  append( W.test(vars683, "treated", T_C_683))

W_N_F <- W.test(vars684, "Framed", N_F_684) %>%  append( W.test(vars683, "Framed", N_F_683)) # Observe significant differences for some variables :: How to deal with it ? Control in regression?

W_T_CSC <- W.test(vars684, "treated", T_C_SC_684) %>%  append( W.test(vars683, "treated", T_C_SC_683))

W_T_SC <- W.test(vars684, "treated", T_SC_684) %>%  append( W.test(vars683, "treated", T_SC_683))

table_W <- cbind(W_T_C,W_N_F, W_T_CSC, W_T_SC) %>%  as.data.frame()
table_W < 0.05

library(clusrank)
#clusWilcox.test(femme, cluster = kcala , group = treated, data = T_C_684, method = "rgl")



#### T test ####
T.test <- function(x, y, z){ 
  W <- function(X) { ww <- t.test( eval(as.symbol(X)) ~ eval(as.symbol(y)),data = z) 
  ww[[3]]
  }
  lapply(x, W)
}  

T_T_C <- T.test(vars684, "treated", T_C_684) %>%  append( T.test(vars683, "treated", T_C_683))

T_N_F <- T.test(vars684, "Framed", N_F_684) %>%  append( T.test(vars683, "Framed", N_F_683))

T_T_CSC <- T.test(vars684, "treated", T_C_SC_684) %>%  append( T.test(vars683, "treated", T_C_SC_683))

T_T_SC <- T.test(vars684, "treated", T_SC_684) %>%  append( T.test(vars683, "treated", T_SC_683))

table_T <- cbind(T_T_C,T_N_F, T_T_CSC, T_T_SC) %>%  as.data.frame()
table_T < 0.05

######  t.test.cluster(y, cluster, group)



#### lm test ####

L.test <- function(x, y, z){ 
  W <- function(X) { ww <- lm( eval(as.symbol(X)) ~ eval(as.symbol(y)),data = z) 
  summary(ww)$coefficients[8]
  }
  lapply(x, W)
}  


L_T_C <- L.test(vars684, "treated", T_C_684) %>%  append( L.test(vars683, "treated", T_C_683))

L_N_F <- L.test(vars684, "Framed", N_F_684) %>%  append( L.test(vars683, "Framed", N_F_683))

L_T_CSC <- L.test(vars684, "treated", T_C_SC_684) %>%  append( L.test(vars683, "treated", T_C_SC_683))

L_T_SC <- L.test(vars684, "treated", T_SC_684) %>%  append( L.test(vars683, "treated", T_SC_683))

table_L <- cbind(L_T_C,L_N_F, L_T_CSC, L_T_SC) %>%  as.data.frame()
table_L 
