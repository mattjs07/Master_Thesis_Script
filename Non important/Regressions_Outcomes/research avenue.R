df <- df %>%  mutate( T_left = (PBD/30.417) - episode_rac_numero_mois ) 
df <- df %>%  mutate(T_left_iar = ifelse(iar == 1, T_left, 0 ))
df <- df %>%  mutate(T_left__no_iar = ifelse(iar == 0, T_left, 0 ))
df <- df %>% arrange(indiv, date)

df <- df %>%  mutate(no_iar = ifelse(iar == 0, 1, 0))
df <- df %>% group_by(indiv) %>% mutate(no_iar_cum = cumsum())

df <- df %>% group_by(indiv) %>%  mutate(E_T_left_iar = ifelse(iar == 1, cumsum(T_left_iar)/iar_cum, 0 ),
                                         T_left__no_iar  = ifelse(iar == 0, cumsum(T_left_no_iar), 0 )) %>% ungroup()



df <- df %>% group_by(indiv) %>%  mutate(T_left__no_iar_cum = ifelse(iar == 0, cumsum(T_left_no_iar), 0 )) %>% ungroup()

df <- df %>% mutate(T_left_cum_both = T_left_iar_cum + T_left__no_iar_cum)


df <- df %>%  mutate(T_left_cum_iar = ifelse(iar == 1 ))