library(pkgloadr)
library(fun)
library(zoo)

data.table::setDTthreads(8)
df <- fread("C:/Users/matti/Desktop/Thesis/Data/R/Data/dataframe_finalv2.csv" )
df <- filter(df, date >=684) #analysis only on post 684


#visualizing
# df %>% head(1000) %>% select(indiv,date,episode_rac_numero, episode_rac_numero_mois, etat_fin_de_mois, defm,
#                              indemnisation, nbj_rac,nbj_consommes_dans_le_droit,PBD, kpjdxp_droit, kpjdxp, date_odd, datefinpot, échéance_droit) %>% View()

###################################################
#correcting the episode numero, s.t it starts at 684.
##################################################

df[, episode_janv := episode_rac_numero]

n = df %>% group_by(indiv) %>% summarise(n = min(episode_rac_numero, na.rm = TRUE))
table(n$n)
n = filter(n, !is.infinite(n))
n = n %>% as.data.table()
n = n[n > 1]
n$n = n$n - 1

pb <- txtProgressBar(min=0, max = nrow(n), style = 3)
for(i in 1:nrow(n)){
  df[indiv == n$indiv[i], episode_janv := episode_rac_numero - n$n[i]]
  setTxtProgressBar(pb, i)
}

#########################################################
###### Constructing the number of episodes per dates #####
##########################################################

df = df %>% as.data.table()
df[date <= 696, max_episode_12m := max(episode_janv, na.rm = TRUE), by = indiv]
df[, max_episode_12m := na.locf(max_episode_12m), by = indiv]
#
df[date <= 708, max_episode_24m := max(episode_janv, na.rm = TRUE), by = indiv]
df[, max_episode_24m := max(max_episode_24m, na.rm = TRUE), by = indiv]
#
df[date <= 720, max_episode_36m := max(episode_janv, na.rm = TRUE), by = indiv]
df[, max_episode_36m := na.locf(max_episode_36m), by = indiv]


#########################################################
#### Constructing truncated spells for JANUARY  #########  #var =  episode_rac_mois_janv
#########################################################

start_mois = df[date == 684 & episode_rac_numero_mois > 1, .(indiv, episode_rac_numero_mois)]
start_mois[, episode_rac_numero_mois := episode_rac_numero_mois -1]
df[,episode_rac_mois_janv := episode_rac_numero_mois]

pb <- txtProgressBar(min=0, max = nrow(start_mois), style = 3)
for(i in 1:nrow(start_mois)){
  df[indiv == start_mois$indiv[i] & episode_janv == 1 , episode_rac_mois_janv := episode_rac_numero_mois -start_mois$episode_rac_numero_mois[i]]
  setTxtProgressBar(pb, i)
}

###################################################
## COmputing average duration for NON_truncated ###
###################################################

df[, length_spell := max(episode_rac_numero_mois), by = .(indiv, episode_janv)]

#12months
df[date <= 696, av_spell_12m := sum(unique(length_spell),na.rm = TRUE)/max_episode_12m, by =indiv  ]
df[, av_spell_12m := na.locf(av_spell_12m), by = indiv]
#24 months
df[date <= 708, av_spell_24m := sum(unique(length_spell),na.rm = TRUE)/max_episode_24m, by =indiv  ]
df[, av_spell_24m := na.locf(av_spell_24m), by = indiv]
#36months
df[date <= 720, av_spell_36m := sum(unique(length_spell),na.rm = TRUE)/max_episode_36m, by =indiv  ]
df[, av_spell_36m := na.locf(av_spell_36m), by = indiv]


###################################################
## COmputing average duration for TRUNCATED ###
###################################################

df[, length_spell_trunc := max(episode_rac_mois_janv), by = .(indiv, episode_janv)]

#12months
df[date <= 696, av_spell_12m_trunc := sum(unique(length_spell_trunc),na.rm = TRUE)/max_episode_12m, by =indiv  ]
df[, av_spell_12m_trunc := na.locf(av_spell_12m_trunc), by = indiv]
#24 months
df[date <= 708, av_spell_24m_trunc := sum(unique(length_spell_trunc),na.rm = TRUE)/max_episode_24m, by =indiv  ]
df[, av_spell_24m_trunc := na.locf(av_spell_24m_trunc), by = indiv]
#36months
df[date <= 720, av_spell_36m_trunc := sum(unique(length_spell_trunc),na.rm = TRUE)/max_episode_36m, by =indiv  ]
df[, av_spell_36m_trunc := na.locf(av_spell_36m_trunc), by = indiv]



################### TRASH ################

# df = mutate(df,  defm_int = ifelse(defm == "0",1, 0))
# df = group_by(indiv) %>%  mutate(defm_av = cumsum(defm_int) / max(episode_janv) )


# n = df %>%  group_by(indiv) %>%  summarise( rac= length(unique(episode_rac_numero)),droit = length(unique(kpjdxp_droit)) )
# 
# n = n %>%  mutate(right = rac - droit)
# 
# table(n$right)["0"] / nrow(n)  ## only 68% have mathcing rac numero and PBD_droit
# 
# keeper = which(n$right == 0)
# keeper = n[keeper, "indiv"]
# keeper = pull(keeper)
# 
# df <- df[indiv %in% keeper]

# df <- df %>%  mutate( échéance_droit = kpjdxp_droit - nbj_consommes_dans_le_droit )

# prb : episode_rac_numero might start at x > 1 
# need to build variable : episode_nbr_cum_janv


#Average time, control for average PBD ? 
#Prb : in kpjdxp_droit, when iar it dimnishes the days consumed ! 
#however, can use, original PBD and the episode_rac_numero_mois ? --> prb, when iar, jours pas décontés, continue unemployment, nbr_mois > basline PBD
#Thereby should stick to pbd_droit - jours consommés = reflect durée of PURE unemployment
# or could filter out indivit that experience iar ? --> robustness check 
# still should construct, the average kpjdxp_droit and average IAR_cum dans le droit (using cumsum) ? 
#no way around : prb = variable kpjdxp_droit. AND nbj_consommes dans le droit is weird as well  
# 
# z = df %>% group_by(indiv, episode_rac_numero, .add = TRUE) %>% summarise(max_consumed_abs = max(nbj_consommes_dans_le_droit), max_consumed_frac = max(nbj_consommes_dans_le_droit)/max(kpjdxp_droit) ) %>%  ungroup()
# 
# z = unique(z)
# z = na.omit(z)
# z %>% count(indiv)
# 
# w = which(z$max_consumed_frac > 1)
# df_i = unique(df$indiv) %>%  length()
# length(w)/ df_i # about 2.5% of indiv imapcted
# length(w)/nrow(z) #about 2% of spells impacted
# 
# spells <- df %>% count(indiv, episode_rac_numero) %>% na.omit() %>% count(indiv)
# spells = spells %>%  as.data.table()
# n_multispell = spells[n >1, indiv] %>% length()
# 
# length(w)/n_multispell #about 7% of individuals with multiple spells
# # In any way : error is correlated with the number of spells 
# 
# 
# 
# 
# z[w,]
# w = z[w,"indiv"]
# w = pull(w)
# filter(z, indiv %in% w)



# z = z %>% filter(max_consumed_frac <=1)









