df <- data[date < 684, .(indiv, date, iar, indemnisation, PBD, episode_rac_numero)]

data$iar <- data$iar %>% as.numeric()
data$indemnisation <-  data$indemnisation %>%  as.numeric()

data$indiv <- data$indiv %>% as.factor()
data$date <- data$date %>% as.integer

df = data 

df <- df %>% as.data.frame()
plm( iar ~ indemnisation + PBD + episode_rac_numero, data= df, index = c("indiv", "date"), model = "within")
