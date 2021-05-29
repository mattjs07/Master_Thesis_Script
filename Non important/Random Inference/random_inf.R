library(haven)
library(dplyr)
library(miceadds)
library(fifer)
library(plm)
library(lmtest)
library(multiwayvcov)
library(doParallel)
library(xtable)
library(stargazer)
library(reshape2)
library("rlist")

rm(list = ls())

setwd("~/Dropbox/Testing_AR/Prog/Paper/randomization_test/")

# load data --------------------------------------------------------------------
# ------------------------------------------------------------------------------
sample_AR = read_dta("data/sample_AR.dta")
random_pop = read_dta("data/pop_pour_randomization.dta")

# Define functions to perform randomization inference --------------------------
# ------------------------------------------------------------------------------

# function 1 : 2 levels stratified sampling ------------------------------------
random_sampling = function(data,
                           strata_1 = strata_ALE,
                           strata_2 = strata_DE,
                           ale_trt_status_varname = "no_supercontrole", 
                           DE_trt_status_varname = "treated", 
                           prob_ale_group = 0.8,
                           prob_DE_group = 0.5){
  
  output = data
  output$id = 1:nrow(output)
  
  # 1 - tirage stratifié sur les ALE
  tmp1 = output %>%
    select(id, strata_ALE, strata_DE) %>%
    group_by(strata_ALE) %>% 
    sample_frac(size = prob_ale_group) %>%
    ungroup() %>%
    select(id)
  
  output[[ale_trt_status_varname]] = 0
  output[[ale_trt_status_varname]][tmp1[["id"]]] = 1

  # 2 - tirage stratifié sur les DE 
  tmp2 = output[tmp1[["id"]],] %>%
    select(id,strata_ALE, strata_DE) %>%
    group_by(strata_DE) %>% 
    sample_frac(size = prob_DE_group) %>%
    ungroup() %>%
    select(id)
  
  output[[DE_trt_status_varname]] = 0
  output[[DE_trt_status_varname]][tmp2[["id"]]] = 1
  
  output$id = NULL
  return(output)
}


# function 2 : randomization inference using function 1 ------------------------
random_inf = function(data, 
                      outcome,
                      covariates,
                      fixed_effect_vars,
                      trt_vars = c("treated","no_supercontrole"),
                      nb_reps = 100, 
                      set_seed = NULL){
  
  start = Sys.time()
  
  if(!is.null(set_seed)){set.seed(set_seed)}
  
  for(v in fixed_effect_vars){
    data[[v]] = as.character(data[[v]])
  }

  output = data.frame(rep_number = 1:nb_reps)
  form = paste0(outcome, "~", 
                paste(trt_vars, collapse = "+"), "+", 
                paste(covariates, collapse = "+"))
  
  for(i in 1:nb_reps){
    
    if(i/50 == i%/%50){print(i)}
    # reassign the treatement status 
    random_sample = random_sampling(data)
    # regress 
    reg_random = lm(formula = form, data = random_sample)
    # store in the table 
    output[i,2:(length(trt_vars)+1)] = reg_random$coefficients[2:(length(trt_vars)+1)]
  }
  
  colnames(output)[2:(length(trt_vars)+1)] = trt_vars

  end = Sys.time()
  print(end - start)
  
  return(output)

}

# function 3 : sshowing the results --------------------------------------------
show_random_inf = function(table, reg_expe){
  
  output = data.frame(coefficient_name = colnames(table)[-1], coefficient_est = NA, model_based_p_val = NA, ri_p_val = NA)
  
  for(coef in colnames(table)[-1]){
    
    # add coeff values from experimental reg 
    output[["coefficient_est"]][output[["coefficient_name"]] == coef] = reg_expe[,1][coef]
    # add model based p value
    output[["model_based_p_val"]][output[["coefficient_name"]] == coef] = reg_expe[,4][coef]
    # add randomization p value 
    output[["ri_p_val"]][output[["coefficient_name"]] == coef] = round(sum(abs(table[[coef]]) >= abs(reg_expe[,1][coef]))/(nrow(table)),3)
  }
  
  output$nb_reps = nrow(table)

  return(output)
}

# Perform RI -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# define parameters ------------------------------------------------------------
tmp = sample_AR %>% filter(numero_mois == 36 & information_subset==1)
data = left_join(random_pop, tmp)
covariates = c("femme", "jeune", "senior", "higher_edu", "lower_2nd_edu", "kpjdxp", "kqcsjp",
               "nombre_de_rct", "contrat_moins_3mois", "contrat_moins_12mois", "tx_chge", "proportion_de_ar",
               "moisregion", "moisodd",
               "nombre_de", "proportion_de_ld", "proportion_de_sortants")
fixed_effect_vars = c("moisregion", "moisodd")
outcome = "iar_cum"
trt_vars = c("treated","no_supercontrole")
nb_reps = 2000
set_seed = 1

# perform the experimental regression ------------------------------------------
data$moisodd = as.character(data$moisodd)
data$moisregion = as.character(data$moisregion)
form = paste0("iar_cum", "~", paste(trt_vars, collapse = "+"), "+", paste(covariates, collapse = "+"))
reg = lm(formula = form, data = data)
vcov=cluster.vcov(reg, cluster = data$kcala)
reg_expe = coeftest(reg, vcov. = vcov)
reg_expe

# RI ---------------------------------------------------------------------------
randomization_table = random_inf(data = data,
                                 outcome = outcome,
                                 trt_vars = trt_vars,
                                 fixed_effect_vars = fixed_effect_vars,
                                 covariates =  covariates,
                                 set_seed = set_seed,
                                 nb_reps = nb_reps)

show_random_inf(table = randomization_table,
                reg_expe = reg_expe)

# replicates paper's results with RI 
covariates = c("femme", "jeune", "senior", "higher_edu", "lower_2nd_edu", "kpjdxp", "kqcsjp","contrat_moins_3mois", "contrat_moins_12mois", 
               "nombre_de_rct", "tx_chge", "proportion_de_ar","nombre_de", "proportion_de_ld", "proportion_de_sortants",
               "moisregion", "moisodd")
fixed_effect_vars = c("moisregion", "moisodd")
outcomes = c("sortietotale_ac_emb2_lm", "sortietotale_ac_emb2_lq")
trt_vars = c("treated","no_supercontrole")
nb_reps = 5000
durations = c(1)
set_seed = 1

list = list()
for (i in outcomes){
  for (j in durations){
    list = list.append(list, c(i,j))
  }
}

# detectCores()
registerDoParallel(cores=2)
getDoParWorkers()

start = Sys.time()
RI_results = foreach(i=1:length(list), .combine = rbind) %dopar% {
  
  outcome = list[[i]][1]
  d = list[[i]][2]
  
  # print(paste("duration :",d, "- outcome :", outcome))
  
  # choose the initial sample 
  tmp = sample_AR %>% filter(numero_mois == d & information_subset==1 & kpjdxp < 730)
  data = left_join(random_pop, tmp, by = "kcrna")
  

  randomization_table = random_inf(data = data,
                                   outcome = outcome,
                                   trt_vars = trt_vars,
                                   fixed_effect_vars = fixed_effect_vars,
                                   covariates =  covariates,
                                   set_seed = set_seed,
                                   nb_reps = nb_reps)
  
  # experimental regression 
  data$moisodd = as.character(data$moisodd)
  data$moisregion = as.character(data$moisregion)
  form = paste0(outcome, "~", paste(trt_vars, collapse = "+"), "+", paste(covariates, collapse = "+"))
  reg = lm(formula = form, data = data)
  vcov=cluster.vcov(reg, cluster = data$kcala)
  reg_expe = coeftest(reg, vcov. = vcov)

  output = show_random_inf(table = randomization_table,reg_expe = reg_expe)
  
  output$outcome = outcome 
  output$duration = d
  
  output
}
end = Sys.time()
print(end - start)

# save(RI_results, file = "RI_results_unemp_inf730_5000reps.RData")

# Exporting the results --------------------------------------------------------
# ------------------------------------------------------------------------------

# Table on extensive margin ----------------------------------------------------
load("RI_results_part_time_unemp_extensive_5000reps.RData")
tmp = RI_results

RI_latex = as.data.frame(matrix(ncol = 10, nrow = 2*length(unique(tmp$outcome))))

RI_latex[,1] = rep(c("Treated ($\\beta$)","In a treated area ($\\delta$)"),length(unique(tmp$outcome)))

RI_latex[,2] = round(tmp$coefficient_est[tmp$duration==3],4)
RI_latex[,5] = round(tmp$coefficient_est[tmp$duration==12],4)
RI_latex[,8] = round(tmp$coefficient_est[tmp$duration==36],4)

RI_latex[,3] = round(tmp$model_based_p_val[tmp$duration==3],4)
RI_latex[,6] = round(tmp$model_based_p_val[tmp$duration==12],4)
RI_latex[,9] = round(tmp$model_based_p_val[tmp$duration==36],4)

RI_latex[,4] = round(tmp$ri_p_val[tmp$duration==3],4)
RI_latex[,7] = round(tmp$ri_p_val[tmp$duration==12],4)
RI_latex[,10] = round(tmp$ri_p_val[tmp$duration==36],4)

addtorow <- list()
addtorow$pos <- list(0,0,0,0,0,0,0,0,2,2,2,4,4,4)
addtorow$command <- c("& \\multicolumn{3}{c}{3 months} & \\multicolumn{3}{c}{12 months} & \\multicolumn{3}{c}{36 months} \\\\\n",
                      "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
                      "& Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} \\\\\n",
                      "\\cmidrule(lr){3-4} \\cmidrule(lr){6-7} \\cmidrule(lr){9-10}",
                      "& estimate & model & rand. & estimate & model & rand.  & estimate & model  & rand.   \\\\\n",
                      "& & based & inference &  & based & inference  &  &  based &  inference  \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel A} : Cumulative number of months with work while on claim} \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel B} : Cumulative number of hours worked while on claim} \\\\\n",
                      "\\hline ",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel C} : Cumulative earnings (in euro) from work while on claim} \\\\\n",
                      "\\hline ")

print(xtable(RI_latex, align = c("clccccccccc")),
      include.rownames = FALSE,
      add.to.row = addtorow,
      include.colnames = FALSE, 
      sanitize.text.function = function(x) {x})

print(xtable(RI_latex, align = c("clccccccccc"), auto = TRUE),
      include.rownames = FALSE, 
      add.to.row = addtorow,
      include.colnames = FALSE, 
      file = "ri_latex_part_time_unemp_extensive_5000reps.tex", 
      sanitize.text.function = function(x) {x})

# Table on intensive margin ----------------------------------------------------
load("RI_results_part_time_unemp_intensive_5000reps.RData")
tmp = RI_results

RI_latex = as.data.frame(matrix(ncol = 10, nrow = 2*length(unique(tmp$outcome))))

RI_latex[,1] = rep(c("Treated ($\\beta$)","In a treated area ($\\delta$)"),length(unique(tmp$outcome)))

RI_latex[,2] = round(tmp$coefficient_est[tmp$duration==3],4)
RI_latex[,5] = round(tmp$coefficient_est[tmp$duration==12],4)
RI_latex[,8] = round(tmp$coefficient_est[tmp$duration==36],4)

RI_latex[,3] = round(tmp$model_based_p_val[tmp$duration==3],4)
RI_latex[,6] = round(tmp$model_based_p_val[tmp$duration==12],4)
RI_latex[,9] = round(tmp$model_based_p_val[tmp$duration==36],4)

RI_latex[,4] = round(tmp$ri_p_val[tmp$duration==3],4)
RI_latex[,7] = round(tmp$ri_p_val[tmp$duration==12],4)
RI_latex[,10] = round(tmp$ri_p_val[tmp$duration==36],4)

addtorow <- list()
addtorow$pos <- list(0,0,0,0,0,0,0,0,2,2,2)
addtorow$command <- c("& \\multicolumn{3}{c}{3 months} & \\multicolumn{3}{c}{12 months} & \\multicolumn{3}{c}{36 months} \\\\\n",
                      "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
                      "& Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} \\\\\n",
                      "\\cmidrule(lr){3-4} \\cmidrule(lr){6-7} \\cmidrule(lr){9-10}",
                      "& estimate & model & rand. & estimate & model & rand.  & estimate & model  & rand.   \\\\\n",
                      "& & based & inference &  & based & inference  &  &  based &  inference  \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel A} : Cumulative number of hours worked while on claim} \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel B} : Cumulative earnings (in euro) from work while on claim} \\\\\n",
                      "\\hline ")

print(xtable(RI_latex, align = c("clccccccccc")),
      include.rownames = FALSE,
      add.to.row = addtorow,
      include.colnames = FALSE, 
      sanitize.text.function = function(x) {x})

print(xtable(RI_latex, align = c("clccccccccc"), auto = TRUE),
      include.rownames = FALSE, 
      add.to.row = addtorow,
      include.colnames = FALSE, 
      file = "ri_latex_part_time_unemp_intensive_5000reps.tex", 
      sanitize.text.function = function(x) {x})

# Table on unemp ----------------------------------------------------
load("RI_results_part_time_unemp_5000reps.RData")
tmp1 = RI_results
tmp1$sample = "All"
load("RI_results_unemp_sup730_5000reps.RData")
tmp2 = RI_results
tmp2$sample = "sup730"
load("RI_results_unemp_inf730_5000reps.RData")
tmp3 = RI_results
tmp3$sample = "inf730"

tmp = rbind(tmp1,tmp2,tmp3)

RI_latex = as.data.frame(matrix(ncol = 10, nrow = 2*length(unique(tmp$outcome))))
RI_latex[,1] = rep(c("Treated ($\\beta$)","In a treated area ($\\delta$)"),length(unique(tmp$outcome)))

RI_latex[,2] = round(tmp$coefficient_est[tmp$sample=="All"],4)
RI_latex[,5] = round(tmp$coefficient_est[tmp$sample=="inf730"],4)
RI_latex[,8] = round(tmp$coefficient_est[tmp$sample=="sup730"],4)

RI_latex[,3] = round(tmp$model_based_p_val[tmp$sample=="All"],4)
RI_latex[,6] = round(tmp$model_based_p_val[tmp$sample=="inf730"],4)
RI_latex[,9] = round(tmp$model_based_p_val[tmp$sample=="sup730"],4)

RI_latex[,4] = round(tmp$ri_p_val[tmp$sample=="All"],4)
RI_latex[,7] = round(tmp$ri_p_val[tmp$sample=="inf730"],4)
RI_latex[,10] = round(tmp$ri_p_val[tmp$sample=="sup730"],4)

addtorow <- list()
addtorow$pos <- list(0,0,0,0,0,0,0,0,0,2,2,2)
addtorow$command <- c("& & & \\multicolumn{6}{c}{Potential Benefit Duration} \\\\\n",
                      "& \\multicolumn{3}{c}{All sample} & \\multicolumn{3}{c}{$<$ 730} & \\multicolumn{3}{c}{$\\geq$ 730} \\\\\n",
                      "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
                      "& Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} &  Coeff. & \\multicolumn{2}{c}{p-value} \\\\\n",
                      "\\cmidrule(lr){3-4} \\cmidrule(lr){6-7} \\cmidrule(lr){9-10}",
                      "& estimate & model & rand. & estimate & model & rand.  & estimate & model  & rand.   \\\\\n",
                      "& & based & inference &  & based & inference  &  &  based &  inference  \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel A} : Prob. to be out of unemployment in the last month} \\\\\n",
                      "\\hline ",
                      "\\multicolumn{10}{l}{\\textit{Panel B} : Prob. to be out of unemployment in the last quarter} \\\\\n",
                      "\\hline ")

print(xtable(RI_latex, align = c("clccccccccc")),
      include.rownames = FALSE,
      add.to.row = addtorow,
      include.colnames = FALSE, 
      sanitize.text.function = function(x) {x})

print(xtable(RI_latex, align = c("clccccccccc"), auto = TRUE),
      include.rownames = FALSE, 
      add.to.row = addtorow,
      include.colnames = FALSE, 
      file = "ri_latex_unemp_5000reps.tex", 
      sanitize.text.function = function(x) {x})

