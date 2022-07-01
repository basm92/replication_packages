#Functions for tables
## Create all the functions I use in tables.R
## Otherwise it takes up a lot of space

# Coefficient and SE for first table

get_coef_and_se2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if (t_stat > 1.7) {
    paste(coef, " ", "(", se, ")","*", sep = "") }
  else { paste(coef, " ", "(", se, ")", sep = "")}
}

# again

get_coef_and_se3 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = firstrents_firsttry[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if (t_stat > 1.7) {
    paste(coef, " ", "(", se, ")","*", sep = "") }
  else { paste(coef, " ", "(", se, ")", sep = "")}
}

# and again

get_coef_and_se4 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = secondrents[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if(between(t_stat, 2.58, 10)){
    paste(coef, " ", "(", se, ")","***", sep = "")
  } else if(between(t_stat, 1.96, 2.58)){
    paste(coef, " ", "(", se, ")","**", sep = "")
  } else if(between(t_stat, 1.65, 1.96)){
    paste(coef, " ", "(", se, ")","*", sep = "")
  } else {
    paste(coef, " ", "(", se, ")","", sep = "")
  }
  
}

# and again..
get_coef_and_se6 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = thirdrents[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if(between(t_stat, 2.58, 10)){
    paste(coef, " ", "(", se, ")","***", sep = "")
  } else if(between(t_stat, 1.96, 2.58)){
    paste(coef, " ", "(", se, ")","**", sep = "")
  } else if(between(t_stat, 1.65, 1.96)){
    paste(coef, " ", "(", se, ")","*", sep = "")
  } else {
    paste(coef, " ", "(", se, ")","", sep = "")
  }
  
}

get_coef_and_se5 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = firstrents_pooled[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if (t_stat > 1.7) {
    paste(coef, " ", "(", se, ")","*", sep = "") }
  else { paste(coef, " ", "(", se, ")", sep = "")}
}


## Conditional filtering for first table

mean_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_control_far <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_control_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}
mean_control_close <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_control_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}

## Find the element to extract the p-value
## Depends on parameters close and far in tables.R

p_val_close <- function(x) {
  out <- t.test(x[abs(dataset$margin) < close] ~ dataset$politician_dummy[abs(dataset$margin) < close])
  if(out$p.value > 0.1){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    return(pv)
  }
  else if(between(out$p.value, 0.05, 0.10)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "*", sep = "")
  }
  else if(between(out$p.value, 0.01, 0.05)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "**", sep = "")
  }
  else if(out$p.value < 0.01){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "***", sep = "")
  }
}

p_val_far <- function(x) {
  out <- t.test(x[abs(dataset$margin) < far] ~ dataset$politician_dummy[abs(dataset$margin) < far])
  if(out$p.value > 0.1){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    return(pv)
  }
  else if(between(out$p.value, 0.05, 0.10)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "*", sep = "")
  }
  else if(between(out$p.value, 0.01, 0.05)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "**", sep = "")
  }
  else if(out$p.value < 0.01){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "***", sep = "")
  }
}

## Get coefficient for regression tables

get_coef <- function(dataset, variable, covs = NULL, ...){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs, ...)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc <- function(dataset, variable, covs = NULL, ...){
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs, ...)
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 10e-10, 0.01)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

get_se_rob <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}


get_coef_w <- function(dataset, variable, covs = NULL){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs,
                                b = 2*b,
                                h = 2*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_w <- function(dataset, variable, covs = NULL){
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs,
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

get_se_rob_w <- function(variable){
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']])
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

mean_wealth_pols <- function(dataset, variable){
  dataset %>% 
    filter(between(margin, 0, 0.01)) %>%
    summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
    pull() %>% 
    round(3) %>%
    format(nsmall=3)
    
}

mean_wealth_nonpols <- function(dataset, variable){
  dataset %>% 
    filter(between(margin, -0.01, 0)) %>%
    summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
    pull() %>% 
    round(3) %>%
    format(nsmall=3)
}

n_pols <- function(dataset, variable, covs = NULL){
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs)
  regression_output[['N']][2]
}

n_nonpols <- function(dataset, variable, covs = NULL){
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs)
  regression_output[['N']][1]
}

# Panel B: 
get_coef_cov <- function(variable, covs, bw_mult =1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_cov <- function(variable, covs, bw_mult=1){
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster=dataset[['place_of_birth']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster = dataset[['place_of_birth']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

get_se_rob_cov <- function(variable, covs, bw_mult=1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

n_pols_cov <- function(variable, covs){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  regression_output[['N']][2]
}

n_nonpols_cov <- function(variable, covs){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  regression_output[['N']][1]
}

#bandwidth function for loop
get_coef_and_ci <- function(variable, covs = NULL, bw_mult){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  out <- list(coef = regression_output$coef[1], 
              cil = regression_output$ci[3],
              ciu = regression_output$ci[6])
  
  return(out)
}


make_covariates <- function(dataset){
  cbind(dataset$yoe, 
        dataset$howmany_before_alg,
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$age_at_election, 
        dataset$yod, 
        dataset$rec_soc,
        dataset$lifespan)
  
}

# Write a function to extract all these statistics at once
get_stats_for_partytable <- function(dataset, dv, covs = NULL){
  out1 <- rdrobust(dataset[[dv]], dataset[['margin']], covs = covs)
  
  coef <- paste(round(out1$coef[1], 3))
  se1 <- round(out1$se[2], 3)
  se2 <- round(out1$se[3], 3)
  pv1 <- out1$pv[2]
  pv2 <- out1$pv[3]
  n <- out1$N[2]
  
  if(between(pv1, 0.05, 0.1)){
    se1_out <- paste("(", se1, ")", "*", sep = "")
  } else if(between(pv1, 0.01, 0.05)){
    se1_out <- paste("(", se1, ")", "**", sep = "")
  } else if(between(pv1, 0.000001, 0.01)){
    se1_out <- paste("(", se1, ")", "***", sep = "")
  } else if(between(pv1, 0.10, 1)) {
    se1_out <- paste("(", se1, ")", sep = "")
  }
  
  if(between(pv2, 0.05, 0.1)){
    se2_out <- paste("(", se2, ")", "*", sep = "")
  } else if(between(pv2, 0.01, 0.05)){
    se2_out <- paste("(", se2, ")", "**", sep = "")
  } else if(between(pv2, 0.0000001, 0.01)){
    se2_out <- paste("(", se2, ")", "***", sep = "")
  } else if(between(pv2, 0.10, 1)) {
    se2_out <- paste("(", se2, ")", sep = "")
  }
  
  
  out <- list(coef = coef, se1_out = se1_out, se2_out = se2_out, n = n)
  
  return(out)
  
}

get_stats_withinparty <- function(dataset, dv, covs = NULL, bw = 'mserd'){
  out1 <- rdrobust(dataset[[dv]], dataset[['margin']], covs = covs, bwselect = bw)
  
  coef <- format(paste(round(out1$coef[1], 3)), nsmall = 3)
  se1 <- format(round(out1$se[2], 3), nsmall = 3)
  se2 <- format(round(out1$se[3], 3), nsmall = 3)
  pv1 <- out1$pv[2]
  pv2 <- out1$pv[3]
  n_treatment <- out1$N[2]
  n_control <- out1$N[1]
  
  if(between(pv1, 0.05, 0.1)){
    se1_out <- paste("(", se1, ")", "*", sep = "")
  } else if(between(pv1, 0.01, 0.05)){
    se1_out <- paste("(", se1, ")", "**", sep = "")
  } else if(between(pv1, 0.000001, 0.01)){
    se1_out <- paste("(", se1, ")", "***", sep = "")
  } else if(between(pv1, 0.10, 1)) {
    se1_out <- paste("(", se1, ")", sep = "")
  }
  
  if(between(pv2, 0.05, 0.1)){
    se2_out <- paste("(", se2, ")", "*", sep = "")
  } else if(between(pv2, 0.01, 0.05)){
    se2_out <- paste("(", se2, ")", "**", sep = "")
  } else if(between(pv2, 0.0000001, 0.01)){
    se2_out <- paste("(", se2, ")", "***", sep = "")
  } else if(between(pv2, 0.10, 1)) {
    se2_out <- paste("(", se2, ")", sep = "")
  }
  
  
  out <- list(coef = coef, se1_out = se1_out, se2_out = se2_out, n_treatment = n_treatment, n_control = n_control)
  
  return(out)
  
}


calc_pv <- function(diff){
  pnorm(diff, mean = 0, sd = 
          sqrt(
            readr::parse_number(get_stats_withinparty(in_party, dv = 'defw')[[2]])^2 +
              readr::parse_number(get_stats_withinparty(out_party, dv = 'defw')[[2]])^2
          )
  )
}


## For robustness table (two optimal bandwidths)

get_coef2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc2 <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.01)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

get_se_rob2 <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.01)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}


get_coef_w2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_w2 <- function(variable){
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.01)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

get_se_rob_w2 <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.01)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}


# Panel B: 
get_coef_cov2 <- function(variable, covs, bw_mult =1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs, bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_cov2 <- function(variable, covs, bw_mult=1){
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster=dataset[['place_of_birth']], covs = covs, bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster = dataset[['place_of_birth']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

get_se_rob_cov2 <- function(variable, covs, bw_mult=1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs, bwselect = 'msetwo')
  h <- regression_output[['bws']][1,]
  b <- regression_output[['bws']][2,]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}



## For tenure table
get_coef_t <- function(depvar, dataset, bw = 'mserd'){
  
    regression_output <- rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = make_covariates(dataset), bwselect = bw)
    
    coef <- regression_output['coef'][[1]][1] %>%
      round(3) %>%
      format(nsmall=3)
    
    paste(coef)
}

get_se_bc_t <- function(depvar, dataset, bw = 'mserd'){
  
  regression_output <- rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = make_covariates(dataset), bwselect = bw)
  
  se <- regression_output['se'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][1]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

get_se_rob_t <- function(depvar, dataset, bw = 'mserd'){
  
  regression_output <- rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = make_covariates(dataset), bwselect = bw)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
  
}

get_n_untreated_t <- function(dataset, depvar){
  
  regression_output <- rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = make_covariates(dataset))
  
  regression_output[['N']][1]
}

get_n_treated_t <- function(dataset, depvar){
  
  regression_output <- rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = make_covariates(dataset))
  
  regression_output[['N']][2]
}


## get_info
## get_info
get_info <- function(dataset, variable, cluster, covs = NULL, bw_mult=1, ...){
  
  # coefficient
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']],
                                cluster = dataset[['naam']], covs = covs, ...)
  
  #bandwidth multiplier
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], 
                                cluster = dataset[['naam']],
                                covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h, 
                                ...)
  
  # sample size
  n_control <- regression_output$N[1]
  n_treated <- regression_output$N[2]
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  # standard error
  se <- regression_output['se'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    out_se <- paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    out_se <- paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 10e-10, 0.01)){
    out_se <- paste("(", se, ")", "***", sep = "")
  } else {
    out_se <- paste("(", se, ")", sep = "")
  }
  
  
  # mean dv treated
  if(is.null(covs)){
    mean_treated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             !is.na(gtools::invalid(covs))) %>%
      filter(between(margin, 0.00, 0.01)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
    # mean dv untreated
    mean_untreated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             !is.na(gtools::invalid(covs))) %>%
      filter(between(margin, -0.01, 0.00)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
  } else {
    
    mean_treated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             complete.cases(covs)) %>%
      filter(between(margin, 0.00, 0.01)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
    mean_untreated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             complete.cases(covs)) %>%
      filter(between(margin, -0.02, 0.00)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
  }
  
  if(bw_mult == 1){
    bw <- "Optimal"
  } else{
    bw <- paste(bw_mult, "x Optimal", sep = "")
  }
  
  #put everything in dataframe
  data.frame(out = c(coef, out_se, mean_treated, mean_untreated, n_treated, n_control, bw))
  
}

##get_info2
## get_info
get_info2 <- function(dataset, variable, covs = NULL, bw_mult=1, ...){
  
  # coefficient
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs, ...)
  
  #bandwidth multiplier
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = dataset[[variable]], x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h, 
                                ...)
  
  # sample size
  n_control <- regression_output$N[1]
  n_treated <- regression_output$N[2]
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  # standard error
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    out_se <- paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    out_se <- paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 10e-10, 0.01)){
    out_se <- paste("(", se, ")", "***", sep = "")
  } else {
    out_se <- paste("(", se, ")", sep = "")
  }
  
  # mean dv treated
  if(is.null(covs)){
    mean_treated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             !is.na(gtools::invalid(covs))) %>%
      filter(between(margin, 0.00, 0.01)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
    # mean dv untreated
    mean_untreated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             !is.na(gtools::invalid(covs))) %>%
      filter(between(margin, -0.01, 0.00)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
  } else {
    
    mean_treated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             complete.cases(covs)) %>%
      filter(between(margin, 0.00, 0.01)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
    mean_untreated <- dataset %>%
      filter(!is.na(dataset[[variable]]),
             !is.na(dataset[['margin']]),
             complete.cases(covs)) %>%
      filter(between(margin, -0.01, 0.00)) %>%
      summarize(mean = mean(!!as.symbol(variable), na.rm=TRUE)) %>%
      pull() %>% 
      round(3) %>%
      format(nsmall=3)
    
  }
  
  if(bw_mult == 1){
    bw <- "Optimal"
  } else{
    bw <- paste(bw_mult, "x Optimal", sep = "")
  }
  
  #put everything in dataframe
  data.frame(out = c(coef, out_se, mean_treated, mean_untreated, n_treated, n_control, bw))
  
}


# now for the dynamic table
get_info_dynamic <- function(dataset, t_star = 4, covs = NULL, ...){
  
  input <- compute_itt_and_att_ext(dataset = dataset, t_star = t_star, covs = covs, ...)
  
  output_table <- tibble(
    names = c(
    "Coefficient (ITT)",
    "SE (ITT)",
    "Coefficient (ATT)",
    "SE (ATT)",
    "N Treated",
    "N Control",
    "Mean DV Treated",
    "Mean DV Control"
  ), 
  hoi = list(input[[2]][,1], 
             input[[2]][,2],
             input[[3]][,1],
             input[[3]][,2],
             input[[4]] %>% map_dbl(~ .x$N[2]),
             input[[4]] %>% map_dbl(~ .x$N[1]),
             input[[5]] %>% map_dbl(~ .x[1] %>% parse_number()),
             input[[5]] %>% map_dbl(~ .x[2] %>% parse_number())
             )
  ) %>%
  unnest_wider(hoi) 
  
  update_se_itt <- map_chr(2:ncol(output_table), ~ clean_up(output_table[1:2, .x]))
  update_se_att <- map_chr(2:ncol(output_table), ~ clean_up(output_table[3:4, .x]))
  
  output_table <- output_table %>%
    mutate(across(-1, ~ as.character(round(.x, 3))))
  
  output_table[2,-1] <- rbind(update_se_itt)
  output_table[4,-1] <- rbind(update_se_att)
  
  return(output_table)
}


clean_up <- function(twocells){
  
  # get the right level
  twocells <- twocells[[1]]
  # itt
  tvals <- twocells[1]/twocells[2]
  
  out <- case_when(between(abs(tvals), 0, 1.64) ~ paste("(", round(twocells[2], 3), ")", sep = ""),
                   between(abs(tvals), 1.64, 1.96) ~ paste("(", round(twocells[2], 3), ")", "*", sep = ""),
                   between(abs(tvals), 1.96, 2.58) ~ paste("(", round(twocells[2], 3), ")", "**", sep = ""),
                   abs(tvals) > 2.58 ~ paste("(", round(twocells[2], 3), ")", "***", sep=""))
  
  return(out)
  
}

