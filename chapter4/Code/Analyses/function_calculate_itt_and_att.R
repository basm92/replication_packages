# compute_itts_and_atts_function
compute_covariates <- function(dataset, names_of_vars){
  
  output <- list(length = length(names_of_vars))
  
  for (i in 1:length(names_of_vars)){
    output[[i]] <- dataset[[names_of_vars[i]]]
    
  }
  
  output <- output %>% purrr::reduce(cbind)
  return(output)
}

compute_itt_and_att <- function(dataset, t_star, covs = NULL, covs_also_for_inc = FALSE, ...){
  
  # estimate the incumbency advantages until t_star
  
  incumbency_advantages <- vector(length = t_star)
  
  for(i in (t_star+1):2){
    
    if(!is.null(covs) & covs_also_for_inc == TRUE){
      covariates <- compute_covariates(dataset, covs)
    } else{
      covariates <- NULL
    }
    
    depvar <- paste("verk_", i, "_gewonnen", sep = "")
    reg <- rdrobust::rdrobust(y = dataset[[depvar]], x = dataset[['margin']], covs = covariates)
    incumbency_advantages[i-1] <- reg$coef[1]
  }
  
  # estimate the itt's
  # fill a matrix with coefficients and standard errors
  itt <- matrix(nrow = t_star, ncol = 2)
  
  for(i in 1:t_star){

    data <- dataset %>%
      filter(hoevaak_gewonnen_verleden == i-1)
    
    if(!is.null(covs)){
      covariates <- compute_covariates(data, covs)
    } else{
      covariates <- NULL
    }
    
    reg <- rdrobust::rdrobust(y = data[['defw']], x = data[['margin']], covs = covariates, ...)
    itt[i, 1] <- reg$coef[1]
    itt[i, 2] <- reg$se[1]
  }
  
  # estimate the att's + standard errors
  att <- matrix(nrow = t_star, ncol = 2)
  att[] <- 0
  att[t_star,] <- itt[t_star,]
  
  for(i in (t_star-1):1){
    
    att[i, 1] <- itt[i,1] - sum(incumbency_advantages[(i+1):length(incumbency_advantages)]*att[(i+1):nrow(att), 1])
    att[i, 2] <- itt[i,2] + sqrt(sum(incumbency_advantages[(i+1):length(incumbency_advantages)]^2*(att[(i+1):nrow(att), 2]^2)))
  }
  
  
  return(list(incumbency_advantages, itt, att))
  
}
