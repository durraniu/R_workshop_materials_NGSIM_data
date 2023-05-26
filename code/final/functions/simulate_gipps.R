simulate_gipps <- function(resolution, dfn1,
                             xn1, vn1, xn_first, vn_first, ln1, an,
                             Vn, tau, bn_const, bcap) {
  ####### Time #############################################
  
  # Last time frame of the simulation:
  last_time <- (nrow(dfn1) - 1) * resolution
  
  # Time vector:
  Time <- seq(from = 0, to = last_time, by = resolution)
  
  # Length of the Time vector
  time_length <- length(Time)
  
  
  
  
    
  ####### Allocate Vectors ##################################
    
  # free-flow speed
  vn_ff <- rep(NA_real_, time_length)
  
  # car-following speed
  vn_cf <- rep(NA_real_, time_length)
  
  # speed
  vn <- rep(NA_real_, time_length)
  
  # position
  xn <- rep(NA_real_, time_length)
  
  # spacing
  sn <- rep(NA_real_, time_length)
  
  # speed difference
  deltav <- rep(NA_real_, time_length)
  
  # acceleration rate
  bn <- rep(NA_real_, time_length)
  
  
  
  
  ######## Initial values for Following vehicle ##################################
  
  # speed
  vn_ff[1] <- vn_first
  vn_cf[1] <- vn_first
  vn[1] <- vn_first
  
  # position
  xn[1] <- xn_first
  
  # spacing
  sn[1] <- xn1[1] - xn_first
  
  # speed difference
  deltav[1] <- vn_first - vn1[1]
  
  ###### Gipps Calculations ############################
  
  for (t in 2:(time_length - 1)) {
    
    ## free flow
    vn_ff[t] <- vn[t - 1] +
      (2.5 * an * tau * (1 - (vn[t - 1]) / Vn)) * ((0.025 + (vn[t - 1] / Vn))^(0.5))
    
    ## car following
    bcap_part_cf <- (((vn1[t - 1])^2) / bcap)
    
    vn_cf[t] <- (bn_const * tau) +
      sqrt(
        ((bn_const^2) * (tau^2)) - (bn_const * (2 * (xn1[t - 1] - ln1 - xn[t - 1]) - (vn[t - 1] * tau) - bcap_part_cf))
      )
    
    ## gipps speed
    if (is.na(vn1[t - 1])) {
      vn[t] <- vn_ff[t]
    } else {
      vn[t] <- min(vn_ff[t], vn_cf[t])
    }
    
    ### if the speed is negative, make it zero
    vn[t] <- ifelse(vn[t] < 0, 0, vn[t])
    
    ## acceleration
    bn[t - 1] <- (vn[t] - vn[t - 1]) / (resolution)
    
    ## position
    xn[t] <- xn[t - 1] + (vn[t - 1] * resolution) + (0.5 * bn[t - 1] * (resolution)^2)
    
    # spacing
    sn[t] <- xn1[t] - xn[t] - ln1
    
    # speed difference
    deltav[t] <- vn[t] - vn1[t]
  }
  # ################## Result in a dataframe ###################################
  result_dfn <- data.frame(Time, xn1, vn1, ln1, bcap, bn, xn, vn_ff, vn_cf, vn, sn, deltav)
    
   
  
  
  return(result_dfn)
}