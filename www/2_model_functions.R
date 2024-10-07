fun_LAP <- function(impulse, alpha, i) {
  
  if (impulse[i] == 1) {
    
    return(1 * alpha)
    
  } else {
    
    return(0)
    
  }
  
}

fun_NW <- function(beta1, NW, NW_init, LAP, beta2, St, beta3, switch_t, i) {
  
  if (i == 1) {
    
    if (switch_t[i] == 1) {
      
      return((beta1 * NW_init) - LAP[i] - (beta2 * St[i, 1]) + (beta3 * St[i-36, 1]))
      
    } else {
      
      return((beta1 * NW_init) - LAP[i] - (beta2 * St[i, 1]))
      
    } 
    
  } else {
    
    if (switch_t[i] == 1) {
      
      return((beta1 * NW[i-1]) - LAP[i] - (beta2 * St[i, 1]) + (beta3 * St[i-36, 1]))
      
    } else {
      
      return((beta1 * NW[i-1]) - LAP[i] - (beta2 * St[i, 1]))
      
    }
    
  }
  
}

fun_S <- function(gamma1, i) {
  
  S <- rbinom(1, 1, gamma1)
  
}

fun_ES <- function(delta1, ES_init, ES, delta2, S, i) {
  
  if (i == 1) {
    
    ES <- ES_init
    
    return((delta1 * ES_init) + (delta2 * S[i]))
    
  } else {
    
    ES <- ES
    
    return((delta1 * ES[i-1]) + (delta2 * S[i]))
    
  }
  
}

fun_CC <- function(epsilon1, i) {
  
  CC <- rbinom(1, 1, epsilon1)
  
}

fun_CR <- function(zeta1, CR_init, CR, zeta2, CC, i) {
  
  if (i == 1) {
    
    CR <- CR_init
    
    return((zeta1 * CR_init) + (zeta2 * CC[i]))
    
  } else {
    
    CR <- CR
    
    return((zeta1 * CR[i-1]) + (zeta2 * CC[i]))
    
  }
  
}

fun_C <- function(eta1, NW, eta2, ES, eta3, CR, eta4, eta5, St_init, St, str_scenario, i) {
  
  if (i == 1) {
    
    if (str_scenario == 0) {
      
      return((eta1 * NW[i]) + ((eta2 * ES[i]) + (eta3 * CR[i])) - (eta4 * St_init[, 3]))
      
    } else {
      
      return((eta1 * NW[i]) + ((eta2 * ES[i]) + (eta3 * CR[i])) - (eta5 * St_init[, 3]))
      
    }
    
  } else {
    
    if (str_scenario == 0) {
      
      return((eta1 * NW[i]) + (eta2 * ES[i]) + (eta3 * CR[i]) - (eta4 * St[i-1, 3]))
      
    } else {
      
      return((eta1 * NW[i]) + (eta2 * ES[i]) + (eta3 * CR[i]) - (eta5 * St[i-1, 3]))
      
    }
    
  }
  
} 

fun_PE <- function(theta1, i) {
  
  PE <- rbinom(1, 1, theta1) 
  
  transformed_PE <- ifelse(PE == 0, -1, PE)
  
  transformed_PE
  
}

fun_SE <- function(iota1, SE_init, SE, iota2, St_init, St, i) {
  
  if (i == 1) {
    
    return((iota1 * SE_init) - (iota2 * St_init[, 1]))
    
  } else {
    
    return((iota1 * SE[i-1]) - (iota2 * St[i-1, 1]))
    
  }
  
}

fun_M <- function(kappa1, SE, kappa2, transformed_PE, i) {
  
  return((kappa1 * SE[i]) + (kappa2 * transformed_PE[i])) 
  
}

fun_SP <- function(C, M, CoS, PoR, lambda1, lambda2, lambda3, i) {
  
  DS <- (lambda1 * C[i]) - ((1 - lambda1) * M[i])
  value_smok <- DS - (lambda2 * CoS[i])
  value_no_smok <- -DS
  value_reg <- -DS + (lambda3 * PoR[i])
  
  return(list(DS = DS,
              value_smok = value_smok, 
              value_no_smok = value_no_smok,
              value_reg = value_reg))

}

softmax <- function(x) {
  
  exp_x <- exp(x - max(x))
  return(exp_x / sum(exp_x))
  
}

fun_St <- function(P) {
  
  response <- sample(c(1, 2, 3), size = 1, prob = P) # 1) smoke, 2) don't smoke; 3) regulate
  
  return(response)
  
}

simulate_data <- function(params) {
  
  # initialise vectors and starting values
  LAP <- numeric(params$nTime)
  impulse <- numeric(params$nTime)
  NW <- numeric(params$nTime)
  switch_t <- c(rep(0, times = 288), rep(1, times = (params$nTime - 288)))
  St <- matrix(0, nrow = params$nTime, ncol = 3)
  S <- numeric(params$nTime)
  ES <- numeric(params$nTime)
  CC <- numeric(params$nTime)
  CR <- numeric(params$nTime)
  C <- numeric(params$nTime)
  PE <- numeric(params$nTime)
  transformed_PE <- numeric(params$nTime)
  SE <- numeric(params$nTime)
  M <- numeric(params$nTime)
  CoS <- rnorm(params$nTime, mean = 5, sd = 3)
  PoR <- rnorm(params$nTime, mean = 3, sd = 3)
  DS <- numeric(params$nTime)
  value_smok <- numeric(params$nTime)
  value_no_smok <- numeric(params$nTime)
  value_reg <- numeric(params$nTime)
  P <- vector("list", length = params$nTime)
  
  set.seed(12345)
  
  for (i in 1:params$nTime) {
    
    impulse[i] <- ifelse((i - 1) %% 288 == 0, 1, 0)
    
    LAP[i] <- fun_LAP(impulse = impulse, alpha = params$alpha, i = i)
    
    switch_t[i] <- if_else(i >= 288, 1, 0)
    
    NW[i] <- fun_NW(beta1 = params$beta1, NW_init = params$NW_init, NW = NW, LAP = LAP, beta2 = params$beta2, St = St, beta3 = params$beta3, switch_t = switch_t, i = i)
    
    if (NW[i] < 0) {
      NW[i] <- 0
    }
    
    if (NW[i] > 10) {
      NW[i] <- 10
    }
    
    S[i] <- fun_S(gamma1 = params$gamma1, i = i)
    
    ES[i] <- fun_ES(delta1 = params$delta1, ES_init = params$ES_init, ES = ES, delta2 = params$delta2, S = S, i = i)
    
    if (ES[i] < 0) {
      ES[i] <- 0
    }
    
    if (ES[i] > 10) {
      ES[i] <- 10
    }
    
    CC[i] <- fun_CC(epsilon1 = params$epsilon1, i = i)
    
    CR[i] <- fun_CR(zeta1 = params$zeta1, CR_init = params$CR_init, CR = CR, zeta2 = params$zeta2, CC = CC, i = i)
    
    if (CR[i] < 0) {
      CR[i] <- 0
    }
    
    if (CR[i] > 10) {
      CR[i] <- 10
    }
    
    C[i] <- fun_C(eta1 = params$eta1, NW = NW, eta2 = params$eta2, ES = ES, eta3 = params$eta3, CR = CR, eta4 = params$eta4, eta5 = params$eta5, St_init = params$St_init, St = St, str_scenario = params$str_scenario, i = i)
    
    if (C[i] < 0) {
      C[i] <- 0
    }
    
    if (C[i] > 10) {
      C[i] <- 10
    }
    
    transformed_PE[i] <- fun_PE(theta1 = params$theta1)
    
    SE[i] <- fun_SE(iota1 = params$iota1, SE = SE, SE_init = params$SE_init, iota2 = params$iota2, St_init = params$St_init, St = St, i = i)
    
    if (SE[i] < 0) {
      SE[i] <- 0
    }
    
    if (SE[i] > 10) {
      SE[i] <- 10
    }
    
    M[i] <- fun_M(kappa1 = params$kappa1, SE = SE, kappa2 = params$kappa2, transformed_PE = transformed_PE, i = i)
    
    if (M[i] < 0) {
      M[i] <- 0
    }
    
    if (M[i] > 10) {
      M[i] <- 10
    }
    
    result <- fun_SP(C = C, M = M, CoS = CoS, PoR = PoR, lambda1 = params$lambda1, lambda2 = params$lambda2, lambda3 = params$lambda3, i = i)
    
    DS[i] <- round(result$DS)
    value_smok[i] <- round(result$value_smok)
    value_no_smok[i] <- round(result$value_no_smok)
    value_reg[i] <- round(result$value_reg)
    
    x <- rbind(value_smok[i], value_no_smok[i], value_reg[i])
    
    probabilities <- softmax(x)
    
    P[[i]] <- probabilities
    
    P_smok <- round(probabilities[1], 3)
    
    P_no_smok <- round(probabilities[2], 3)
    
    P_reg <- round(probabilities[3], 3)

    St[i, fun_St(P = P[[i]])] <- 1
    
  }
  
  st_df <- data.frame(St)
  st_colnames <- c("smok", "no_smok", "reg")
  colnames(st_df) <- st_colnames
  
  st_t <- st_df %>%
    mutate(timepoint = row_number()) %>%  # create a time point column based on row number
    pivot_longer(cols = -timepoint, names_to = "strategy", values_to = "value") %>%
    filter(value == 1) %>%
    select(-value) %>%
    mutate(strategy = factor(strategy, levels = names(st_df))) %>%
    select(-timepoint)
  
  sim_df <- data.frame(nTime = 1:params$nTime, impulse, LAP, switch_t, NW, st_df, st_t, S, ES, CC, CR, C, transformed_PE, SE, M, P_smok, P_no_smok, P_reg)
  
  return(sim_df)
  
}

# to test that the function is working, change from commented out to actual code below and run the entire script

# params <- list(
#   nTime = 8064,
#   alpha = 0.5,
#   beta1 = 0.997,
#   beta2 = 0.02,
#   beta3 = 0.1,
#   gamma1 = 0.005,
#   delta1 = 0.98,
#   delta2 = 4,
#   epsilon1 = 0.1,
#   zeta1 = 0.95,
#   zeta2 = 4,
#   eta1 = 0.4,
#   eta2 = 0.3,
#   eta3 = 0.3,
#   eta4 = 0.1,
#   eta5 = 0.3,
#   theta1 = 0.5,
#   iota1 = 1.0015,
#   iota2 = 0.1,
#   kappa1 = 0.9,
#   kappa2 = 1.2,
#   lambda1 = 0.1,
#   lambda2 = 0.2,
#   lambda3 = 0.3,
#   str_scenario = 0,
#   NW_init = 8,
#   ES_init = 3,
#   CR_init = 3,
#   SE_init = 6,
#   St_init = matrix(0, nrow = 1, ncol = 3))
# 
# simulate_data(params = params)
