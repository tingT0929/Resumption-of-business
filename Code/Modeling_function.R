## Packages
library(deSolve)
library(extraDistr)
library(EnvStats)
library(truncdist)
library(tidyr)
library(ggplot2)

## Time-varying conatct rate
f_alp <- function(k, alp) {
  alp[4] * (alp[3] / (1 + exp(2 * log(99) / alp[2] * (k - alp[1] - alp[2] / 2))) + 1 - alp[3])
}

eqn <- function(time, init, para, N) {
  
  dS <- - para[1] * init[2] * init[1]  / N 
  a <- para[2] * init[2]
  b <- para[3] * init[2]
  dI <- - dS - a - b
  dH <- a
  dC <- b
  
  return(list(c(dS, dI, dC, dH)))
}

time_l <- function(k, Y, para_1, para, N, a) {
  para[1] <- para_1[k]
  out <- as.numeric(ode(y = Y[k, ], times = k:(k + 1), eqn, parms = para, N = N, atol = 1)[2,-1]) + 10^(-10)
  Lik <- sum(dtpois(floor(Y[k + 1, a:3]), out[a:3], -10 ^ (-10), b = Inf, log = T))
  
  if(is.finite(Lik) == F) {
    Lik <- -10 ^ (10)
  }
  
  return(Lik)
}

l <- function(para, Y, I, H, N, para_1, mark, a) {
  Y[,2] <- I
  Y[,4] <- H
  Y[,1] <- N - rowSums(Y[,-1]) 
  
  logLik_Y <- sapply(mark, time_l, Y = Y, para_1 = para_1,
                     para = para, N = N, a = a)
  return(sum(logLik_Y))
}

gibbs <- function(para_t, Y, N) { 
  
  para <- para_t[[1]]
  I <- para_t[[2]]
  alp <- para_t[[3]]
  H <- para_t[[4]]
  max_l <- para_t[[5]]
  
  para_1 <- f_alp(1:(nrow(Y) - 1), alp)
  
  for(j in 1:2) {
    for(i in 3) {
      para_t <- para
      para_t[i] <- rtnorm(1, para[i], 0.01, 0, Inf)
      b <- l(para_t, Y, I, H, N, para_1, 1:(nrow(Y)-1), 2)
      r <- b - max_l +
        dlnorm(1 / para_t[i], log(5.1), log(1.05), log = T) - 
        dlnorm(1 / para[i], log(5.1), log(1.05), log = T) 
      U <- log(runif(1))
      if(U < r) {
        para <- para_t
        max_l <- b
      }
    }
  }
  
  for(h in 1:2) {
    alp_t <- alp
    alp_t[2] <- rtnorm(1, alp[2], 0.01, 0, Inf)
    para_1_t <- f_alp(1:(nrow(Y) - 1), alp_t)
    b <- l(para, Y, I, H, N, para_1_t, 1:(nrow(Y) - 1), 2)
    r <- b - max_l + 
      dlnorm(alp_t[2], log(30), log(1.05), log = T) -
      dlnorm(alp[2], log(30), log(1.05), log = T) 
    U <- log(runif(1))
    if(U < r) {
      alp <- alp_t
      para_1 <- para_1_t
      max_l <- b
    }
  }
  
  for(h in 1:2) {
    alp_t <- alp
    alp_t[3] <- rtnorm(1, alp[3], 0.01, 0, 1)
    para_1_t <- f_alp(1:(nrow(Y) - 1), alp_t)
    b <- l(para, Y, I, H, N, para_1_t, 1:(nrow(Y) - 1), 2)
    r <- b - max_l
    U <- log(runif(1))
    if(U < r) {
      alp <- alp_t
      para_1 <- para_1_t
      max_l <- b
    }
  }

  for(h in 1:2) {
    alp_t <- alp
    alp_t[4] <- rtnorm(1, alp[4], 0.01, 0, Inf)
    para_1_t <- f_alp(1:(nrow(Y) - 1), alp_t)
    b <- l(para, Y, I, H, N, para_1_t, 1:(nrow(Y) - 1), 2)
    r <- b - max_l
    U <- log(runif(1))
    if(U < r) {
      alp <- alp_t
      para_1 <- para_1_t
      max_l <- b
    }
  }
  
  z <- exp(alp[4] - para[2] - para[3])
  for(h in 3) {
    for(k in 1) {
      I_t <- I
      I_t[1] <- rgamma(1, I[2] + 1, z) 
      r <- l(para, Y, I_t, H, N, para_1, 1, 2) - l(para, Y, I, H, N, para_1, 1, 2) 
      r <- r + dgamma(I[1], I[2] + 1, z, log = T) - dgamma(I_t[1], I[2] + 1, z, log = T)
      U <- log(runif(1))
      if(U < r) {
        I <- I_t
        Y[k, 2] <- I[k]
        Y[k, 1] <- N - sum(Y[k, -1])
        para[1] <- para_1[k]
        H[k + 1] <- as.numeric(ode(y = as.numeric(Y[k, ]), times = k:(k + 1), eqn, parms = para, N = N)[2, 5])
      }
    }
    Test <- runif(1)
    if(Test < 0.9) {
      
      for(k in 2:(nrow(Y)-1)) {
        I_t <- I
        I_t[k] <- rlnorm(1, log(I[k]), log(1.01))
        r <- l(para, Y, I_t, H, N, para_1, (k-1):(k), 2) - l(para, Y, I, H, N, para_1, (k-1):(k), 2) 
        r <- r + dlnorm(I[k], log(I_t[k]), log(1.01), log = T) - 
                 dlnorm(I_t[k], log(I[k]), log(1.01), log = T)
        U <- log(runif(1))
        if(U < r) {
          I <- I_t
          Y[k,2] <- I[k]
          Y[k,4] <- H[k]
          Y[k,1] <- N - sum(Y[k, -1])
          para[1] <- para_1[k]
          H[k + 1] <- as.numeric(ode(y = as.numeric(Y[k,]), times = (k):(k + 1), eqn, parms = para, N = N)[2, 5])
        }
      }
      
      for(k in (nrow(Y))) {
        I_t <- I
        Y[k - 1, 2] <- I[k - 1]
        Y[k - 1, 4] <- H[k - 1]
        Y[k - 1, 1] <- N - sum(Y[k - 1, -1])
        para[1] <- para_1[k - 1]
        
        out <- as.numeric(ode(y = as.numeric(Y[k - 1, ]), times = (k-1):(k), eqn, parms = para, N = N)[2, -1])
        I[k] <- rtpois(1, out[2], a = -10 ^ (-10), b = Inf)
      }
      
    } else {
      
      Y[1,2] <- I[1]
      Y[1,1] <- N - sum(Y[1, -1])
      mark <- 1:(nrow(Y) - 1)
      
      for(k in mark) {
        para[1] <- para_1[k]
        out <- as.numeric(ode(y = as.numeric(Y[k, ]), times = k:(k + 1), eqn, parms = para, N = N)[2, -1])
        Y[k + 1, 2] <- rtpois(1, out[2], a = 10^(-10) , b = Inf) 
        Y[k + 1, 4] <- out[4]
        Y[k + 1, 1] <- N - sum(Y[k + 1, -1]) 
      }
      
      I_t <- Y[, 2]
      H_t <- Y[, 4]
      r <- l(para, Y, I_t, H_t, N, para_1, 1:(nrow(Y) - 1), 3) - l(para, Y, I, H, N, para_1, 1:(nrow(Y) - 1), 3) 
      U <- log(runif(1))
      if(U < r) {
        I <- I_t
        H <- H_t
      }
    }
  }  
  
  max_l <- l(para, Y, I, H, N, para_1, 1:(nrow(Y) - 1), 2)
  
  return(list(para, I, alp, H, max_l))
}

para_est <- function(dat_tol, N, d) { 

  dat_tol <- dat_tol + 10 ^ (-10)
  b <- c(0.5, 1 / 9.5, 1 / 5.1)
  
  Y <- dat_tol
  I <- rep(0, nrow(dat_tol))
  I[1] <- (sum(as.numeric(Y[2, c(3)] -  Y[1, c(3)]))) / (sum(b[c(2, 3)]))
  for(k in 2:(nrow(dat_tol) - 1)) {
    Y[, 2] <- I
    I[k] <- max(max(sum(Y[k-1, 2:3]) - sum(Y[k, 3]), 1), (sum(Y[k + 1, 3]) -  sum(Y[k, 3])) / (sum(b[c(2, 3)])))
  }
  Y[, 2] <- I
  I[nrow(dat_tol)] <- max(sum(Y[nrow(dat_tol) - 1, 2:3]) - sum(Y[nrow(dat_tol), 3]), 1)
  
  Y[1, 1] <- N - sum(Y[1, -1])
  for(k in 2:(nrow(dat_tol))) {
    b[1] <- f_alp(k - 1, c(d, 30, 0, 1))
    Y[k, 4] <- as.numeric(ode(y = as.numeric(Y[k - 1, ]), times = (k-1):(k), eqn, parms = b, N = N)[2, 5])
    Y[k, 1] <- N - sum(Y[k, -1])
  }
  H <- Y[, 4]
  
  para <- list()
  para[[1]] <- list(b, I, c(d, 30, 0, 1), H, -Inf)
  
  k <- 10000
  for(i in 2:k) {
    para[[1]] <- gibbs(para[[1]], Y, N)
    cat(c(i, para[[1]][[5]], para[[1]][[1]], para[[1]][[2]][1:2], para[[1]][[3]], para[[1]][[4]][2:3]), "\n")
  }
  
  k <- 20000
  for(i in 2:k) {
    para[[i]] <- gibbs(para[[i - 1]], Y, N)
    cat(c(i, para[[i]][[5]], para[[i]][[1]], para[[i]][[2]][1:2], para[[i]][[3]], para[[i]][[4]][2:3]), "\n")
  }
  
  para <- lapply(seq(1, k, 10), function(i){para[[i]]})
  
  return(para)
}

pred <- function(para, dat_tol, N) { 
  
  pre_arr <- array(0, c(length(para), 4, 200 + nrow(dat_tol)))
  
  for(j in 1:length(para)) {
    Y <- dat_tol
    Y[,2] <- para[[j]][[2]]
    Y[,4] <- para[[j]][[4]]
    Y[,1] <- N - rowSums(Y[, -1]) 
    
    pre <- t(Y)
    
    for(k in (nrow(dat_tol) + 1):(nrow(dat_tol) + 200)) {
      a <- para[[j]][[1]]
      a[1] <- f_alp(k - 1, para[[j]][[3]])
      out <- as.numeric(ode(y = as.numeric(Y[k - 1,]), times = (k - 1):k, eqn, parms = a, N = N)[2, -1])
      pre <- cbind(pre, rep(0, 4))
      pre[-1, k] <- rtpois(3, out[-1], -10 ^ (-10), Inf) 
      pre[4, k] <- out[4]
      pre[1, k] <- N - sum(pre[-1, k])
      
      Y <- rbind(Y, rep(0, 4))
      Y[k, ] <- out
    }
    
    pre_arr[j, , ] <- pre
  }
  
  return(pre_arr)
}


TrainModel <- function(dat_tol, N, d) {
  
  para <- para_est(dat_tol, N, d)
  pre_arr <- pred(para, dat_tol, N) 
  
  # I
  I_Quan <- sapply(1:(200 + nrow(dat_tol)), function(j) {
    return(c(quantile(pre_arr[, 2, j], 0.025), quantile(pre_arr[, 2, j], 0.5), quantile(pre_arr[, 2, j], 0.975)))
  })
  
  # C
  C_Quan <- sapply(1:(200 + nrow(dat_tol)), function(j) {
    return(c(quantile(pre_arr[, 3, j], 0.025), quantile(pre_arr[, 3, j], c(0.5)), quantile(pre_arr[, 3, j], 0.975)))
  })
  
  # 治愈
  H_Quan <- sapply(1:(200 + nrow(dat_tol)), function(j) {
    a <- pre_arr[, 4, j]
    return(c(quantile(a, 0.025), quantile(a, 0.5), quantile(a, 0.975)))
  })
  
  result <- list(I_Quan = I_Quan, C_Quan = C_Quan, H_Quan = H_Quan,
                 para = para, pre_arr = pre_arr)
  
  return(list(dat = dat, N = N, result = result))
}

