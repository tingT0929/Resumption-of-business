pred_tran <- function(para, dat_tol, d, N, change_day) { 
  pre_arr <- array(0, c(length(para), 4, d + nrow(dat_tol)))
  
  for(j in 1:length(para)) {
    Y <- dat_tol
    Y[, 2] <- para[[j]][[2]]
    Y[, 4] <- para[[j]][[4]]
    Y[, 1] <- N - rowSums(Y[, -1]) 
    
    pre <- t(Y)
    
    if((nrow(dat_tol) + 1) <= (change_day - 1)) {
      for(k in (nrow(dat_tol) + 1):(change_day - 1)) {
        a <- para[[j]][[1]]
        a[1] <- f_alp(k - 1, para[[j]][[3]])
        out <- as.numeric(ode(y = as.numeric(Y[k - 1, ]), times = (k - 1):k, eqn, parms = a, N = N)[2, -1])
        pre <- cbind(pre, rep(0, 4))
        pre[-1, k] <- rtpois(3, out[-1], -10 ^ (-10), Inf)
        pre[1, k] <- N - sum(pre[-1, k])
        
        Y <- rbind(Y, rep(0, 4))
        Y[k, ] <- out
      }
    }
    
    for(k in (change_day):(change_day + 6)) {
      a <- para[[j]][[1]]
      a[1] <- para[[j]][[3]][4]
      out <- as.numeric(ode(y = as.numeric(Y[k - 1, ]), times = (k - 1):k, eqn, parms = a, N = N)[2, -1])
      pre <- cbind(pre, rep(0, 4))
      pre[-1, k] <- rtpois(3, out[-1], -10 ^ (-10), Inf)
      pre[1, k] <- N - sum(pre[-1, k])
      
      Y <- rbind(Y, rep(0, 4))
      Y[k, ] <- out
    }
    
    for(k in (change_day + 7):(nrow(dat_tol) + d)) {
      a <- para[[j]][[1]]
      a[1] <- f_alp(k - 1, para[[j]][[3]])
      out <- as.numeric(ode(y = as.numeric(Y[k - 1, ]), times = (k - 1):k, eqn, parms = a, N = N)[2, -1])
      pre <- cbind(pre, rep(0, 4))
      pre[-1, k] <- rtpois(3, out[-1], -10 ^ (-10), Inf)
      pre[1, k] <- N - sum(pre[-1, k])
      
      Y <- rbind(Y, rep(0, 4))
      Y[k, ] <- out
    }
    
    pre_arr[j, , ] <- pre
  }
  
  return(pre_arr)
}

deresult <- function(para, dat, d, N, change_day) {
  pre_arr <-  pred_tran(para, dat, d, N, change_day)
  
  I_Quan <- sapply(1:(d + nrow(dat)), function(j) {
    return(c(quantile(pre_arr[, 2, j], 0.025), quantile(pre_arr[, 2, j], 0.5), quantile(pre_arr[, 2, j], 0.975)))
  })
  
  C_Quan <- sapply(1:(d + nrow(dat)), function(j) {
    return(c(quantile(pre_arr[, 3, j], 0.025), quantile(pre_arr[, 3, j], 0.5), quantile(pre_arr[, 3, j], 0.975)))
  })
  
  H_Quan <- sapply(1:(d + nrow(dat)), function(j) {
    a <- pre_arr[, 4, j]
    return(c(quantile(a, 0.025), quantile(a, 0.5), quantile(a, 0.975)))
  })
  
  result <- list(I_Quan = I_Quan, 
                 C_Quan = C_Quan, 
                 H_Quan = H_Quan)
  return(list(dat = dat, N = N, result = result))
}