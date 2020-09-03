time_change <- T
potential <- F
d <- 200

source('Code/SICH_model.R')
N <- 3590886

province <- "Connecticut"
dat_ <- read.csv("Data/us-states.csv")
a <- dat_[dat_[, 2] == province,c(1, 4)]
dat <- matrix(0, nrow(a), 6)
dat[, 3] <- a[, 2]
dat <- dat[-c(nrow(dat)), ]
dat <- dat[c((nrow(dat) - 72):nrow(dat)), ]

# Training
which.max(dat[-1, 3] - dat[-nrow(dat), 3])
train_result <-  TrainModel(dat, N = N, d, time_change, potential, F, 33, 4, 8, 28)

# Plot
plot_pred(province, train_result, Sys.Date() - 4)
plot_all(province, train_result, 200, Sys.Date())
save(train_result, file =  paste0("Result/result_", province, ".rda"), version = 2)

# Resumption
para <- train_result$result$para
pre_arr <- train_result$result$pre_arr
dat <- train_result$dat

source('Code/Resumption.R')
train_result_0601 <- deresult(para, dat, d, N, 81, pi)
train_result_0608 <- deresult(para, dat, d, N, 88, pi)
train_result_0615 <- deresult(para, dat, d, N, 95, pi)
train_result_0622 <- deresult(para, dat, d, N, 102, pi)
train_result_0629 <- deresult(para, dat, d, N, 109, pi)

result_sim <- list(train_result, train_result_0601, train_result_0608, train_result_0615, train_result_0622, train_result_0629)

save(result_sim, file = paste0("Result/result_sim_", province, ".rda"), version = 2)