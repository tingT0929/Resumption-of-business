# Load data
source('Code/Modeling_function.R')
load("Data/NY_dat.RData")
dat_tol <- dat[[1]]
N <- dat[[2]]
province <- "New York"

# Training
train_result <-  TrainModel(dat_tol, N, 8)
save(train_result, file =  paste0("Result/result_", province, ".rda"), version = 2)

# Simulation of resumption
para <- train_result$result$para
pre_arr <- train_result$result$pre_arr

source('Code/Resumption.R')
d <- 200
train_result_0601 <- deresult(para, dat_tol, d, N, 81)
train_result_0608 <- deresult(para, dat_tol, d, N, 88)
train_result_0615 <- deresult(para, dat_tol, d, N, 95)
train_result_0622 <- deresult(para, dat_tol, d, N, 102)
train_result_0629 <- deresult(para, dat_tol, d, N, 109)
result_sim <- list(train_result, train_result_0601, train_result_0608, train_result_0615, train_result_0622, train_result_0629)
save(result_sim, file = paste0("Result/result_sim_", province, ".rda"), version = 2)
