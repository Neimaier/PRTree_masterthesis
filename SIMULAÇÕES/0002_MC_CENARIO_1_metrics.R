library(foreach)
library(doParallel)
library(dplyr)
library(readr)

path1 = 'CEN1_IID1'
path2 = 'CEN1_IID2'

data1 <- read_csv(paste("CENARIOS/", path1, ".csv", sep = ''))[,-1] %>% as.matrix()
data2 <- read_csv(paste("CENARIOS/", path2, ".csv", sep = ''))[,-1]  %>% as.matrix()
source('000_function_metrics.R')
#TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
# AQUI INICIALIZA COM NULL PARA PODER RBIND DEPOIS
#TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

# Set the number of cores to use (adjust as needed)
num_cores <- 4
cl <- makeCluster(num_cores)
registerDoParallel(cl)

rho_list = c(0.1, 0.2, 0.5, 0.8)


results <- foreach(rho = rho_list) %dopar% {
  f(rho)
}




results_final = bind_rows(
  results[[1]]$eqm_x1,
  results[[2]]$eqm_x1,
  results[[3]]$eqm_x1,
  results[[4]]$eqm_x1,
  results[[1]]$eqm_x2,
  results[[2]]$eqm_x2,
  results[[3]]$eqm_x2,
  results[[4]]$eqm_x2,
  results[[1]]$mape_x1,
  results[[2]]$mape_x1,
  results[[3]]$mape_x1,
  results[[4]]$mape_x1,
  results[[1]]$mape_x2,
  results[[2]]$mape_x2,
  results[[3]]$mape_x2,
  results[[4]]$mape_x2,
  results[[1]]$bias_x1,
  results[[2]]$bias_x1,
  results[[3]]$bias_x1,
  results[[4]]$bias_x1,
  results[[1]]$bias_x2,
  results[[2]]$bias_x2,
  results[[3]]$bias_x2,
  results[[4]]$bias_x2
)


write_csv2(results_final, paste('SALVA/', path1, '_metrics.csv', sep = ''))









