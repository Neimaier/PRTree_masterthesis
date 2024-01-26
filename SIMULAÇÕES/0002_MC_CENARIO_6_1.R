library(foreach)
library(doParallel)
library(dplyr)
library(readr)

path1 = 'CEN6_x1_1'
path2 = 'CEN6_x2_1'

data1 <- read_csv(paste("CENARIOS/", path1, ".csv", sep = ''))[,-1] %>% as.matrix() 
data2 <- read_csv(paste("CENARIOS/", path2, ".csv", sep = ''))[,-1]  %>% as.matrix() 

#TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
# AQUI INICIALIZA COM NULL PARA PODER RBIND DEPOIS
#TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

# Set the number of cores to use (adjust as needed)
num_cores <- 4
cl <- makeCluster(num_cores)
registerDoParallel(cl)

rho_list = c(0.1, 0.2, 0.5, 0.8)




f = function(rho){
  library(dplyr)
  library(rpart)
  library(PRTree)
  ####FUNÇÃO PARA PREENCHER X
  fill_X = function(y){
    
    cbind(lag(y, 1),
          c(y[2:length(y)], NA)
          
    )
  }
  
  fill_df = function(x1, x2, s1, s2, xs1, xs2, m_list){
    x1[s1] = xs1[s1]
    x2[s2] = xs2[s2]
    
    temp = DCCA::rhodcca(x1,x2, m_list)
    return(temp)
  }
  
  fill_df_trees = function(x1, x2, s1, s2, xs1, xs2, m_list){
    x1[s1] = xs1
    x2[s2] = xs2
    
    temp = DCCA::rhodcca(x1,x2, m_list)
    return(temp)
  }
  
  
  
  dfa1_results = NULL
  dfa2_results = NULL
  dcca_results = NULL
  rhodcca_results= NULL
  
  m_list = seq(3,101,2)
  
  start = 1
  
  for(i in start:1000){
    
    set.seed(as.numeric(paste(rho*10000, i, sep= '')))
    x1 = data1[,i]
    s1 = sample(1:length(x1), rho*length(x1))
    x1miss = ifelse(1:length(x1) %in% s1, NA, x1)
    
    x2 = data2[,i]
    s2 = sample(1:length(x2), rho*length(x2))
    x2miss = ifelse(1:length(x2) %in% s2, NA, x2)
    
    
    ### Testes imputeTS
    
    x1_random = imputeTS::na_random(x1miss)
    x1_LI = imputeTS::na_interpolation(x1miss)
    x1_KMLE = imputeTS::na_kalman(x1miss)
    x1_locf = imputeTS::na_locf(x1miss)
    x1_mean = imputeTS::na_mean(x1miss)
    x1_MME = imputeTS::na_ma(x1miss)
    
    x2_random = imputeTS::na_random(x2miss)
    x2_LI = imputeTS::na_interpolation(x2miss)
    x2_KMLE = imputeTS::na_kalman(x2miss)
    x2_locf = imputeTS::na_locf(x2miss)
    x2_mean = imputeTS::na_mean(x2miss)
    x2_MME = imputeTS::na_ma(x2miss)
    
    
    ### Construindo matrizes X
    
    X1 = fill_X(x1miss)
    X1_random = fill_X(x1_random)
    X1_LI = fill_X(x1_LI)
    X1_KMLE = fill_X(x1_KMLE)
    X1_locf = fill_X(x1_locf)
    X1_mean = fill_X(x1_mean)
    X1_MME = fill_X(x1_MME)
    
    X2 = fill_X(x2miss)
    X2_random = fill_X(x2_random)
    X2_LI = fill_X(x2_LI)
    X2_KMLE = fill_X(x2_KMLE)
    X2_locf = fill_X(x2_locf)
    X2_mean = fill_X(x2_mean)
    X2_MME = fill_X(x2_MME)
    
    
    ### TESTES RPART
    
    X1_df = as.data.frame(cbind(x1, X1))
    reg1_rpart = rpart(x1~., data = X1_df[-s1,])
    x1_rpart = predict(reg1_rpart, X1_df[s1,])
    
    X2_df = as.data.frame(cbind(x2, X2))
    reg2_rpart = rpart(x2~., data = X2_df[-s2,])
    x2_rpart = predict(reg2_rpart, X2_df[s2,])
    
    ### TESTES PRTree
    
    x1_prtree = predict(pr_tree(x1[-s1], X1[-s1,]), X1[s1,])$yhat
    x1_random_prtree = predict(pr_tree(x1[-s1], X1_random[-s1,]), X1_random[s1,])$yhat
    x1_LI_prtree = predict(pr_tree(x1[-s1], X1_LI[-s1,]), X1_LI[s1,])$yhat
    x1_KMLE_prtree = predict(pr_tree(x1[-s1], X1_KMLE[-s1,]), X1_KMLE[s1,])$yhat
    x1_locf_prtree = predict(pr_tree(x1[-s1], X1_locf[-s1,]), X1_locf[s1,])$yhat
    x1_mean_prtree = predict(pr_tree(x1[-s1], X1_mean[-s1,]), X1_mean[s1,])$yhat
    x1_MME_prtree = predict(pr_tree(x1[-s1], X1_MME[-s1,]), X1_MME[s1,])$yhat
    
    
    x2_prtree = predict(pr_tree(x2[-s2], X2[-s2,]), X2[s2,])$yhat
    x2_random_prtree = predict(pr_tree(x2[-s2], X2_random[-s2,]), X2_random[s2,])$yhat
    x2_LI_prtree = predict(pr_tree(x2[-s2], X2_LI[-s2,]), X2_LI[s2,])$yhat
    x2_KMLE_prtree = predict(pr_tree(x2[-s1], X2_KMLE[-s1,]), X2_KMLE[s1,])$yhat
    x2_locf_prtree = predict(pr_tree(x2[-s2], X2_locf[-s2,]), X2_locf[s2,])$yhat
    x2_mean_prtree = predict(pr_tree(x2[-s2], X2_mean[-s2,]), X2_mean[s2,])$yhat
    x2_MME_prtree = predict(pr_tree(x2[-s2], X2_MME[-s2,]), X2_MME[s2,])$yhat
    
    
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    # AQUI EU SALVEI AS COISAS EM UM DATA.FRAME TEMPORARIO, PRA DEPOIS
    # RBIND COM O OFICIAL
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    
    temp = DCCA::rhodcca(x1,x2, m_list)
    dfa1_temp = data.frame(original = temp$F2dfa1)
    dfa2_temp = data.frame(original = temp$F2dfa2)
    dcca_temp = data.frame(original = temp$Fdcca)
    rhodcca_temp = data.frame(original = temp$rhodcca)
    
    nm = 'random'
    temp = try(fill_df(x1,x2, s1,s2, x1_random, x2_random, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'LI'
    temp = try(fill_df(x1,x2, s1,s2, x1_LI, x2_LI, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'KMLE'
    temp = try(fill_df(x1,x2, s1,s2, x1_KMLE, x2_KMLE, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'locf'
    temp = try(fill_df(x1,x2, s1,s2, x1_locf, x2_locf, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'mean'
    temp = try(fill_df(x1,x2, s1,s2, x1_mean, x2_mean, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'MME'
    temp = try(fill_df(x1,x2, s1,s2, x1_MME, x2_MME, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    nm = 'rpart'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_rpart, x2_rpart, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_prtree, x2_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'random_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_random_prtree, x2_random_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    nm = 'LI_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_LI_prtree, x2_LI_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    nm = 'KMLE_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_KMLE_prtree, x2_KMLE_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    nm = 'locf_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_locf_prtree, x2_locf_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    nm = 'mean_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_mean_prtree, x2_mean_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    nm = 'MME_prtree'
    temp = try(fill_df_trees(x1,x2, s1,s2, x1_MME_prtree, x2_MME_prtree, m_list))
    if(is.character(temp)){
      dfa1_temp[,nm]= dfa2_temp[,nm] = dcca_temp[,nm] = rhodcca_temp[,nm] = NA
    }else{
      dfa1_temp[,nm]=temp$F2dfa1; dfa2_temp[,nm]=temp$F2dfa2; dcca_temp[,nm]=temp$Fdcca; rhodcca_temp[,nm] = temp$rhodcca
    }
    
    
    dfa1_temp$iter = dfa2_temp$iter = dcca_temp$iter = rhodcca_temp$iter = i; 
    dfa1_temp$m = dfa2_temp$m = dcca_temp$m = rhodcca_temp$m = m_list
    dfa1_temp$rho = dfa2_temp$rho = dcca_temp$rho = rhodcca_temp$rho = rho
    
    dfa1_temp$method = 'dfa1'
    dfa2_temp$method = 'dfa2'
    dcca_temp$method = 'dcca'
    rhodcca_temp$method = 'rhodcca'
    
    
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    # AQUI A GENTE RBIND, SE ESTIVER NO 25 SALVA E ZERA OS DOIS DATA.FRAMES
    # OFICIAIS PRA REINICIAR O PREENCHIMENTO
    # A PRIMEIRA VEZ QUE SALVA VAI DAR UM WARNING PORQUE VAI ANEXAR O NOME
    # DAS COLUNAS NO ARQUIVO, DEPOIS ELE NÃO ADICIONA MAIS
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    dfa1_results = rbind(dfa1_results, dfa1_temp)
    dfa2_results = rbind(dfa2_results, dfa2_temp)
    dcca_results = rbind(dcca_results, dcca_temp)
    rhodcca_results = rbind(rhodcca_results, rhodcca_temp)
    
    
  }
  
  return(list(dfa1_results = dfa1_results,
              dfa2_results = dfa2_results, 
              dcca_results = dcca_results, 
              rhodcca_results = rhodcca_results))
}



results <- foreach(rho = rho_list) %dopar% {
  f(rho)
}

results_final = bind_rows(
  results[[1]]$dfa1_results,
  results[[2]]$dfa1_results,
  results[[3]]$dfa1_results,
  results[[4]]$dfa1_results,
  results[[1]]$dfa2_results,
  results[[2]]$dfa2_results,
  results[[3]]$dfa2_results,
  results[[4]]$dfa2_results,
  results[[1]]$dcca_results,
  results[[2]]$dcca_results,
  results[[3]]$dcca_results,
  results[[4]]$dcca_results,
  results[[1]]$rhodcca_results,
  results[[2]]$rhodcca_results,
  results[[3]]$rhodcca_results,
  results[[4]]$rhodcca_results
)



write_csv2(results_final, paste('SALVA/', path1, '.csv', sep = ''))