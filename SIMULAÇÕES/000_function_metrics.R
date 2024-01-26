
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
  
  eqm1 = function(x,y,s){mean((x[s]-y[s])^2)}
  bias1 = function(x,y,s){mean((x[s]-y[s]))}
  mape1 = function(x,y,s){mean(abs((x[s]-y[s])/x[s]))}
  
  eqm = function(x,y,s){mean((x[s]-y)^2)}
  bias = function(x,y,s){mean((x[s]-y))}
  mape = function(x,y,s){mean(abs((x[s]-y)/x[s]))}
  
  eqm_x1 = NULL
  mape_x1 = NULL
  bias_x1 = NULL
  
  eqm_x2 = NULL
  mape_x2 = NULL
  bias_x2 = NULL
  
  for(i in 1:1000){
    
    
    set.seed(as.numeric(paste(rho*10000, i, sep= '')))
    
    x1 = data1[,i]
    s1 = sample(1:length(x1), rho*length(x1))
    x1miss = ifelse(1:length(x1) %in% s1, NA, x1)
    
    x2 = data2[,i]
    s2 = sample(1:length(x2), rho*length(x2))
    x2miss = ifelse(1:length(x2) %in% s2, NA, x2)
    
    
    ### Testes imputeTS
    
    
    x1_random = try(imputeTS::na_random(x1miss))
    x1_LI = try(imputeTS::na_interpolation(x1miss))
    x1_KMLE = try(imputeTS::na_kalman(x1miss))
    x1_locf = try(imputeTS::na_locf(x1miss))
    x1_mean = try(imputeTS::na_mean(x1miss))
    x1_MME = try(imputeTS::na_ma(x1miss))
    
    x2_random = try(imputeTS::na_random(x2miss))
    x2_LI = try(imputeTS::na_interpolation(x2miss))
    x2_KMLE = try(imputeTS::na_kalman(x2miss))
    x2_locf = try(imputeTS::na_locf(x2miss))
    x2_mean = try(imputeTS::na_mean(x2miss))
    x2_MME = try(imputeTS::na_ma(x2miss))
    
    ### Construindo matrizes X
    
    X1 = try(fill_X(x1miss))
    X1_random = try(fill_X(x1_random))
    X1_LI = try(fill_X(x1_LI))
    X1_KMLE = try(fill_X(x1_KMLE))
    X1_locf = try(fill_X(x1_locf))
    X1_mean = try(fill_X(x1_mean))
    X1_MME = try(fill_X(x1_MME))
    
    X2 = try(fill_X(x2miss))
    X2_random = try(fill_X(x2_random))
    X2_LI = try(fill_X(x2_LI))
    X2_KMLE = try(fill_X(x2_KMLE))
    X2_locf = try(fill_X(x2_locf))
    X2_mean = try(fill_X(x2_mean))
    X2_MME = try(fill_X(x2_MME))
    
    
    ### TESTES RPART
    
    X1_df = try(as.data.frame(cbind(x1, X1)))
    reg1_rpart = try(rpart(x1~., data = X1_df[-s1,]))
    x1_rpart = try(predict(reg1_rpart, X1_df[s1,]))
    
    X2_df = try(as.data.frame(cbind(x2, X2)))
    reg2_rpart = try(rpart(x2~., data = X2_df[-s2,]))
    x2_rpart = try(predict(reg2_rpart, X2_df[s2,]))
    
    ### TESTES PRTree
    
    x1_prtree = try(predict(pr_tree(x1[-s1], X1[-s1,]), X1[s1,])$yhat)
    x1_random_prtree = try(predict(pr_tree(x1[-s1], X1_random[-s1,]), X1_random[s1,])$yhat)
    x1_LI_prtree = try(predict(pr_tree(x1[-s1], X1_LI[-s1,]), X1_LI[s1,])$yhat)
    x1_KMLE_prtree = try(predict(pr_tree(x1[-s1], X1_KMLE[-s1,]), X1_KMLE[s1,])$yhat)
    x1_locf_prtree = try(predict(pr_tree(x1[-s1], X1_locf[-s1,]), X1_locf[s1,])$yhat)
    x1_mean_prtree = try(predict(pr_tree(x1[-s1], X1_mean[-s1,]), X1_mean[s1,])$yhat)
    x1_MME_prtree = try(predict(pr_tree(x1[-s1], X1_MME[-s1,]), X1_MME[s1,])$yhat)
    
    
    x2_prtree = try(predict(pr_tree(x2[-s2], X2[-s2,]), X2[s2,])$yhat)
    x2_random_prtree = try(predict(pr_tree(x2[-s2], X2_random[-s2,]), X2_random[s2,])$yhat)
    x2_LI_prtree = try(predict(pr_tree(x2[-s2], X2_LI[-s2,]), X2_LI[s2,])$yhat)
    x2_KMLE_prtree = try(predict(pr_tree(x2[-s2], X2_KMLE[-s2,]), X2_KMLE[s2,])$yhat)
    x2_locf_prtree = try(predict(pr_tree(x2[-s2], X2_locf[-s2,]), X2_locf[s2,])$yhat)
    x2_mean_prtree = try(predict(pr_tree(x2[-s2], X2_mean[-s2,]), X2_mean[s2,])$yhat)
    x2_MME_prtree = try(predict(pr_tree(x2[-s2], X2_MME[-s2,]), X2_MME[s2,])$yhat)
    
    
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    # AQUI EU SALVEI AS COISAS EM UM DATA.FRAME TEMPORARIO, PRA DEPOIS
    # RBIND COM O OFICIAL
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    
    temp = try(eqm1(x1, x1_random, s1))
    if(is.character(temp)){eqm_x1_temp = data.frame(random = NA)}else{
      eqm_x1_temp = data.frame(random = temp)     
    }
    
    temp = try(mape1(x1, x1_random, s1))
    if(is.character(temp)){mape_x1_temp = data.frame(random = NA)}else{
      mape_x1_temp = data.frame(random = temp)     
    }
    
    temp = try(bias1(x1, x1_random, s1))
    if(is.character(temp)){bias_x1_temp = data.frame(random = NA)}else{
      bias_x1_temp = data.frame(random = temp)     
    }
    
    
    temp = try(eqm1(x1, x1_LI, s1));if(is.character(temp)){eqm_x1_temp$LI = NA}else{eqm_x1_temp$LI = temp}  
    temp = try(mape1(x1, x1_LI, s1));if(is.character(temp)){mape_x1_temp$LI = NA}else{mape_x1_temp$LI = temp}
    temp = try(bias1(x1, x1_LI, s1));if(is.character(temp)){bias_x1_temp$LI = NA}else{bias_x1_temp$LI = temp}
    
    temp = try(eqm1(x1, x1_KMLE, s1));if(is.character(temp)){eqm_x1_temp$KMLE = NA}else{eqm_x1_temp$KMLE = temp}  
    temp = try(mape1(x1, x1_KMLE, s1));if(is.character(temp)){mape_x1_temp$KMLE = NA}else{mape_x1_temp$KMLE = temp}
    temp = try(bias1(x1, x1_KMLE, s1));if(is.character(temp)){bias_x1_temp$KMLE = NA}else{bias_x1_temp$KMLE = temp}
    
    temp = try(eqm1(x1, x1_locf, s1));if(is.character(temp)){eqm_x1_temp$locf = NA}else{eqm_x1_temp$locf = temp}  
    temp = try(mape1(x1, x1_locf, s1));if(is.character(temp)){mape_x1_temp$locf = NA}else{mape_x1_temp$locf = temp}
    temp = try(bias1(x1, x1_locf, s1));if(is.character(temp)){bias_x1_temp$locf = NA}else{bias_x1_temp$locf = temp}
    
    temp = try(eqm1(x1, x1_mean, s1));if(is.character(temp)){eqm_x1_temp$mean = NA}else{eqm_x1_temp$mean = temp}  
    temp = try(mape1(x1, x1_mean, s1));if(is.character(temp)){mape_x1_temp$mean = NA}else{mape_x1_temp$mean = temp}
    temp = try(bias1(x1, x1_mean, s1));if(is.character(temp)){bias_x1_temp$mean = NA}else{bias_x1_temp$mean = temp}
    
    temp = try(eqm1(x1, x1_MME, s1));if(is.character(temp)){eqm_x1_temp$MME = NA}else{eqm_x1_temp$MME = temp}  
    temp = try(mape1(x1, x1_MME, s1));if(is.character(temp)){mape_x1_temp$MME = NA}else{mape_x1_temp$MME = temp}
    temp = try(bias1(x1, x1_MME, s1));if(is.character(temp)){bias_x1_temp$MME = NA}else{bias_x1_temp$MME = temp}
    
    temp = try(eqm(x1, x1_rpart, s1));if(is.character(temp)){eqm_x1_temp$rpart = NA}else{eqm_x1_temp$rpart = temp}  
    temp = try(mape(x1, x1_rpart, s1));if(is.character(temp)){mape_x1_temp$rpart = NA}else{mape_x1_temp$rpart = temp}  
    temp = try(bias(x1, x1_rpart, s1));if(is.character(temp)){bias_x1_temp$rpart = NA}else{bias_x1_temp$rpart = temp} 
    
    temp = try(eqm(x1, x1_prtree, s1));if(is.character(temp)){eqm_x1_temp$prtree = NA}else{eqm_x1_temp$prtree = temp}  
    temp = try(mape(x1, x1_prtree, s1));if(is.character(temp)){mape_x1_temp$prtree = NA}else{mape_x1_temp$prtree = temp}
    temp = try(bias(x1, x1_prtree, s1));if(is.character(temp)){bias_x1_temp$prtree = NA}else{bias_x1_temp$prtree = temp}
    
    temp = try(eqm(x1, x1_LI_prtree, s1));if(is.character(temp)){eqm_x1_temp$LI_prtree = NA}else{eqm_x1_temp$LI_prtree = temp}  
    temp = try(mape(x1, x1_LI_prtree, s1));if(is.character(temp)){mape_x1_temp$LI_prtree = NA}else{mape_x1_temp$LI_prtree = temp}
    temp = try(bias(x1, x1_LI_prtree, s1));if(is.character(temp)){bias_x1_temp$LI_prtree = NA}else{bias_x1_temp$LI_prtree = temp}
    
    temp = try(eqm(x1, x1_KMLE_prtree, s1));if(is.character(temp)){eqm_x1_temp$KMLE_prtree = NA}else{eqm_x1_temp$KMLE_prtree = temp}  
    temp = try(mape(x1, x1_KMLE_prtree, s1));if(is.character(temp)){mape_x1_temp$KMLE_prtree = NA}else{mape_x1_temp$KMLE_prtree = temp}
    temp = try(bias(x1, x1_KMLE_prtree, s1));if(is.character(temp)){bias_x1_temp$KMLE_prtree = NA}else{bias_x1_temp$KMLE_prtree = temp}
    
    temp = try(eqm(x1, x1_locf_prtree, s1));if(is.character(temp)){eqm_x1_temp$locf_prtree = NA}else{eqm_x1_temp$locf_prtree = temp}  
    temp = try(mape(x1, x1_locf_prtree, s1));if(is.character(temp)){mape_x1_temp$locf_prtree = NA}else{mape_x1_temp$locf_prtree = temp}
    temp = try(bias(x1, x1_locf_prtree, s1));if(is.character(temp)){bias_x1_temp$locf_prtree = NA}else{bias_x1_temp$locf_prtree = temp}
    
    temp = try(eqm(x1, x1_mean_prtree, s1));if(is.character(temp)){eqm_x1_temp$mean_prtree = NA}else{eqm_x1_temp$mean_prtree = temp}  
    temp = try(mape(x1, x1_mean_prtree, s1));if(is.character(temp)){mape_x1_temp$mean_prtree = NA}else{mape_x1_temp$mean_prtree = temp}
    temp = try(bias(x1, x1_mean_prtree, s1));if(is.character(temp)){bias_x1_temp$mean_prtree = NA}else{bias_x1_temp$mean_prtree = temp}
    
    temp = try(eqm(x1, x1_MME_prtree, s1));if(is.character(temp)){eqm_x1_temp$MME_prtree = NA}else{eqm_x1_temp$MME_prtree = temp}  
    temp = try(mape(x1, x1_MME_prtree, s1));if(is.character(temp)){mape_x1_temp$MME_prtree = NA}else{mape_x1_temp$MME_prtree = temp}
    temp = try(bias(x1, x1_MME_prtree, s1));if(is.character(temp)){bias_x1_temp$MME_prtree = NA}else{bias_x1_temp$MME_prtree = temp}
    
    
    temp = try(eqm1(x2, x2_random, s2))
    if(is.character(temp)){eqm_x2_temp = data.frame(random = NA)}else{
      eqm_x2_temp = data.frame(random = temp)     
    }
    
    temp = try(mape1(x2, x2_random, s2))
    if(is.character(temp)){mape_x2_temp = data.frame(random = NA)}else{
      mape_x2_temp = data.frame(random = temp)     
    }
    
    temp = try(bias1(x2, x2_random, s2))
    if(is.character(temp)){bias_x2_temp = data.frame(random = NA)}else{
      bias_x2_temp = data.frame(random = temp)     
    }
    
    
    temp = try(eqm1(x2, x2_LI, s2));if(is.character(temp)){eqm_x2_temp$LI = NA}else{eqm_x2_temp$LI = temp}  
    temp = try(mape1(x2, x2_LI, s2));if(is.character(temp)){mape_x2_temp$LI = NA}else{mape_x2_temp$LI = temp}
    temp = try(bias1(x2, x2_LI, s2));if(is.character(temp)){bias_x2_temp$LI = NA}else{bias_x2_temp$LI = temp}
    
    temp = try(eqm1(x2, x2_KMLE, s2));if(is.character(temp)){eqm_x2_temp$KMLE = NA}else{eqm_x2_temp$KMLE = temp}  
    temp = try(mape1(x2, x2_KMLE, s2));if(is.character(temp)){mape_x2_temp$KMLE = NA}else{mape_x2_temp$KMLE = temp}
    temp = try(bias1(x2, x2_KMLE, s2));if(is.character(temp)){bias_x2_temp$KMLE = NA}else{bias_x2_temp$KMLE = temp}
    
    temp = try(eqm1(x2, x2_locf, s2));if(is.character(temp)){eqm_x2_temp$locf = NA}else{eqm_x2_temp$locf = temp}  
    temp = try(mape1(x2, x2_locf, s2));if(is.character(temp)){mape_x2_temp$locf = NA}else{mape_x2_temp$locf = temp}
    temp = try(bias1(x2, x2_locf, s2));if(is.character(temp)){bias_x2_temp$locf = NA}else{bias_x2_temp$locf = temp}
    
    temp = try(eqm1(x2, x2_mean, s2));if(is.character(temp)){eqm_x2_temp$mean = NA}else{eqm_x2_temp$mean = temp}  
    temp = try(mape1(x2, x2_mean, s2));if(is.character(temp)){mape_x2_temp$mean = NA}else{mape_x2_temp$mean = temp}
    temp = try(bias1(x2, x2_mean, s2));if(is.character(temp)){bias_x2_temp$mean = NA}else{bias_x2_temp$mean = temp}
    
    temp = try(eqm1(x2, x2_MME, s2));if(is.character(temp)){eqm_x2_temp$MME = NA}else{eqm_x2_temp$MME = temp}  
    temp = try(mape1(x2, x2_MME, s2));if(is.character(temp)){mape_x2_temp$MME = NA}else{mape_x2_temp$MME = temp}
    temp = try(bias1(x2, x2_MME, s2));if(is.character(temp)){bias_x2_temp$MME = NA}else{bias_x2_temp$MME = temp}
    
    temp = try(eqm(x2, x2_rpart, s2));if(is.character(temp)){eqm_x2_temp$rpart = NA}else{eqm_x2_temp$rpart = temp}  
    temp = try(mape(x2, x2_rpart, s2));if(is.character(temp)){mape_x2_temp$rpart = NA}else{mape_x2_temp$rpart = temp}  
    temp = try(bias(x2, x2_rpart, s2));if(is.character(temp)){bias_x2_temp$rpart = NA}else{bias_x2_temp$rpart = temp} 
    
    temp = try(eqm(x2, x2_prtree, s2));if(is.character(temp)){eqm_x2_temp$prtree = NA}else{eqm_x2_temp$prtree = temp}  
    temp = try(mape(x2, x2_prtree, s2));if(is.character(temp)){mape_x2_temp$prtree = NA}else{mape_x2_temp$prtree = temp}
    temp = try(bias(x2, x2_prtree, s2));if(is.character(temp)){bias_x2_temp$prtree = NA}else{bias_x2_temp$prtree = temp}
    
    temp = try(eqm(x2, x2_LI_prtree, s2));if(is.character(temp)){eqm_x2_temp$LI_prtree = NA}else{eqm_x2_temp$LI_prtree = temp}  
    temp = try(mape(x2, x2_LI_prtree, s2));if(is.character(temp)){mape_x2_temp$LI_prtree = NA}else{mape_x2_temp$LI_prtree = temp}
    temp = try(bias(x2, x2_LI_prtree, s2));if(is.character(temp)){bias_x2_temp$LI_prtree = NA}else{bias_x2_temp$LI_prtree = temp}
    
    temp = try(eqm(x2, x2_KMLE_prtree, s2));if(is.character(temp)){eqm_x2_temp$KMLE_prtree = NA}else{eqm_x2_temp$KMLE_prtree = temp}  
    temp = try(mape(x2, x2_KMLE_prtree, s2));if(is.character(temp)){mape_x2_temp$KMLE_prtree = NA}else{mape_x2_temp$KMLE_prtree = temp}
    temp = try(bias(x2, x2_KMLE_prtree, s2));if(is.character(temp)){bias_x2_temp$KMLE_prtree = NA}else{bias_x2_temp$KMLE_prtree = temp}
    
    temp = try(eqm(x2, x2_locf_prtree, s2));if(is.character(temp)){eqm_x2_temp$locf_prtree = NA}else{eqm_x2_temp$locf_prtree = temp}  
    temp = try(mape(x2, x2_locf_prtree, s2));if(is.character(temp)){mape_x2_temp$locf_prtree = NA}else{mape_x2_temp$locf_prtree = temp}
    temp = try(bias(x2, x2_locf_prtree, s2));if(is.character(temp)){bias_x2_temp$locf_prtree = NA}else{bias_x2_temp$locf_prtree = temp}
    
    temp = try(eqm(x2, x2_mean_prtree, s2));if(is.character(temp)){eqm_x2_temp$mean_prtree = NA}else{eqm_x2_temp$mean_prtree = temp}  
    temp = try(mape(x2, x2_mean_prtree, s2));if(is.character(temp)){mape_x2_temp$mean_prtree = NA}else{mape_x2_temp$mean_prtree = temp}
    temp = try(bias(x2, x2_mean_prtree, s2));if(is.character(temp)){bias_x2_temp$mean_prtree = NA}else{bias_x2_temp$mean_prtree = temp}
    
    temp = try(eqm(x2, x2_MME_prtree, s2));if(is.character(temp)){eqm_x2_temp$MME_prtree = NA}else{eqm_x2_temp$MME_prtree = temp}  
    temp = try(mape(x2, x2_MME_prtree, s2));if(is.character(temp)){mape_x2_temp$MME_prtree = NA}else{mape_x2_temp$MME_prtree = temp}
    temp = try(bias(x2, x2_MME_prtree, s2));if(is.character(temp)){bias_x2_temp$MME_prtree = NA}else{bias_x2_temp$MME_prtree = temp}
    
    temp = try(bias(x1, x1_MME_prtree, s1));if(is.character(temp)){bias_x1_temp$MME_prtree = NA}else{bias_x1_temp$MME_prtree = temp}
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    # AQUI A GENTE RBIND, SE ESTIVER NO 25 SALVA E ZERA OS DOIS DATA.FRAMES
    # OFICIAIS PRA REINICIAR O PREENCHIMENTO
    # A PRIMEIRA VEZ QUE SALVA VAI DAR UM WARNING PORQUE VAI ANEXAR O NOME
    # DAS COLUNAS NO ARQUIVO, DEPOIS ELE NÃO ADICIONA MAIS
    #TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    eqm_x1_temp$rho = rho; eqm_x1_temp$method = 'MSE'; eqm_x1_temp$iter = i; eqm_x1_temp$x = 'x1'
    mape_x1_temp$rho = rho; mape_x1_temp$method = 'MAPE'; mape_x1_temp$iter = i; mape_x1_temp$x = 'x1'
    bias_x1_temp$rho = rho; bias_x1_temp$method = 'BIAS'; bias_x1_temp$iter = i; bias_x1_temp$x = 'x1'
    
    eqm_x2_temp$rho = rho; eqm_x2_temp$method = 'MSE'; eqm_x2_temp$iter = i; eqm_x2_temp$x = 'x2' 
    mape_x2_temp$rho = rho; mape_x2_temp$method = 'MAPE'; mape_x2_temp$iter = i; mape_x2_temp$x = 'x2'
    bias_x2_temp$rho = rho; bias_x2_temp$method = 'BIAS'; bias_x2_temp$iter = i; bias_x2_temp$x = 'x2'
    
    eqm_x1 = rbind(eqm_x1, eqm_x1_temp)
    mape_x1 = rbind(mape_x1, mape_x1_temp)
    bias_x1 = rbind(bias_x1, bias_x1_temp)
    
    eqm_x2 = rbind(eqm_x2, eqm_x2_temp)
    mape_x2 = rbind(mape_x2, mape_x2_temp)
    bias_x2 = rbind(bias_x2, bias_x2_temp)
  }
  
  return(list(eqm_x1 = eqm_x1,
              mape_x1 = mape_x1,
              bias_x1 = bias_x1,
              eqm_x2 = eqm_x2, 
              mape_x2 = mape_x2,
              bias_x2 = bias_x2))
}
