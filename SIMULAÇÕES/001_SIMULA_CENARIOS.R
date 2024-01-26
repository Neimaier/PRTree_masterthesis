library(dplyr)

m_list = seq(3,101, 2)
dm = length(m_list)
n = 2000
rep = 1000

### Cenário 1 - IID independentes entre si

set.seed(3033151)

df1  = matrix(rnorm(n*rep), ncol = rep) %>% as.data.frame()
df2 = matrix(rnorm(n*rep), ncol = rep) %>% as.data.frame()

write.csv(df1, 'CENARIOS/CEN1_IID1.csv')
write.csv(df2, 'CENARIOS/CEN1_IID2.csv')


### Cenário 2 - acf não nula mas independentes entre si
# X1(t) = 0.6*X(t-1) + eps1(t)
# X2(t) = eps2(t) + 0.6*eps(t-1)

set.seed(3033152)

df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()

for(r in 1:rep){
  x = numeric(n+10)
  y = numeric(n+10)
  eps1 = rnorm(n+10)
  eps2 = rnorm(n+10)
  x[1] = eps1[1]
  y[1] = eps2[1]
  for(i in 2:(n+10)){
    x[i] = 0.6*x[i-1] + eps1[i]
    y[i] = eps2[i] + 0.6*eps2[i-1]
  }
  df1[,r] = x[-c(1:10)]
  df2[,r] = y[-c(1:10)]
}


write.csv(df1, 'CENARIOS/CEN2_x1.csv')
write.csv(df2, 'CENARIOS/CEN2_x2.csv')


### Cenário 3 - IID dependentes entre si
# Sinal + ruido

set.seed(30331531)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
for(r in 1:rep){
  x = rnorm(n)
  y = 2*x+3+rnorm(n,sd=2)
  df1[,r] = x
  df2[,r] = y
}
write.csv(df1, 'CENARIOS/CEN3_x1_sd2.csv')
write.csv(df2, 'CENARIOS/CEN3_x2_sd2.csv')


set.seed(30331532)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
for(r in 1:rep){
  x = rnorm(n)
  y = 2*x+3+rnorm(n,sd=8)
  df1[,r] = x
  df2[,r] = y
}
write.csv(df1, 'CENARIOS/CEN3_x1_sd8.csv')
write.csv(df2, 'CENARIOS/CEN3_x2_sd8.csv')





### Cenário 4 - Normal bivariada
# correlação 0.5

set.seed(30331541)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
for(r in 1:rep){
  vec5 = MASS::mvrnorm(n=n,mu=c(0,0), Sigma=matrix(0.5, ncol = 2, nrow = 2) + diag(0.5, 2))
  x = vec5[,1]
  y = vec5[,2]
  df1[,r] = x
  df2[,r] = y
}

write.csv(df1, 'CENARIOS/CEN4_x1_cor50.csv')
write.csv(df2, 'CENARIOS/CEN4_x2_cor50.csv')


set.seed(30331542)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
for(r in 1:rep){
  vec5 = MASS::mvrnorm(n=n,mu=c(0,0), Sigma=matrix(0.8, ncol = 2, nrow = 2) + diag(0.2, 2))
  x = vec5[,1]
  y = vec5[,2]
  df1[,r] = x
  df2[,r] = y
}

write.csv(df1, 'CENARIOS/CEN4_x1_cor80.csv')
write.csv(df2, 'CENARIOS/CEN4_x2_cor80.csv')




### Cenário 5 - Correlação cruzada não nula
# Estrutra MA(20)
thetas=(20:1)/10

set.seed(30331551)

df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()

for(r in 1:rep){
  x = rnorm(n+length(thetas))
  y = is.numeric(n+length(thetas))
  for(i in (length(thetas)+1):(n+length(thetas))){
    y[i]=x[i]+sum(thetas*x[(i-1):(i-length(thetas))])
  }
 df1[,r] = x[-(1:length(thetas))]
 df2[,r] = y[-(1:length(thetas))]
  
}


write.csv(df1, 'CENARIOS/CEN5_x1_MA20.csv')
write.csv(df2, 'CENARIOS/CEN5_x2_MA20.csv')

set.seed(30331552)

df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()

for(r in 1:rep){
  x = rnorm(n+10)
  y = is.numeric(n+10)
  y[1] = x[1]
  for(i in 2:(n+10)){
    y[i] = 0.6*y[i-1] + x[i] 
  }
  df1[,r] = x[-(1:10)]
  df2[,r] = y[-(1:10)]
  
}


write.csv(df1, 'CENARIOS/CEN5_x1_AR1.csv')
write.csv(df2, 'CENARIOS/CEN5_x2_AR1.csv')




#
# Cen?rio 6 - dois AR's e dois MA's com o mesmo erro
# 	* gerar n(t) ~ N(0,1)                (aqui temos uma sequencia i.i.d)
# 	* gerar e(t) = 0.7*e(t-1) + n(t)     (aqui temos uma AR(1))
#
# - Modelo NOVO 1: ? um modelo onde cada vari?vel segue um AR(2)
#   e correla??o cruzada tem estrutura de um model AR(infty).	
# 	---------------------------------------------------------
# 	* gerar X(t) = 0.2*X(t-1) + e(t)     (usando essa estrutura recaimos em um AR(2))
# 	* gerar Y(t) = 0.6*Y(t-1) + e(t)     
# 	---------------------------------------------------------

set.seed(30331561)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()

burnEps =  50 # quantidade de valores que serão jogados fora no in?cio da s?rie e(t)
burnXY =  10  # quantidade de valores que serão jogados fora no in?cio das s?ries X(t) e Y(t)

for(r in 1:rep){
  
  ##################################
  #
  #  c?digo para gerar e(t)
  #
  ##################################
  nt = rnorm(n+burnEps+burnXY)
  et = numeric(n+burnEps+burnXY)
  et[1] = nt[1]
  for(i in 2:(n+burnEps+burnXY)) et[i] = 0.7*et[i-1] + nt[i]
  # removendo os valores iniciais e ficando apenas com os "n+burnXY" ?ltimos	
  et = et[-c(1:burnEps)]
  
  ##################################
  #
  #  c?digo para gerar X(t) e Y(t)
  #
  ##################################	
  X = is.numeric(n + burnXY)	
  Y = is.numeric(n + burnXY)
  X[1] = et[1]
  Y[1] = et[1]
  for(i in 2:(n+burnXY)){
    X[i]= 0.2*X[i-1] + et[i]
    Y[i]= 0.6*Y[i-1] + et[i]
  }
  # removendo os valores iniciais e ficando apenas com os "n" ?ltimos	
  df1[,r] = X[-c(1:burnXY)]
  df2[,r] = Y[-c(1:burnXY)]
  
}

write.csv(df1, 'CENARIOS/CEN6_x1_1.csv')
write.csv(df2, 'CENARIOS/CEN6_x2_1.csv')

#
# - Modelo NOVO 2: ? um modelo onde cada vari?vel segue um ARMA(1,1)
#   e correla??o cruzada tem estrutura de um model AR(infty).	
# 	---------------------------------------------------------
# 	* gerar X(t) = e(t) + 0.2*e(t-1)     (usando essa estrutura recaimos em um ARMA(1,1))
# 	* gerar Y(t) = e(t) + 0.6*e(t-1)     
# 	---------------------------------------------------------
#
# OBS: note que se e(t) fosse um ru?do branco, X(t) e Y(t) seriam processos AR(1) ou MA(1),
#      mas como e(t) ? um AR(1) conseguimos, ap?s algumas manipula??es alg?bricas,
# 	   recair em um modelo AR(2) no caso 1 e em um ARMA(1,1) no caso 2.


set.seed(30331562)
df1  = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()
df2 = matrix(NA, ncol = rep, nrow = n) %>% as.data.frame()




for(r in 1:rep){
  
  ##################################
  #
  #  c?digo para gerar e(t)
  #
  ##################################
  nt = rnorm(n+burnEps+burnXY)
  et = numeric(n+burnEps+burnXY)
  et[1] = nt[1]
  for(i in 2:(n+burnEps+burnXY)) et[i] = 0.7*et[i-1] + nt[i]
  # removendo os valores iniciais e ficando apenas com os "n+burnXY" ?ltimos	
  et = et[-c(1:burnEps)]
  
  ##################################
  #
  #  c?digo para gerar X(t) e Y(t)
  #
  ##################################	
  X = is.numeric(n + burnXY)	
  Y = is.numeric(n + burnXY)
  X[1] = et[1]
  Y[1] = et[1]
  for(i in 2:(n+burnXY)){
    X[i] = et[i] + 0.2*et[i-1] 
    Y[i] = et[i] + 0.6*et[i-1] 
  }
  # removendo os valores iniciais e ficando apenas com os "n" ?ltimos	
  df1[,r] = X[-c(1:burnXY)]
  df2[,r] = Y[-c(1:burnXY)]
  
}


write.csv(df1, 'CENARIOS/CEN6_x1_2.csv')
write.csv(df2, 'CENARIOS/CEN6_x2_2.csv')