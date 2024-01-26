library(readr)
library(dplyr)
library(ggplot2)

polyinv.ar <- function(ar = NULL, m = 1000){
  # ar são os coeficientes do modelo AR(p), incluindo os 
  # que são iguais a zero, caso existam.
  phi = c(ar[-1], rep(0, m - length(ar)+1))
  psi = numeric(m+1)
  psi[1] = 1 # psi_0
  for(i in 2:(m+1)) psi[i] = -sum(rev(phi[1:(i-1)])*psi[1:(i-1)])
  return(psi)
}
poly.prod <- function(a = 1,b = 1){
  
  k1 = length(a)-1
  k2 = length(b)-1
  k = k1+k2
  p = numeric(k+1)
  
  ax = rep(0,k+1); ax[1:(k1+1)] = a
  bx = rep(0,k+1); bx[1:(k2+1)] = b
  
  for(i in 0:k){
    p[i+1] = sum(ax[1:(i+1)]*bx[(i+1):1])
  }
  
  return(p)
}
K_func = function(m){
  I = diag(m+1)
  J = matrix(1, nrow = m+1, ncol = m+1)
  J[upper.tri(J, diag = FALSE)] = 0
  D = cbind(1, 1:(m+1))
  Q = I - D%*%solve(t(D)%*%D)%*%t(D)
  
  K = round(t(J) %*% Q %*% J, 4)
  return(K)
}
tr = function(m){sum(diag(m))}





# AR(1) ---------------------------------------------------------------
set.seed(00303315)
rep = 1000
psi_Y = polyinv.ar(ar = c(1, -0.6), m = 10000)

m_max = 27
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(psi_Y[1:(length(psi_Y)-h)]*psi_Y[(1+h):length(psi_Y)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  }
}
Gamma2_27 = Gamma2

m_max = 81
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(psi_Y[1:(length(psi_Y)-h)]*psi_Y[(1+h):length(psi_Y)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  }
}
Gamma2_81 = Gamma2

m_max = 101
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(psi_Y[1:(length(psi_Y)-h)]*psi_Y[(1+h):length(psi_Y)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  }
}
Gamma2_101 = Gamma2

K27 = DCCA:::Km(27); K81 = DCCA:::Km(81);
K101 = DCCA:::Km(101);



df = data.frame(rho = rep(c(0.1, 0.2, 0.5, 0.8), each = rep))
df = bind_rows(df %>% mutate(m = 27),
          df %>% mutate(m = 81),
          df %>% mutate(m = 101))

df$n = floor(df$rho*df$m)
df$values = NA

df$dfa = (df$m == 27)*tr(K27%*%Gamma2_27) + (df$m == 81)*tr(K81%*%Gamma2_81) + 
  (df$m == 101)*tr(K101%*%Gamma2_101) 

df$max_prop = NA

for(i in 1:nrow(df)){
  
  s = sample(1:(df[i,'m']+1), df[i,'n'])
  
  if(df[i, 'm'] == 27){Gamma = Gamma2_27; K = K27}
  if(df[i, 'm'] == 81){Gamma = Gamma2_81; K = K81}
  if(df[i, 'm'] == 101){Gamma = Gamma2_101; K = K101}
  
  Gamma_aux = Gamma
  Gamma[s,] = 0; Gamma[,s] = 0
  
  df[i,'values'] = tr(K%*%Gamma)
  
  df[i,'max_prop'] = max(diag(K%*%Gamma)/ diag(K%*%Gamma_aux), na.rm =TRUE)
}






df = df %>% mutate(rho = case_when(
  rho==0.1 ~ 'rho = 0.1',
  rho==0.2 ~ 'rho = 0.2',
  rho==0.5 ~ 'rho = 0.5',
  rho==0.8 ~ 'rho = 0.8',
  T ~ ''
))

df = df %>% mutate(m = factor(
  m, levels = c('27', '81', '101'), labels = c('m = 27', 'm = 81', m = '101')
))


df_text = df %>% select(rho, m, dfa) %>% distinct()


p = ggplot(data = df, mapping = aes(x = rho, y = values))+
  geom_boxplot() + theme_bw() + facet_wrap(~m, nrow = 1, scales = 'free') +
  theme_bw() + labs(y = latex2exp::TeX('tr(K $\\Gamma$)'), x = '') +
  geom_hline(mapping = aes(yintercept = dfa), col = 'red')+ 
  geom_text(data = df_text, mapping = aes(y = 1.04*dfa), label = latex2exp::TeX('E[$F^2_{DFA}(m)$]'), x = 3) +
  geom_text(data = df_text, mapping = aes(y = 1.04*dfa, label = paste('=', round(dfa,2))), x = 3.9)


ggsave('KGamma_AR_TR.pdf', plot = p, width = 30, height = 10, units = 'cm')









#MA(1) ---------------------------------------------------------------
set.seed(00303315)
rep = 1000
m_max = 27
theta = c(1,0.6,rep(0, m_max - 1))
Gamma1 = Gamma2 = Gamma12 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  # MA(1)
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(theta[1:(length(theta)-h)]*theta[(1+h):length(theta)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  } 
}
Gamma2_27 = Gamma2

m_max = 81
theta = c(1,0.6,rep(0, m_max - 1))
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  # MA(1)
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(theta[1:(length(theta)-h)]*theta[(1+h):length(theta)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  } 
}
Gamma2_81 = Gamma2

m_max = 101
theta = c(1,0.6,rep(0, m_max - 1))
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  # MA(1)
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(theta[1:(length(theta)-h)]*theta[(1+h):length(theta)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  } 
}
Gamma2_101 = Gamma2

K27 = DCCA:::Km(27); K81 = DCCA:::Km(81);
K101 = DCCA:::Km(101);



df = data.frame(rho = rep(c(0.1, 0.2, 0.5, 0.8), each = rep))
df = bind_rows(df %>% mutate(m = 27),
               df %>% mutate(m = 81),
               df %>% mutate(m = 101))

df$n = floor(df$rho*df$m)
df$values = NA

df$dfa = (df$m == 27)*tr(K27%*%Gamma2_27) + (df$m == 81)*tr(K81%*%Gamma2_81) + 
  (df$m == 101)*tr(K101%*%Gamma2_101) 

df$max_prop = NA

for(i in 1:nrow(df)){
  
  s = sample(1:(df[i,'m']+1), df[i,'n'])
  
  if(df[i, 'm'] == 27){Gamma = Gamma2_27; K = K27}
  if(df[i, 'm'] == 81){Gamma = Gamma2_81; K = K81}
  if(df[i, 'm'] == 101){Gamma = Gamma2_101; K = K101}
  
  Gamma_aux = Gamma
  Gamma[s,] = 0; Gamma[,s] = 0
  
  df[i,'values'] = tr(K%*%Gamma)
  
  df[i,'max_prop'] = max(diag(K%*%Gamma)/ diag(K%*%Gamma_aux), na.rm =TRUE)
}






df = df %>% mutate(rho = case_when(
  rho==0.1 ~ 'rho = 0.1',
  rho==0.2 ~ 'rho = 0.2',
  rho==0.5 ~ 'rho = 0.5',
  rho==0.8 ~ 'rho = 0.8',
  T ~ ''
))

df = df %>% mutate(m = factor(
  m, levels = c('27', '81', '101'), labels = c('m = 27', 'm = 81', m = '101')
))

df_text = df %>% select(rho, m, dfa) %>% distinct()


p = ggplot(data = df, mapping = aes(x = rho, y = values))+
  geom_boxplot() + theme_bw() + facet_wrap(~m, nrow = 1, scales = 'free') +
  theme_bw() + labs(y = latex2exp::TeX('tr(K $\\Gamma$)'), x = '') +
  geom_hline(mapping = aes(yintercept = dfa), col = 'red')+ 
  geom_text(data = df_text, mapping = aes(y = 1.04*dfa), label = latex2exp::TeX('E[$F^2_{DFA}(m)$]'), x = 3) +
  geom_text(data = df_text, mapping = aes(y = 1.04*dfa, label = paste('=', round(dfa,2))), x = 3.9)


ggsave('KGamma_MA_TR.pdf', plot = p, width = 30, height = 10, units = 'cm')
