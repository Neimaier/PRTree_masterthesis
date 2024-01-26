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
m_max = 100


Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(psi_Y[1:(length(psi_Y)-h)]*psi_Y[(1+h):length(psi_Y)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  }
}

K = DCCA:::Km(m_max)




df = data.frame(rho = rep(c(0.1, 0.2, 0.5, 0.8), each = rep*100))
df = df %>% mutate(m = 100)
df$n = floor(df$rho*(df$m))
df$values = NA
df$KGamma = NA
df$pos = rep(2:101, nrow(df)/100)

for(i in 1:(nrow(df)/100)){
  
  s = sample(2:(df[(100*i),'m']+1), df[(100*i),'n'])
  
  Gamma = Gamma2
  Gamma[s,] = 0; Gamma[,s] = 0
  
  df[(100*(i-1)+1):(100*i),'values'] = diag(K%*%Gamma)[-1]
  df[(100*(i-1)+1):(100*i),'KGamma'] = diag(K%*%Gamma2)[-1]
  
}

df = df %>% mutate(rho = case_when(
  rho==0.1 ~ 'rho = 0.1',
  rho==0.2 ~ 'rho = 0.2',
  rho==0.5 ~ 'rho = 0.5',
  rho==0.8 ~ 'rho = 0.8',
  T ~ ''
))

df$pos = factor(df$pos)

p = ggplot(data = df %>% filter(values>0), mapping = aes(x = pos, y = values, group = pos)) +
  geom_boxplot(outlier.size = 0.01) + facet_wrap(~rho, ncol = 2) + 
  geom_point(mapping = aes(y = KGamma, x = pos, group = pos), col = 'red', size = 0.5)+
  theme_bw() + labs(y = latex2exp::TeX('diag(K $\\Gamma$)'), x = '') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave('KGamma_asas_AR.png', p, width = 32, height = 24, units = 'cm')


















# MA(1) ---------------------------------------------------------------
set.seed(00303315)
m_max = 100
rep = 1000
theta = c(1,0.6,rep(0, m_max - 1))
Gamma1 = Gamma2 = Gamma12 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  # MA(1)
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(theta[1:(length(theta)-h)]*theta[(1+h):length(theta)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
  } 
}


K = DCCA:::Km(m_max)




df = data.frame(rho = rep(c(0.1, 0.2, 0.5, 0.8), each = rep*100))
df = df %>% mutate(m = 100)
df$n = floor(df$rho*(df$m))
df$values = NA
df$KGamma = NA
df$pos = rep(2:101, nrow(df)/100)

for(i in 1:(nrow(df)/100)){
  
  s = sample(2:(df[(100*i),'m']+1), df[(100*i),'n'])
  
  Gamma = Gamma2
  Gamma[s,] = 0; Gamma[,s] = 0
  
  df[(100*(i-1)+1):(100*i),'values'] = diag(K%*%Gamma)[-1]
  df[(100*(i-1)+1):(100*i),'KGamma'] = diag(K%*%Gamma2)[-1]
  
}

df = df %>% mutate(rho = case_when(
  rho==0.1 ~ 'rho = 0.1',
  rho==0.2 ~ 'rho = 0.2',
  rho==0.5 ~ 'rho = 0.5',
  rho==0.8 ~ 'rho = 0.8',
  T ~ ''
))

df$pos = factor(df$pos)

p = ggplot(data = df %>% filter(values>0), mapping = aes(x = pos, y = values, group = pos)) +
  geom_boxplot(outlier.size = 0.01) + facet_wrap(~rho, ncol = 2) + 
  geom_point(mapping = aes(y = KGamma, x = pos, group = pos), col = 'red', size = 0.5)+
  theme_bw() + labs(y = latex2exp::TeX('diag(K $\\Gamma$)'), x = '') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave('KGamma_asas_MA.png', p, width = 32, height = 24, units = 'cm')














