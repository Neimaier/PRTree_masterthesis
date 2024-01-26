library(readr)
library(dplyr)

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

m_max = 27
ms = seq(3, m_max, 2)

k = length(ms)

K_func = function(m){
  I = diag(m+1)
  J = matrix(1, nrow = m+1, ncol = m+1)
  J[upper.tri(J, diag = FALSE)] = 0
  D = cbind(1, 1:(m+1))
  Q = I - D%*%solve(t(D)%*%D)%*%t(D)
  
  K = round(t(J) %*% Q %*% J, 4)
  return(K)
}

K = K_func(m_max)
K = DCCA:::Km(m_max)

# AR(1) ---------------------------------------------------------------
psi_Y = polyinv.ar(ar = c(1, -0.6), m = 10000)
Gamma1 = diag(m_max+1)  # identity matrix
Gamma2 = matrix(0, ncol = m_max+1, nrow = m_max+1)
Gamma12 = matrix(0, ncol = m_max+1, nrow = m_max+1)
for(t in 1:(m_max+1)){
  for(h in 0:(m_max+1-t)){
    Gamma2[t,t+h] = sum(psi_Y[1:(length(psi_Y)-h)]*psi_Y[(1+h):length(psi_Y)])
    Gamma2[t+h,t] = Gamma2[t,t+h] 
    Gamma12[t,t+h] = psi_Y[h+1]
  }
}

####
set.seed(303315)
rho_list = c(0.1, 0.2, 0.5, 0.8)
plot = list()
cont = 0

for(rho in rho_list){
cont = cont+1

a2 = floor(max(ncol(Gamma2)/3, rho*ncol(Gamma2)))
s1 = round(seq(1,ncol(Gamma2),length.out = rho*ncol(Gamma2))); 
s2 = round(seq(1,a2,length.out = rho*ncol(Gamma2))); 

if(ncol(Gamma2)/3 > rho*ncol(Gamma2)){
  s3 = round(seq(ncol(Gamma2)/3,ncol(Gamma2)*2/3,length.out = rho*ncol(Gamma2))); 
  s4 = round(seq(ncol(Gamma2)*2/3,ncol(Gamma2),length.out = rho*ncol(Gamma2)));}
else{
  s3 = round(seq((ncol(Gamma2) - a2)/2 + 1, (ncol(Gamma2) - a2)/2 + a2,length.out = rho*ncol(Gamma2))); 
  s4 = round(seq((ncol(Gamma2) - a2) + 1, ncol(Gamma2),length.out = rho*ncol(Gamma2))); 
}


Gamma01 = Gamma02 = Gamma03 = Gamma04 = Gamma2

Gamma01[s1,] = 0; Gamma01[,s1] = 0
Gamma02[s2,] = 0; Gamma02[,s2] = 0
Gamma03[s3,] = 0; Gamma03[,s3] = 0
Gamma04[s4,] = 0; Gamma04[,s4] = 0


p1 = pheatmap::pheatmap(K%*%Gamma01, cluster_rows = FALSE, cluster_cols = FALSE);
p2 = pheatmap::pheatmap(K%*%Gamma02, cluster_rows = FALSE, cluster_cols = FALSE);
p3 = pheatmap::pheatmap(K%*%Gamma03, cluster_rows = FALSE, cluster_cols = FALSE);
p4 = pheatmap::pheatmap(K%*%Gamma04, cluster_rows = FALSE, cluster_cols = FALSE);

plot[[cont]] = gridExtra::grid.arrange(p1[[4]], p2[[4]],
                               p3[[4]],p4[[4]], ncol= 4)

}


gridExtra::grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], ncol = 1)
ggsave('KGamma_AR_rho10_exemplos.pdf', plot[[1]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_AR_rho20_exemplos.pdf', plot[[2]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_AR_rho50_exemplos.pdf', plot[[3]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_AR_rho80_exemplos.pdf', plot[[4]], width = 35, height = 10, units = 'cm')

p = pheatmap::pheatmap(K%*%Gamma2, cluster_rows = FALSE, cluster_cols = FALSE)
ggsave('KGamma_AR_exemplo.pdf', p[[4]], width = 20, height = 20, units = 'cm')

# MA(1) ---------------------------------------------------------------


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

####
set.seed(303315)
rho_list = c(0.1, 0.2, 0.5, 0.8)
plot = list()
cont = 0

for(rho in rho_list){
  cont = cont+1
  
  a2 = floor(max(ncol(Gamma2)/3, rho*ncol(Gamma2)))
  s1 = round(seq(1,ncol(Gamma2),length.out = rho*ncol(Gamma2))); 
  s2 = round(seq(1,a2,length.out = rho*ncol(Gamma2))); 
  
  if(ncol(Gamma2)/3 > rho*ncol(Gamma2)){
    s3 = round(seq(ncol(Gamma2)/3,ncol(Gamma2)*2/3,length.out = rho*ncol(Gamma2))); 
    s4 = round(seq(ncol(Gamma2)*2/3,ncol(Gamma2),length.out = rho*ncol(Gamma2)));}
  else{
  s3 = round(seq((ncol(Gamma2) - a2)/2 + 1, (ncol(Gamma2) - a2)/2 + a2,length.out = rho*ncol(Gamma2))); 
  s4 = round(seq((ncol(Gamma2) - a2) + 1, ncol(Gamma2),length.out = rho*ncol(Gamma2))); 
  }
  
  Gamma01 = Gamma02 = Gamma03 = Gamma04 = Gamma2
  
  Gamma01[s1,] = 0; Gamma01[,s1] = 0
  Gamma02[s2,] = 0; Gamma02[,s2] = 0
  Gamma03[s3,] = 0; Gamma03[,s3] = 0
  Gamma04[s4,] = 0; Gamma04[,s4] = 0
  
  p1 = pheatmap::pheatmap(K%*%Gamma01, cluster_rows = FALSE, cluster_cols = FALSE);
  p2 = pheatmap::pheatmap(K%*%Gamma02, cluster_rows = FALSE, cluster_cols = FALSE);
  p3 = pheatmap::pheatmap(K%*%Gamma03, cluster_rows = FALSE, cluster_cols = FALSE);
  p4 = pheatmap::pheatmap(K%*%Gamma04, cluster_rows = FALSE, cluster_cols = FALSE);
  
  plot[[cont]] = gridExtra::grid.arrange(p1[[4]], p2[[4]],
                                         p3[[4]],p4[[4]], ncol= 4)
  
}

gridExtra::grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], ncol = 1)

ggsave('KGamma_MA_rho10_exemplos.pdf', plot[[1]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_MA_rho20_exemplos.pdf', plot[[2]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_MA_rho50_exemplos.pdf', plot[[3]], width = 35, height = 10, units = 'cm')
ggsave('KGamma_MA_rho80_exemplos.pdf', plot[[4]], width = 35, height = 10, units = 'cm')

p = pheatmap::pheatmap(K%*%Gamma2, cluster_rows = FALSE, cluster_cols = FALSE)
ggsave('KGamma_MA_exemplo.pdf', p[[4]], width = 20, height = 20, units = 'cm')
