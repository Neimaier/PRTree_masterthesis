library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(labeling)

cen = c('cen_1', 'cen_2', 'cen_31', 'cen_32', 'cen_41', 'cen_42', 'cen_51', 'cen_52', 'cen_61', 'cen_62')

plot_func = function(df_ggplot){

  df_ggplot = df_ggplot %>% group_by(m, rho, method, ind) %>% mutate(media = mean(values, na.rm = TRUE),
                                                   iqr = IQR(values, na.rm = TRUE),
                                                   q90 = quantile(values, 0.90, na.rm = TRUE),
                                                   q10 = quantile(values, 0.10, na.rm = TRUE),
                                                   
                                                   ) %>% ungroup()
  
  df_ggplot = df_ggplot %>% mutate(ind1 = values < q90 +10*iqr & values > q10 - 10*iqr
                                     ) %>% filter(ind1)
  
  p1 = ggplot(df_ggplot %>% filter(m1 == 3), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_grid(rho~m) +
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none')  +
    geom_hline(mapping = aes(yintercept = theoretical), col = 'red') +
    theme(strip.text.y = element_blank() ,axis.title = element_blank())
  
  p2 = ggplot(df_ggplot %>% filter(m1 == 27), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_grid(rho~m) + 
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none') +
    geom_hline(mapping = aes(yintercept = theoretical), col = 'red')+
    theme(strip.text.y = element_blank(), axis.title = element_blank())
  
  p3 = ggplot(df_ggplot %>% filter(m1 == 81), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_grid(rho~m) + 
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none') +
    geom_hline(mapping = aes(yintercept = theoretical), col = 'red')+
    theme(strip.text.y = element_blank(), axis.title = element_blank())
  
  p4 = ggplot(df_ggplot %>% filter(m1 == 101), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_grid(rho~m) + 
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none') +
    geom_hline(mapping = aes(yintercept = theoretical), col = 'red')+
    theme(axis.title = element_blank())
  
  
  p = gridExtra::grid.arrange(p1, p2, p3, p4, nrow= 1)
  
  return(p)
}

f_zoom = function(df_ggplot){
  
  
  df_ggplot = df_ggplot %>% group_by(m, rho, method, ind) %>% mutate(media = mean(values, na.rm = TRUE),
                                                                     iqr = IQR(values, na.rm = TRUE),
                                                                     q90 = quantile(values, 0.90, na.rm = TRUE),
                                                                     q10 = quantile(values, 0.10, na.rm = TRUE),
                                                                     
  ) %>% ungroup()
  
  df_ggplot = df_ggplot %>% mutate(ind1 = values < q90 +10*iqr & values > q10 - 10*iqr
  ) %>% filter(ind1)
  
  p = ggplot(df_ggplot %>% filter(rho1 == 0.10), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_wrap(~m, nrow = 1) +
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none')  +
    geom_hline(mapping = aes(yintercept = theoretical), col = 'red') +
    theme(strip.text.y = element_blank() ,axis.title = element_blank())
}



x = list.files('SALVA/', pattern = '_metrics')
y = list.files('SALVA/'); y = y[!(y %in% x)]

i = 1
for(i in 1:10){
  
  nm = paste('SALVA/', y[i], sep = '')
  df = read_csv2(nm)
  
  title = gsub('_metrics.csv', '',gsub('_x1_', '', gsub('SALVA', '',nm)))
  
  
  df1 = 
    cbind(
      df %>% select(method, rho, m, theoretical),
      df %>% select(original) %>% stack()
    ) %>% filter(rho == 0.1)
  
  
  df1$ind = ifelse(df1$m %in% c(3,27,81,101), 'blue', 'black')
  df1 = df1 %>% mutate(method = factor(method, levels = c('dfa1', 'dfa2', 'dcca', 'rhodcca')))
  
  df1$method = factor(df1$method, labels = c( TeX('$F^2_{1,DFA}$'), 
                                              TeX('$F^2_{2, DFA}$'), 
                                              'F[DCCA]', 'rho[DCCA]'))
  

  df = 
    cbind(
      df %>% filter(m %in% c(3,27,81,101)) %>% select(method, rho, m, theoretical),
      df %>% filter(m %in% c(3,27,81,101)) %>% select(locf, LI, KMLE, MME, mean, rpart, prtree) %>% stack()
    ) %>% mutate(rho1 = rho, rho = case_when(
    rho == 0.1 ~ 'rho = 0.1',
    rho == 0.2 ~ 'rho = 0.2',
    rho == 0.5 ~ 'rho = 0.5',
    rho == 0.8 ~ 'rho = 0.8',
  ),
  m1 = m,
  m = factor(case_when(
    m==3 ~ 'm = 3',
    m==27 ~ 'm = 27',
    m==81 ~ 'm = 81',
    m==101 ~ 'm = 101'
  ), levels = c('m = 3', 'm = 27', 'm = 81', 'm = 101')),
  ind = factor(case_when(
    ind == 'locf' ~ 'LOCF',
    ind == 'KMLE' ~ 'KS',
    ind == 'MME' ~ 'EMA',
    ind == 'mean' ~ 'M',
    TRUE ~ ind), levels = c('LOCF', 'LI', 'KS', 'EMA', 'M', 'rpart', 'prtree')
  )
  )
  
  #df$method[df$method == 'dfa1'] = latex2exp::TeX('$F^2[1, DFA]$')
  #df$method[df$method == 'dfa2'] = latex2exp::TeX('$F^2[2, DFA]$')
  #df$method[df$method == 'dcca'] = latex2exp::TeX('$F[DCCA]$')
  #df$method[df$method == 'rhodcca'] = latex2exp::TeX('$\rho[DCCA]$')
  
  #p = plot_func(df %>% filter(method == 'dfa1'))
  #ggsave(paste('imagens/',cen[i], '_dfa1.pdf', sep = ''), p, width = 30, height = 20, units = 'cm', dpi = 300)

  #p = plot_func(df %>% filter(method == 'dfa2'))
  #ggsave(paste('imagens/',cen[i], '_dfa2.pdf', sep = ''), p, width = 30, height = 20, units = 'cm', dpi = 300)
  
  
  #p = plot_func(df %>% filter(method == 'dcca'))
  #ggsave(paste('imagens/',cen[i], '_dcca.pdf', sep = ''), p, width = 30, height = 20, units = 'cm', dpi = 300)
  
  
  #p = plot_func(df %>% filter(method == 'rhodcca'))
  #ggsave(paste('imagens/',cen[i], '_rhodcca.pdf', sep = ''), p, width = 30, height = 20, units = 'cm', dpi = 300)

  if(i == 7){
  p = f_zoom(df %>% filter(method == 'rhodcca'))
  ggsave(paste('imagens/',cen[i], '_rhodcca_rho10.pdf', sep = ''), p, width = 30, height = 10, units = 'cm', dpi = 300)
  }
  
  p5 = ggplot(data = df1) +
    geom_boxplot(mapping = aes(x = m, y = values, group = m, color = ind), outlier.size = 0.01) + 
    theme_bw() + scale_color_identity() +
    geom_line(mapping = aes(x = m, y = theoretical), col = 'red') +
    facet_wrap(~method, scales = 'free', nrow = 1, labeller = label_parsed)+
    labs(y = '', x = '')
  
  ggsave(paste('imagens/',cen[i], '_original.pdf', sep = ''), p5, width = 30, height = 10, units = 'cm', dpi = 300)
  
}








