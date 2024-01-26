library(readr)
library(dplyr)
library(ggplot2)
cen = c('cen_1', 'cen_2', 'cen_31', 'cen_32', 'cen_41', 'cen_42', 'cen_51', 'cen_52', 'cen_61', 'cen_62')


x = list.files('SALVA/', pattern = '_metrics')

for(i in 1:length(x)){
  
  nm = paste('SALVA/', x[i], sep = '')
  df = read_csv2(nm)
  
  title = gsub('_metrics.csv', '',gsub('_x1_', '', gsub('SALVA', '',nm)))
  
  df = 
    cbind(
      df %>% filter(method == 'MSE') %>% select(method, rho, x),
      df %>% filter(method == 'MSE') %>% select(locf, LI, KMLE, MME, mean, rpart, prtree) %>% stack()
    ) %>% mutate(rho1 = rho, rho = case_when(
      rho == 0.1 ~ 'rho = 0.1',
      rho == 0.2 ~ 'rho = 0.2',
      rho == 0.5 ~ 'rho = 0.5',
      rho == 0.8 ~ 'rho = 0.8',
    ),
    ind = factor(case_when(
      ind == 'locf' ~ 'LOCF',
      ind == 'KMLE' ~ 'KS',
      ind == 'MME' ~ 'EMA',
      ind == 'mean' ~ 'M',
      TRUE ~ ind), levels = c('LOCF', 'LI', 'KS', 'EMA', 'M', 'rpart', 'prtree')
    ),
    
    method = factor(case_when(
      method == 'dfa1' ~ 
    ))
    
    )
  
  
  df_ggplot = df %>% group_by(x, rho, method, ind) %>% mutate(media = mean(values, na.rm = TRUE),
                                                                     sd = sd(values, na.rm = TRUE)) %>% ungroup()
  
  df_ggplot = df_ggplot %>% mutate(ind1 = values < media +5*sd & values > media - 5*sd  ) %>% filter(ind1)
  
  p1 = ggplot(df_ggplot %>% filter(x == 'x1'), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_wrap(~rho, nrow = 1) +
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none')  +
    theme(axis.title = element_blank())
  
  p2 = ggplot(df_ggplot %>% filter(x == 'x2'), mapping = aes(x = ind, y = values, fill = ind)) +
    geom_boxplot() + facet_wrap(~rho, nrow = 1) +
    theme_bw() + labs(y = 'MSE', x = '', fill = 'Method') +theme(legend.position = 'none')  +
    theme(axis.title = element_blank())

  ggsave(paste('imagens/',cen[i], '_x1.pdf', sep = ''), p1, width = 35, height = 10, units = 'cm', dpi = 300)
  
  ggsave(paste('imagens/',cen[i], '_x2.pdf', sep = ''), p2, width = 35, height = 10, units = 'cm', dpi = 300)
  
  
}
