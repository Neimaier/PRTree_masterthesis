library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(labeling)

cen = c('cen_1', 'cen_2', 'cen_31', 'cen_32', 'cen_41', 'cen_42', 'cen_51', 'cen_52', 'cen_61', 'cen_62')

x = list.files('SALVA/', pattern = '_metrics')
y = list.files('SALVA/'); y = y[!(y %in% x)]

i = 1

nm = paste('SALVA/', y[i], sep = '')
df = read_csv2(nm)
title = gsub('_metrics.csv', '',gsub('_x1_', '', gsub('SALVA', '',nm)))

df = 
  cbind(
    df %>% select(method, rho, m, theoretical),
    df %>% select(locf) %>% stack()
  ) %>% mutate(rho1 = rho, rho = case_when(
    rho == 0.1 ~ 'rho = 0.1',
    rho == 0.2 ~ 'rho = 0.2',
    rho == 0.5 ~ 'rho = 0.5',
    rho == 0.8 ~ 'rho = 0.8',
  )) %>% filter(method == 'dfa1')


#df = df %>% mutate(m = factor(m))
  
data = df %>% select(m, rho, theoretical) %>% distinct()

df$m
  
ggplot(data = df, mapping = aes(x= m, y= values, group = m)) +
  geom_boxplot() + facet_wrap(~rho, scales = 'free', nrow = 1) +
  theme_bw() + geom_line(data = data, mapping = aes(x = m, y = theoretical, group = 1), col = 'red') +
  labs(x = '', y = '')

ggsave('imagens/cen_1_DFA_imputada_LOCF.pdf', height = 10, width = 35, units = 'cm')
