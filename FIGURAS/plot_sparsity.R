library(dplyr)

n = 100000
rho = seq(0:n)/n

df = data.frame(rho = rho, density = 1 - (1-rho)^2, rhos = NA)

df$density2 = apply(df %>% select(density), 1, function(x){min(2*x, 1)})

df$rhos[(df$rho %in% c(0.1, 0.2, 0.5, 0.8))] = df$density[(df$rho %in% c(0.1, 0.2, 0.5, 0.8))]

library(ggplot2)

ggplot(df, mapping = aes(x = rho, y = density)) +
  geom_line() + theme_bw() +
  geom_point(mapping = aes(y = rhos), size = 2, col = 'blue') +
  labs(y= expression("Sparsity of "*Gamma), x = expression(rho)) +
  scale_x_continuous(breaks = (0:10)/10) +
  geom_line(mapping = aes(y = density2), linetype = 'dashed', col = 'red')

ggsave('MISC/sparsity_Gamma.pdf', p, width = 15, height = 10, units = 'cm', dpi = 300)
