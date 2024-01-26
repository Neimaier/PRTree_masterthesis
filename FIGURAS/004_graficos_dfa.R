library(dplyr)
library(ggplot2)
library(latex2exp)
set.seed(20022024)
n = 20
x = 1:n
X = arima.sim(model = list(ar = c(0.7), ma = c(0.4)), n = n)
R = cumsum(X)


df = data.frame(t = x, X=X, R=R)


p1 = ggplot(df, mapping = aes(x = t, y = X)) +
  geom_line() + geom_point() +theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  ) +
  scale_x_continuous(breaks =  1:11, minor_breaks = 1:11, limits = c(1,11)) +
  scale_y_continuous(breaks = -2:3, minor_breaks = -2:3, limits = c(-0.1,3)) +
  labs(y = latex2exp::TeX('$X_t$'))

####
p2 = ggplot(df, mapping = aes(x = t, y = R)) +
  geom_line() + geom_point() + theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  ) +
  scale_x_continuous(breaks =  1:11, minor_breaks = 1:11, limits = c(1,11)) +
  scale_y_continuous(breaks = 5*(-0:4), minor_breaks = 5*(-0:4), limits = c(0,12))+
  labs(y = latex2exp::TeX('$R_t$'))

####
df$reg1 = NA; df$reg1[1:10] = predict(lm(R~t, data = (df %>% mutate(t = t))[1:10,]))
df$reg2 = NA; df$reg2[2:11] = predict(lm(R~t, data = (df %>% mutate(t = t-1))[2:11,]))
df$reg3 = NA; df$reg3[11:20] = predict(lm(R~t, data = (df %>% mutate(t = t-10))[11:20,]))

#####
p3 = ggplot(df %>% mutate(R = ifelse(t<=11, R, NA)), mapping = aes(x = t, y = R)) +
  geom_line() + geom_point() + theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  ) +
  scale_x_continuous(breaks =  1:11, minor_breaks = 1:11, limits = c(1,11)) +
  scale_y_continuous(breaks = 5*(-0:4), minor_breaks = 5*(-0:4), limits = c(-1, 15))+
  geom_line(mapping = aes(y = reg1), linetype = 'dashed', col = 'red', size = 1) +
  geom_line(mapping = aes(y = reg2), linetype = 'dashed', col = 'blue', size = 1) +
  geom_rect(fill = "blue", col = 'blue', alpha = 0.002, xmin = 1.8, xmax = 11.2, ymin = 0, ymax = 12) +
  geom_rect(fill = "red", col = 'red', alpha = 0.002, xmin = 0.8, xmax = 10.2, ymin = -0.5, ymax = 11.5)+

  geom_text(x = 4.9, y = -1.2, label = TeX('Fitted values: $\\textbf{R}_1(1) = ( R_1(1), {R}_2(1), ..., {R}_{1+m}(1))$'), size = 3, col = 'red') +
  geom_text(x = 5.4, y = -0.75, label = '~            ~        ~              ~', size = 3, col = 'red') +


  geom_text(x = 5.9, y = 13.2, label = TeX('Fitted values: $\\textbf{R}_1(1) = ( R_1(1), {R}_2(1), ..., {R}_{1+m}(1))$'), size = 3, col = 'blue') +
  geom_text(x = 6.4, y = 13.65, label = '~            ~        ~              ~', size = 3, col = 'blue') +

  labs(y = latex2exp::TeX('$R_t$'))


p3
#####
#p4 = ggplot(df, mapping = aes(x = t, y = R)) +
#  geom_line() + geom_point() + theme_bw()+
#  theme(
#    plot.background = element_blank(),
#    panel.border = element_blank(),
#    axis.line = element_line(color = 'black')
#  ) +
#  scale_x_continuous(breaks =  1:11, minor_breaks = 0:11) +
#  scale_y_continuous(breaks = 5*(-0:4), minor_breaks = 5*(-0:4), limits = c(0,20))+
#  geom_line(mapping = aes(y = reg1), linetype = 'dashed', col = 'red', size = 1) +
#  geom_line(mapping = aes(y = reg3), linetype = 'dashed', col = 'blue', size = 1) +
#  geom_rect(fill = "blue", col = 'blue', alpha = 0.002, xmin = 10.8, xmax = 20.2, ymin = 10, ymax = 20) +
#  geom_rect(fill = "red", col = 'red', alpha = 0.002, xmin = 0.8, xmax = 10.2, ymin = -5, ymax = 10)


###
df$eps1 = df$reg1 - df$R
df$eps2 = df$reg2 - df$R
df$eps3 = df$reg3 - df$R




#####
p4 = ggplot(df, mapping = aes(x = t, y = R)) + theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  ) +
  geom_line(mapping = aes(y = eps1), col = 'red', size = 1) +
  geom_point(mapping = aes(y = eps1), col = 'red', size = 2) +
  geom_line(mapping = aes(y = eps2), col = 'blue', size = 1) +
  geom_point(mapping = aes(y = eps2), col = 'blue', size = 2) +
  scale_x_continuous(breaks =  1:11, minor_breaks = 1:11, limits = c(1,11)) +
  scale_y_continuous(breaks = -2:2, minor_breaks = -2:2, limits = c(-2, 2))+

  geom_text(x = 4.5, y = 1.6, label = TeX('Residuals: ($epsilon_1(1), epsilon_2(1), ..., epsilon_{1+m}(1)$)'), size = 4, col = 'red') +


  geom_text(x = 5.5, y = -1.5, label = TeX('Residuals: ($epsilon_2(2), epsilon_3(2), ..., epsilon_{2+m}(2)$)'), size = 4, col = 'blue') +


  geom_rect(fill = "blue", col = 'blue', alpha = 0.002, xmin = 1.8, xmax = 11.2, ymin = -1.1, ymax = 1) +
  geom_rect(fill = "red", col = 'red', alpha = 0.002, xmin = 0.8, xmax = 10.2, ymin = -1, ymax = 1.1)+
  labs(y = latex2exp::TeX('$R_t$'))

p4
#####
#p6 = ggplot(df, mapping = aes(x = t, y = R)) + theme_bw()+
#    theme(
#      plot.background = element_blank(),
#      panel.border = element_blank(),
#      axis.line = element_line(color = 'black')
#    ) +
#    geom_line(mapping = aes(y = eps1), col = 'red', size = 1) +
#    geom_point(mapping = aes(y = eps1), col = 'red', size = 2) +
#    geom_line(mapping = aes(y = eps3), col = 'blue', size = 1) +
#    geom_point(mapping = aes(y = eps3), col = 'blue', size = 2) +
#
#    scale_x_continuous(breaks =  1:11, minor_breaks = 0:11) +
#      scale_y_continuous(breaks = -2:3, minor_breaks = -2:3, limits = c(-2, 3))+
#  geom_text(x = 10.5, y = 1, label = expression(epsilon[1](9)), size = 6, col = 'red') +
#  geom_text(x = 11.5, y = 2.8, label = expression(epsilon[11](9)), size = 6, col = 'blue')
#


library(latex2exp)

p1 = p1 + theme(axis.line = element_line(size = 0.1))
p2 = p2 + theme(axis.line = element_line(size = 0.1))
p3 = p3 + theme(axis.line = element_line(size = 0.1))
p4 = p4 + theme(axis.line= element_line(size = 0.1))

p = gridExtra::grid.arrange(p1,p2,p3,p4)

ggsave('MISC/figura_exemplo_dfa.pdf',p, width = 24, height = 18, units = 'cm')
