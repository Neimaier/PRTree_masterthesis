library(PRTree)
library(rpart)
library(ggplot2)
library(ggtext)

set.seed(1234)
X = matrix(runif(200, 0, 10), ncol = 1)
eps = matrix(rnorm(200, 0, 0.05), ncol = 1)
y =  matrix(cos(X) + eps, ncol = 1)

reg = pr_tree(y, X, max_terminal_nodes = 9)


plot(y, reg$yhat)

reg2 = rpart(y~X)

o = order(X)

df = data.frame(x = X[o], y = y[o], CART = predict(reg2)[o], PRTree = reg$yhat[o])

p = ggplot(data = df, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y), size = 0.50) +
  geom_line(mapping = aes(y = CART, color = 'CART')) +
  geom_line(mapping = aes(y = PRTree, color = 'PRTree'), lwd = 1) +
  theme_bw() +
  scale_color_manual(name = latex2exp::TeX("$Y = cos(X) + \\epsilon  $ "), values = c("PRTree" = "darkblue", "CART" = "red"))+
  theme(legend.position = 'top',
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour ="black"),
        text = element_text(size=18)
  ) +
 scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1)) +
 scale_x_continuous(breaks = round(1:6*pi/2,4),
                    labels = c(latex2exp::TeX('$\\pi/2$'),latex2exp::TeX('$\\pi$'),
                               latex2exp::TeX('3$\\pi/2$'),latex2exp::TeX('$2\\pi$'),
                               latex2exp::TeX('5$\\pi/2$'),latex2exp::TeX('$3\\pi$')
                               ))



p





mean((df$y-df$CART)^2)
mean((df$y-df$PRTree)^2)

a = matrix(seq(10/200,10, 10/200), ncol = 1)



ggsave('MISC/cosx.pdf', width = 30, height = 10, units = 'cm', dpi = 500)


library(dplyr)

reg = pr_tree(y, X, max_terminal_nodes = 2)

cutpoints = unique(reg$nodes_matrix_info[,'cutpoints'])

df = data.frame(x = X[o], y = y[o], PRTree = reg$yhat[o])

p2 = ggplot(data = df, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y), size = 0.50) +
  geom_line(mapping = aes(y = PRTree, color = 'PRTree')) +
  geom_vline(xintercept = cutpoints, linetype = 'dashed', col = 'red')+
  theme_bw() +
  scale_color_manual(name = "f(X) = cos(X) + e", values = c("PRTree" = "darkblue", "CART" = "red"))+
  theme(legend.position = 'top',
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour ="black")
  ) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1)) +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = round(1:6*pi*1/2, 2),
                     labels = c(latex2exp::TeX('$\\pi/2$'),latex2exp::TeX('$\\pi$'),
                                latex2exp::TeX('3$\\pi/2$'),latex2exp::TeX('$2\\pi$'),
                                latex2exp::TeX('5$\\pi/2$'),latex2exp::TeX('$3\\pi$'))) +
  ggtitle('2 Regions')+
  theme(plot.title = element_text(hjust = 0.5, size = 10), text = element_text(size=12))


#ggsave('cosx_2r.png', width = 15, height = 10, units = 'cm', dpi = 500)

reg = pr_tree(y, X, max_terminal_nodes = 3)

cutpoints = unique(reg$nodes_matrix_info[,'cutpoints'])

df = data.frame(x = X[o], y = y[o], PRTree = reg$yhat[o])

p3 = ggplot(data = df, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y), size = 0.50) +
  geom_line(mapping = aes(y = PRTree, color = 'PRTree')) +
  geom_vline(xintercept = cutpoints, linetype = 'dashed', col = 'red')+
  theme_bw() +
  scale_color_manual(name = "f(X) = cos(X) + e", values = c("PRTree" = "darkblue", "CART" = "red"))+
  theme(legend.position = 'top',
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour ="black")
  ) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1)) +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = round(1:6*pi*1/2, 2),
                     labels = c(latex2exp::TeX('$\\pi/2$'),latex2exp::TeX('$\\pi$'),
                                latex2exp::TeX('3$\\pi/2$'),latex2exp::TeX('$2\\pi$'),
                                latex2exp::TeX('5$\\pi/2$'),latex2exp::TeX('$3\\pi$'))) +
  ggtitle('3 Regions')+
  theme(plot.title = element_text(hjust = 0.5, size = 10), text = element_text(size=12))


#ggsave('cosx_3r.png', width = 15, height = 10, units = 'cm', dpi = 500)

reg = pr_tree(y, X, max_terminal_nodes = 4)

cutpoints = unique(reg$nodes_matrix_info[,'cutpoints'])

df = data.frame(x = X[o], y = y[o], PRTree = reg$yhat[o])

p4 = ggplot(data = df, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y), size = 0.50) +
  geom_line(mapping = aes(y = PRTree, color = 'PRTree')) +
  geom_vline(xintercept = cutpoints, linetype = 'dashed', col = 'red')+
  theme_bw() +
  scale_color_manual(name = "f(X) = cos(X) + e", values = c("PRTree" = "darkblue", "CART" = "red"))+
  theme(legend.position = 'top',
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour ="black")
  ) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1)) +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = round(1:6*pi*1/2, 2),
                     labels = c(latex2exp::TeX('$\\pi/2$'),latex2exp::TeX('$\\pi$'),
                                latex2exp::TeX('3$\\pi/2$'),latex2exp::TeX('$2\\pi$'),
                                latex2exp::TeX('5$\\pi/2$'),latex2exp::TeX('$3\\pi$'))) +
  ggtitle('4 Regions')+
  theme(plot.title = element_text(hjust = 0.5, size = 10), text = element_text(size=12))


#ggsave('cosx_4r.png', width = 15, height = 10, units = 'cm', dpi = 500)

reg = pr_tree(y, X, max_terminal_nodes = 5)

cutpoints = unique(reg$nodes_matrix_info[,'cutpoints'])

df = data.frame(x = X[o], y = y[o], PRTree = reg$yhat[o])

p5 = ggplot(data = df, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y), size = 0.50) +
  geom_line(mapping = aes(y = PRTree, color = 'PRTree')) +
  geom_vline(xintercept = cutpoints, linetype = 'dashed', col = 'red')+
  theme_bw() +
  scale_color_manual(name = "f(X) = cos(X) + e", values = c("PRTree" = "darkblue", "CART" = "red"))+
  theme(legend.position = 'top',
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour ="black")
  ) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1)) +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = round(1:6*pi*1/2, 2),
                     labels = c(latex2exp::TeX('$\\pi/2$'),latex2exp::TeX('$\\pi$'),
                                latex2exp::TeX('3$\\pi/2$'),latex2exp::TeX('$2\\pi$'),
                                latex2exp::TeX('5$\\pi/2$'),latex2exp::TeX('$3\\pi$'))) +
  ggtitle('5 Regions')+
  theme(plot.title = element_text(hjust = 0.5, size = 10), text = element_text(size=12))
##ggsave('cosx_5r.png', width = 15, height = 10, units = 'cm', dpi = 500)

g = gridExtra::arrangeGrob(p2,p3,p4,p5)

ggsave('MISC/grid_2_5_regions.pdf', plot = g, width = 20, height = 10, units = 'cm', dpi = 500)


