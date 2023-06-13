## Multiple ggplots on same page....

##  From:  https://gist.github.com/tomhopper/faa24797bb44addeba79

## Also see:   https://felixfan.github.io/stacking-plots-same-x/

## Options are to use facets in ggplot, use grid.draw + ggplotGrib,
###   or use a package like cowplot or egg.

## Some nice stuff here:
##  http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
##  https://rpkgs.datanovia.com/ggpubr/index.html


### E X A M P L E S

## Just using  grid.draw

#library(ggplot2)
#library(grid)
#library(dplyr)
#library(lubridate)

#' Create some data to play with. Two time series with the same timestamp.
df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2), series1 = rnorm(8761), series2 = rnorm(8761, 100))

#' Create the two plots.
plot1 <- df %>%
  select(DateTime, series1) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
  ylab("Red dots / m") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

plot2 <- df %>%
  select(DateTime, series2) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
  ylab("Blue drops / L") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))



#####  Using cowplot OR egg, and change sizes

# library(ggplot2) # 3.2.1
# library(dplyr)
# library(lubridate)
# library(cowplot) # 1.0.0
# library(egg) # 0.4.5

#' Create some data to play with. Two time series with the same timestamp.
df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2),
                 series1 = rnorm(8761),
                 series2 = rnorm(8761, 100))

#' Create the two plots.
plot1 <- df %>%
  select(DateTime, series1) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
  ylab("Red dots / m") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

plot2 <- df %>%
  select(DateTime, series2) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
  ylab("Blue drops / L") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

# Draw the two plot aligned vertically, with the top plot 1/3 of the height
# of the bottom plot
cowplot::plot_grid(plot1, plot2, align = "v", ncol = 1, rel_heights = c(0.25, 0.75))
egg::ggarrange(plot1, plot2, heights = c(0.25, 0.75))
#plot_grid(plot1, plot2, align = "v", ncol = 1, rel_heights = c(0.25, 0.75))
#ggarrange(plot1, plot2, heights = c(0.25, 0.75))
