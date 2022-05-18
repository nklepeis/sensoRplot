library(ggplot2)
library(grid)


## ... trying to plot ractangles in a viewport /
#  with options of filling the viewport or draswing in specific regions.
#  i.e., using user npc units relative [0,1] in each direction...

## Check this out for ideas
#   https://www.stat.auckland.ac.nz/~paul/Reports/gggrid/gggrid.html

geom_rect2 <- function(...) {

  #print(current.viewport)

  ylim <- current.viewport()$yscale
  xlim <- current.viewport()$xscale
  fullheight <- diff(range(ylim))

  # geom_rect(data = data.frame(xleft=xlim[1], xright=xlim[2],
  #                             ybottom=ylim[1], ytop=ylim[2]),
  #           aes(xmin=xleft, xmax=xright, ymin=ybottom, ymax=ytop),
  #           fill="blue", alpha=0.3)

  geom_text( data = data.frame(x = c(2,4), y=c(20,40),
                               label=c(paste(xlim, collapse=":"),
                                       paste(ylim, collapse=":"))
  ), aes(x=x, y=y, label=label))
  #geom_rect(xmin=xlim[1], xmax=6, ymin=min(ylim), ymax=40,
  #         fill="blue", alpha=0.3)
  #labs(title="OK2", subtitle=paste(ylim, collapse=":"))

}


d <- data.frame(A=c(1,3,4,5,6,7,8),B=c(10,45,12,67,12,34,67),
                C=c("A","A","A","A","B","B","B"))

p <- ggplot(data=d, aes(A, B)) + geom_point()

p <- p + geom_rect2()# + labs(title="OK2")

# Use vars() to supply faceting variables:
p + facet_wrap(~C, scales="free")

