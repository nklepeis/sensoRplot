#' @name panel_mtact
#'
#' @aliases panel_mtact
#'
#' @title lattice panel function for plotting multiple time activity data groups (contexts)
#'
#' @description This function provides a function to plot multiple time activity context
#' timelines on a lattice plot
#'
#' @author Neil Klepeis
#'
#'
# ---------------------------------------------------------------

## Taken from the her.Activities package 5Feb2022 in the panel.mtact.R file
#    This is the one to use with the latest xytact.R/panel.superpose.tact functions.
## Function used in panel_superpose_tact
####

panel_mtact<-
  function (z, codes=NULL, codes.colors=NULL, alpha.tact=0.15,
            fill=TRUE, pattern=FALSE, overlap.bars=FALSE, overlap.all=FALSE,
            show.labels=FALSE, labels=NULL, srt.label=-90, adj.label=c(0,0),
            fontsize=0.5, fontfamily="HersheySans",
            fontface="bold", font.col="black",
            col.border=0, lwd.border=1,  box=FALSE, col.box=1, lwd.box=1,
            pattern.type="segment", pattern.angle=45, pattern.width=3, pattern.col="black",
            pattern.pch=16, pattern.cex=0.5,  margin.bottom=0, margin.top=0)
  {

    # Idea... have option for codes.colors to be "gray.colors", "heat.colors", etc.
    #    where we sort the codes (must be numeric) and assign increasing colors
    #     based on their value.

    # IDEA; Have option to stack ALL or group-wise or *overlap* all or group-wise
    #     Change alpha to let different events "show through" NA=means don't draw anything
    #     for a given event, letting other events be more visible    NK 25-Sept-2015
    #   Done...

    ##  UPDATE:  Now the passed mtact object z, is a list with elements that are mtacts
    #   corresponding to each group.  NK 24Sept2015
    ##  Revised -- fixed bar labeling.  can now overlap bars instead of stacking, e.g., to
    #    just show a single series' bars.  Removed "uncol" for NA codes.  Just remove those
    #   codes iwth missing color values...  These changes let us plot a single set of bars
    #    for one series by setting other series tact.vars to NA.  NK 24-Sept-2015

    # BIG BUG FIX:   Now we collapse the data.  Before, we were plotting all the individual points
    #      which ended up being a HUGE amount of individual graphics and making for big PDF graphics
    #      files....    24-Apr-2010

    # UPDATE:  Now we require gridExtra to draw hatched (and other) patterns in color or
    #    black & white (for publication).   New parameters: pattern (filled, segments, points),
    #	 angle, pattern.width, for drawing B&W rectangles.....
    #    .............also fixed the box=TRUE and display.labels features.  NK 24-Apr-2010
    #    ......  Also added `'fill' and `pattern' parameters to control filling with solid color
    #       and/or filling with a crosshatch point pattern.......(segments,points,grid)
    #   NOTE:  Points don't seem to work yet for panel functions....

    #  This version handles multiple time activities if y is a dataframe containing multiple
    #      columns.  NK 15-June-2009
    #  No, z is handled as an mtact object...

    #  Function to add a tact (time-activity) plot to the current lattice pane.
    #       Adapted from the original plot.tact function.   NK 6-june-2009

    #   Takes raw data as x and y coordinates...

    # TO DO:   handle multiple tacts on a single plot....should make a panel.mtact function.

    #      If we get duplicate times, try to plot by deleting 1 set of times????

    #    See plot.tact for full history of notes...
    # --------------------------------------------------------------

    #require(heR.Misc)
    #require(grid)
    #require(gridExtra)

    #cat("got to panel.tact\n")
    #print(list(x=x,y=y))

    # We expect y to consist of
    #if (!is.data.frame(y))
    #  yl <- tact(breaks=x, events=y[-length(y)])
    #else {
    #	yl <- list()
    #	for (i in 1:NCOL(y))
    #		yl[[i]] <- list(breaks=x, events=y[[i]][-NROW(y)])
    #}

    #print(z)

    #  We expect an mtact list, easiest way is to make it in the panel
    #       function and just send it along for plotting


    #cat("Got colors: ",codes.colors,"\n")

    # Density/angle of optional hatch lines
    #density <- rep(density, length=length(codes))
    #angle <- rep(angle, length=length(codes))

    #cat("got to find height\n")
    # Find height of event bars to plot

    ylim <- current.viewport()$yscale
    xlim <- current.viewport()$xscale
    AR <- diff(ylim)/diff(xlim)
    fullheight <- diff(range(ylim))
    ytop <- ylim[2]

    #print(list(ylim=ylim, ytop=ytop, height=height))


    # ----------------------------------------------------------------
    # Function to draw a horizontal,filled,stacked bar given vectors
    #         for breaks and events
    drawbar <- function (rownum, breaks, events) {

      m <- match(as.character(events), as.character(codes))
      #cat("Event matches raw...\n")
      #print(m)

      #print(data.frame(m=m, breaks=breaks[-length(breaks)], events=events))

      # remove matches with NA
      #m <- m[which(!is.na(m))]
      #cat("Event matches after NA removal...")
      #print(m)

      #if (length(m) == 0) {cat("No Non-NA codes...\n")}

      #  if some codes left, then continue
      #if (length(m) > 0) {

      #  codes/colors mismatched?????????????????????
      ccodes <- codes[m]
      # Get colors
      ac <- codes.colors[m]   # get colors for the events
      ##No just don't plot NA codes or colors..see above m code  NK 24Sept2015
      ######ac[is.na(ccodes)] <- uncol   # assign color/code? of unassigned codes. colors??? had bug I think...

      apat <- pattern.type[m]
      acol <- pattern.col[m]
      aang <- pattern.angle[m]
      awid <- pattern.width[m]
      apch <- pattern.pch[m]
      acex <- pattern.cex[m]

      #print(ac)
      #print(m)
      # Get hatches
      #dens <- density[m]
      #ang <- angle[m]

      xleft <- breaks[-length(breaks)]
      #if (!overlap.bars) {
      xbottom <- rep(ytop-height*rownum, length(m))
      xtop <- rep(ytop-height*(rownum-1), length(m))
      #} else {
      #  xbottom <- rep(ytop-height, length(m))
      #  xtop <- rep(ytop, length(m))
      #}
      xright <- breaks[-1]

      xbottom <- xbottom + (xtop[1] - xbottom[1]) * margin.bottom / 2
      xtop <- xtop - (xtop[1] - xbottom[1]) * margin.top / 2

      #  Get center points and total width/height
      x <- (xleft + xright)/2
      y <- (xbottom + xtop)/2
      width <- xright - xleft
      myheight <- xtop - xbottom

      #print(list(ac=ac, ccodes=ccodes, xleft=xleft, xbottom=xbottom, xtop=xtop, xright=xright))

      #  select only non-NA pairs
      keep <- which(!is.na(m))
      #cat("Event matches after NA removal...")
      #print(keep)

      if (length(keep) > 0)  {

        x <- x[keep]
        y <- y[keep]
        width <- width[keep]
        myheight <- myheight[keep]
        ccodes <- ccodes[keep]
        ac <- ac[keep]
        xleft <- xleft[keep]
        xright <- xright[keep]
        xtop <- xtop[keep]
        xbottom <- xbottom[keep]

        #print(data.frame(ac=ac, ccodes=ccodes, xleft=xleft, xbottom=xbottom, xtop=xtop, xright=xright))

        #print(data.frame(ac=ac, xleft=xleft, xright=xright))

        #cat("got to panel.rect\n")
        #for (k in 1:length(m)) {
        #if (apat[k]=="filled")
        #	panel.rect(xleft[k], xbottom[k], xright[k], xtop[k], col=ac[k],
        #		border=as.character(col.border), lwd=lwd.border)
        #else
        #	grid.vpattern(x=x[k], y=y[k], width=width[k], height=height[k], motif=apat[k],
        #			motif.params=list(angle=aang[k], pch=apch[k]),
        #			motif.width=awid[k], gp=gpar(cex=acex[k]), clip=TRUE)
        #}

        if (fill)
          panel.rect(xleft, xbottom, xright, xtop, col=ac,
                     border=as.character(col.border), lwd=lwd.border)

        if (pattern)
          for (k in 1:length(keep)) {

            # cat("Drawing TACT rects: ",k,"\n")
            # cat("Pattern:",apat[k],"\n")
            # cat("Angle:",aang[k],"\n")
            # cat("Width:",awid[k],"\n\n")


            x2 <- unit(x[k], "native")
            y2 <- unit(y[k], "native")
            width2 <- unit(width[k], "native")
            height2 <- unit(myheight[k], "native")
            motif.width2 <- unit(width[k], "mm")   #awid???
            AR2 <- height[k]/width[k]
            motif.height2 <- AR2*motif.width2
            #print(list(x=x2, y=y2, width=width2,height=height2, motif.width=motif.width2))
            #grid.vpattern(width=unit(1,"npc"), height=unit(1,"npc"),
            #      motif="segments", motif.params=list(angle=45))

            grid.vpattern(x=x2, y=y2,
                          width=width2, height=height2,
                          motif=apat[k],
                          motif.params=list(angle=aang[k], pch=apch[k],
                                            gp=gpar(cex=acex[k], col=acol[k])),
                          motif.width=motif.width2, motif.height=motif.height2,
                          clip=TRUE)
          }

        if (box)
          panel.rect(min(xleft), min(xbottom), max(xright), max(xtop),
                     border=col.box, lwd=lwd.box)

        if (show.labels) {
          #cat("Showing tact labels....\n")
          if (is.null(labels)) labels <- ccodes
          else labels <- rep(labels, length.out=length(ccodes))
          labels[is.na(labels)] <- ""
          #print(data.frame(m=m, ccodes=ccodes, ac=ac, labels=labels))
          #print(labels)
          #grid.text(labels, x = xleft, y = xtop, just = c("left","top"),
          #  		gp=gpar(fontsize=fontsize, col=font.col, fontface=fontface,
          #		  	fontfamily=fontfamily))
          #print(xleft)
          #print(xtop)
          panel.text(x=xleft, y=xtop, labels=labels,
                     cex=fontsize,
                     col=font.col, fontfamily=fontfamily,
                     fontface=fontface, adj=adj.label, srt=srt.label)
        }

        #cat("finished panel.rect\n")
      }
    }

    # -----------------------------------------------------------------

    # Draw the bars
    #if (is.null(d$events[[1]]))
    #  drawbar(range(d$breaks), NA)
    #else drawbar(d$breaks, d$events[[1]])

    #  Draws bars for multiple time activities (stacked in the plot)
    #     Drawing only the first event set.....

    ntotal <- 0
    ##cat("Total tact vars: ", length(z), "\n")
    for (i in 1:length(z)) ntotal <- ntotal + length(z[[i]])
    ##cat("Total tacts: ", ntotal, "\n")
    rownum <- 0

    ## Iterate over each group's mtact object.
    for (i in 1:length(z)) {

      ##cat("No. tacts in grp ", i, " = ", length(z[[i]]),"\n")
      #print(is.list(z[[i]]))
      d <- as_mtact(z[[i]])
      n <- length(d)    # number of tacts to plot in the current group
      #  Collapse the events to make smaller graphics file...  New bug fix..... NK 24-Apr-2010
      d <- tact.collapse(d)
      #print(d)

      if (!overlap.bars) height <- fullheight/ntotal
      else if (!overlap.all) height <- fullheight/n
      else height <- fullheight
      if (overlap.bars) rownum <- 0
      ##cat("No. tacts for group ", i, " = ", n, "\n")
      ##cat("Computed tact height: ", height,"\n")

      if (is.null(codes)) {
        codes <- as.character(attributes(d)$unique.events[[1]])
        # remove any NA codes
        codes <- codes[!is.na(codes)]
      } else if (any(duplicated(codes)) | length(codes) < 1 | any(is.na(codes)))
        stop("`codes' must contain at least one event type to plot with no duplications and no missing values.")

      #cat("Got codes: ",codes,"\n")

      if (is.null(codes.colors)) {
        codes.colors <- rainbow(length(codes), alpha=alpha.tact)
      } else {
        if (length(codes.colors) != length(codes))
          warning("Wrong number of colors, recycling to match event codes.")
        codes.colors <- rep(codes.colors, length=length(codes))
      }


      #  Add angle, pattern params, etc. filled-pattern rectangles.... 25-April-2010
      pattern.type <- rep(pattern.type, length.out=length(codes))
      pattern.col <- rep(pattern.col, length.out=length(codes))
      pattern.angle <- rep(pattern.angle, length.out=length(codes))
      pattern.width <- rep(pattern.width, length.out=length(codes))
      pattern.pch <- rep(pattern.pch, length.out=length(codes))
      pattern.cex <- rep(pattern.cex, length.out=length(codes))


      for (j in 1:n) {
        #cat("\nDrawing grp #",i," tact# ", j, "\n")
        rownum <- rownum + 1
        #cat("Current rownum = ", rownum, "\n")
        drawbar(rownum, d[[j]]$breaks, d[[j]]$events[[1]])
        if (overlap.bars && overlap.all) rownum <- 0
      }

    }

  }
