
## Taken from the her.Activities pacakage 5Feb2022 in the panel.superpose.tact.R file
#    This is the one to use with the latest xytact.R function.
#  uses panel.mtact from heR.Activities...


panel_superpose_tact<-
  function (x, y, subscripts, groups=NULL, tact.data, tact.var, col=NULL,
            col.line = superpose.symbol$col, col.symbol = superpose.symbol$col,
            pch = superpose.symbol$pch, cex = superpose.symbol$cex, cex.key=0.8,
            columns.key=2, between.key=1, between.columns=0,
            lty = superpose.line$lty, lwd = superpose.line$lwd, type = "p",
            group.factor=1, border.area=trellis.par.get("plot.polygon")$border,
            alpha.area=trellis.par.get("plot.polygon")$alpha,
            fill.area=trellis.par.get("plot.polygon")$col, origin.area=0,
            remove.nonincreasing=FALSE, show.inside.key=TRUE, debug=FALSE, abline=NULL, ...)
  {

    # UPDATE:  Added "group.factor" for simple multiplication of each series by a
    #           factor...
    # UPDATE:   Add type = "A" for adding area per series (not area=TRUE for *all* series as before)
    #           NK 26-Sept-2015
    # UPDATE:   store mtact object in list with one element per group.... so we can
    #             access one groups tacts at a time in the plot.tact function. NK24Sep2015
    # UPDATE:  Add parameter abline to allow for plotting any kind of lines in a panel
    #         abline is a list with parameters the same as panel.abline.. NK 8Feb14
    # UPDATE:  Add option to remove data with nonincreasing time values, just keeping the last one
    #		in a series that has the same time or a decrementing time.  NK 19Dec2012
    # UPDATE:  Modify to *not* plot tact if data have nonincreasing time or too few points
    #			but still go on to plot the X-Y data -- giving warnings.  NK 17Dec2012
    # UPDATE:  Add debug argument for increased output... NK 17Dec2012
    # UPDATe:  If tact.var is NULL, we now just draw the data series.  NL 26-Jan-2012

    # UPDATE:   Added ability to draw area under the curve plots.   Nk 26-Aug-2011

    #  UPDATE:  Fixed bug with setting col.line/col.symbol to col if col not missing... now we
    #		do it differently looking at lenght of col (since it seemed to be passed but
    #			with no length).  NK  26-Aug-2011

    # UPDATE:  Didn't actually seem to work well when groups=NULL, so I added a separate
    #		panel.xyplot call when groups=NULL.   NK 26-Aug-2011   ... didn't help...

    #  Now deal with case when groups=NULL (i.e., missing).   nk 6-Apr-2010

    #  UPDATE:  Now process time breaks with format(..., nsmall=6) to insure that they are
    #        passed with digits out to the 6th place (e.g., microseconds for time class objects).
    #    Time classes are passed with digits out to microseconds hidden (but still there).
    #     But they are cut off to the 5th place if only using the as.character() conversion
    #    that `tact' uses.  So use format(x,nsmall=6) instead.  Works!!
    #                NK  3-March-2010

    #  OK.  This is the new version panel.superpose.tact, which uses the subscripts argument instead
    #     of having to reshape the data file to plot time-activity information....
    #    tact.var contains the variables that contain tact information to plot...

    #	!!! NOTE:   This approach should be changed to use the "subscripts" instead to pull in tact data
    #          from aligned columns in the parent data frame.   ... Done.

    #  See this post:http://www.nabble.com/Drawing-rectangles-in-multiple-panels-td11544191.html#a11553636

    #  and:  http://www.nabble.com/Overlay-plots-from-different-data-sets-using-the-Lattice-package-td14824421.html#a14824822

    #  Also check out ggplot sometime....to pull in data from various sources...

    #   Based on panel.superpose.3 in heR.Misc package.
    #      But we add a new type "tact" where we plot time-activity
    #       data optionally along with any other data groups in "group".
    #       Note:  Make sure to align the elements of the "type" argument so
    #       the factors in "group" for time activity data are plotted
    #       correctly.   NK  6-June-2009

    #  Simple change to define factors only on the levels in the subgroup
    #     of values passed to each panel.  This allows for specifying
    #     colors for each panel series and making a (nice compact) legend for each panel.
    #           NK  19-June-2008

    #cat("got to panel.superpose.tact\n")

    #   Return seconds to 6 decimal place if POSIXct class,
    #     otherwise just return as.numeric(x)
    #time2numeric <- function(x) {
    #	#print(attributes(x))
    #	if (inherits(x, "POSIXct")) {
    #		x1 <- as.numeric(strftime(x,format="%OS6"))
    #		x <- as.numeric(x) + x1 - trunc(x1)
    #	} else {
    #		x <- as.numeric(x)
    #	}
    #	x
    #}

    if (debug) cat("Inside `panel.superpose.tact' ...\n")

    if (any(type=="A")) require(latticeExtra)

    if (length(x) > 0) {

      if (debug) cat("`panel_superpose_tact':  X-Y data available")

      x <- as.numeric(x)
      y <- as.numeric(y)
      #x <- time2numeric(x)
      #y <- time2numeric(y)
      #  'groups' is the grouping variable as long as 'data'
      #  'subscripts' give the index of rows to use for the current panel (conditioned data)
      #  'x' and 'y' are independent and dependent variable vectors (conditioned data)
      #  We use groups to plot different curves differentiated by levels in groups
      if (!is.null(groups)) {
        subgr <- groups[subscripts]
        vals <- sort(unique(subgr))
      } else {
        subgr <- rep("All",length=length(x))
        vals <- "All"
      }

      superpose.symbol <- trellis.par.get("superpose.symbol")
      superpose.line <- trellis.par.get("superpose.line")

      nvals <- length(vals)
      tact.data <- tact.data[subscripts,]

      if (length(col) != 0) {
        col.line <- col
        col.symbol <- col
      }

      #print(col)

      col.line <- rep(col.line, length = nvals)
      col.symbol <- rep(col.symbol, length = nvals)
      pch <- rep(pch, length = nvals)
      lty <- rep(lty, length = nvals)
      lwd <- rep(lwd, length = nvals)
      cex <- rep(cex, length = nvals)
      type <- rep(type, length = nvals)

      fill.area <- rep(fill.area, length=nvals)
      alpha.area <- rep(alpha.area, length=nvals)
      border.area <- rep(border.area, length=nvals)

      # Note: now if tact.var is null we just draw the series with no
      #		tact data superimposed.    NK 26-Jan-2012

      # 1. Plot the time-activity (tact) information on bottom ----
      #      multiple tacts are plotted stacked on top of the order with
      #      first groups on top -- plot big mtact with all tact.vars and groups...
      do.tact <- TRUE
      if (!is.null(tact.var)) {
        if (debug) cat("`panel.superpose.tact': plotting tact data...")
        z <- list()
        grp <- 0
        for (i in seq(along = vals)) {
          grp <- grp + 1
          count <- 0
          z[[grp]] <- list()
          id <- (subgr == vals[i])
          # Move these conditions up here to give a warning, set flag, and do *not*
          # 	plot the tact.  Still go on to plot the X-Y data.  NK 17Dec2012
          if (any(diff(x[id]) <= 0 )) {
            if (remove.nonincreasing) {
              # remove all of the non-increasing times, leaving
              # the most recent point of any duplicate times...NK19Dec2012
              cat(paste("Removing from Group: ", vals[i],"\n"))
              idx <- which(diff(x[id]) <= 0)
              print(idx)
              cat("Before id TRUE's:",sum(id),"\n")
              id[idx] <- FALSE
              cat("After id TRUE's:",sum(id),"\n")
            } else {
              warning("Group: ",vals[i],". Times non-increasing. Can't plot time-activity data.  Are there two time-activities in a factor combination?  Try adding a conditional variable or a group interaction.  Or, optionally, set remove.nonincreasing=TRUE")
              cat(paste("Group ",vals[i]," times non-increasing @ \n"))
              print(which(diff(x[id]) <= 0))
              print(x[which(diff(x[id]) <= 0)])
              print(as.POSIXct(as.POSIXlt(x[which(diff(x[id]) <= 0)],
                                          origin="1970-01-01")))

              do.tact <- FALSE
            }
          }
          if (length(x[id]) < 2 ) {
            warning("Group: ",vals[i],".  Less than 2 data points.  Try subsetting the data.")
            do.tact <- FALSE
          }
          # Don't try to build up the tact if data do not conform. NK 17Dec2012
          if (any(id) & !any(diff(x[id]) <= 0) & !length(x[id]) < 2 )  {

            #print(x[diff(x) < 1])
            #print(format(x[id][which(diff(x[id]) < 1)], nsmall=6))
            for (j in 1:length(tact.var)) {
              count <- count + 1
              events <- tact.data[id, tact.var[j]]
              #print(head(x[id]))
              #print(head(events))
              #  Send breaks as character with 6 digits after decimal
              #    since `tact` converts everything with as.numeric(as.character())
              z[[grp]][[count]] <- list(breaks=format(x[id], nsmall=6),
                                        events=events[-length(events)])

              if (debug) print(head(z))
            }

          }
        }
        #   all [...] extra arguements sent to panel.mtact...
        if (do.tact) panel.mtact(z, ...)
      }




      # 2. Plot the usual X-Y data on top -----------------------
      if (!is.null(groups)) {
        group.factor <- rep(group.factor, length.out=length(vals))
        if (debug) cat("`panel.superpose.tact': plotting groups of series...\n")
        for (i in seq(along = vals)) {
          id <- (subgr == vals[i])
          if (any(id)) {
            #print(head(x[id]))
            #print(head(y[id]))
            # plot others as normal X-Y points
            #  not sure why but with no groups panel.xyplot doesn't produce
            #    output with all arguments.  The simple one below does work.... 6-Apr-2010
            #panel.xyplot(x = x[id], y = y[id], type=type[i])
            if (type[i] == "A")
              panel.xyarea(x = x[id], y = y[id]*group.factor[i],
                           col.line=col.line[i], lwd=lwd[i], lty=lty[i],
                           col=fill.area[i], border=border.area[i], alpha=alpha.area[i],
                           origin=origin.area)
            else
              panel.xyplot(x = x[id], y = y[id]*group.factor[i], pch = pch[i],
                           cex = cex[i], col.line = col.line[i],
                           col.symbol = col.symbol[i],
                           lty = lty[i], lwd = lwd[i], type = type[i])
            #panel.xyplot(x = x[id], y = y[id], pch = pch[i],
            #  cex = cex[i], col.line = col.line[i],
            #  col.symbol = col.symbol[i],
            #  lty = lty[i], lwd = lwd[i], type = type[i],
            #...)
          }
        }
        #grid.legend.3(pch, col.symbol, vals)
        #  Use new panel.key function that does both lines/points/text NK 17Dec2012
        #lines<-NULL; points<-NULL; rectangles<-NULL
        #if (any(c("b","p") %in% type))
        if (debug) {cat("panel.superpose.tact: group vals"); print(vals)}
        # Note: this is not the stock latticeExtra panel.key.  This one allows
        #  more complex 'key' arguments for styling the graphic elements. NK 5June2013
        if (show.inside.key)
          panel.key2(text=list(paste(vals,group.factor,sep="*")), rectangles=list(col=col.symbol, size=0.5, height=0.5),
                     cex=cex.key, columns=columns.key, between=between.key, between.columns=between.columns)
      } else {
        if (debug) cat("`panel.superpose.tact': `groups' is null \n")
        #  Something weird with col.line, nukes the whole thing...
        #		ok fixed above.. NK 26-Aug-2011
        if (type == "A")
          panel.xyarea(x = x, y = y,
                       col.line=col.line, lwd=lwd, lty=lty,
                       col=fill.area, border=border.area, alpha=alpha.area,
                       origin=origin.area)
        else
          panel.xyplot(x = x, y = y, pch = pch,
                       type=type, cex=cex, col.symbol=col.symbol,
                       col.line=col.line, lwd=lwd, lty=lty)
        #cex = cex, col.line = col.line,
        #col.symbol = col.symbol,
        #lty = lty, lwd = lwd, type = type,
        #groups=NULL)
      }

      # 3.   Plot any kind of ablines
      if (!is.null(abline))
        if (!is.list(abline)) {
          stop("`abline' must be a list with parameters for drawing ablines.")
        } else {
          do.call ("panel.abline", abline)
        }


    }
  }
