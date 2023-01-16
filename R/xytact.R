#' @name xytact
#'
#' @alias xytact
#'
#' @title Plot sensor straams and contexts based on lattice \code{xyplot} function
#'
#' @description This function creates a lattice plot with continuous sensor streams and
#' discrete context overlays (timelines)
#'
#' @author Neil Klepeis
#'
#' @param x formula indicating X and Y variables and conditioning variables for the sensor streams
#' @param data a data frame containing the variables included in the formula
#' @param groups
#' @param tact.var a character vector with names of variables in \code{data} or \code{tact.data}
#' that represent the time-activity of states with one group of mutually exclusive states
#' per variable
#' @param tact.data a data set (if other than \code{data}) containing time-activity data
#' @param codes
#' @param codes.colors
#' @param key.space
#' @param key.columns
#' @param alpha.tact,
#' @param col
#' @param fill
#' @param pattern
#' @param pattern.type
#' @param pattern.angle
#' @param pattern.width
#' @param pattern.col
#' @param debug
#' @param abline
#' @param shnow.outside.key
#' @param show.inside.key
#' @param show.labels
#' @param \dots

#' @details
#'
#' This is a legacy function to plot sensor streams along with contexts in the form
#' of rectangles overlaid onto lattice panels to represent timelines of states.
#' This kind of plot may be possible using ggplot facets but there seems to
#' be more flexible to create conditioning panel with overlays using the
#' \code{lattice} R package.
#'
# ------------------------------------------------

## Taken from the her.Activities pacakage 5Feb2022 in the xytact2.R file
#    cleaned up here.   Feb 2022  NK


xytact <-
  function (x, data, groups=NULL, tact.var, tact.data=data, codes, codes.colors,
            color.fun=gray.colors, group.factor=1,
            key.space="right", key.columns=1, alpha.tact=0.2, col=NULL,
            fill=TRUE, pattern=FALSE,
            pattern.type=c("segments","points"), pattern.angle=45, pattern.width=3,
            pattern.col="gray20", debug=FALSE, abline=NULL, show.outside.key=TRUE,
            show.inside.key=TRUE, show.labels=FALSE, ...)
  {

    # NOTE:   In this function `fill' and 'pattern' are applied to the time-activity bands
    #			that aer drawn on each plot (NOT the actual data series which are handled
    #			with "area" in the panel function.   NK   26-jan-2012

    # Note:   Dec-14-2011:   The new panel.superpose.tact function allows for drawing area
    #		under the curve with panel.xyarea arguments passed for "..." above...

    # UPDATE:   Now have uiser-specified function to auto compute colors
    #                we order the auto-generated codes in case we want to
    #                assign colors by their value.   NK 25-Sept-2015
    # UPDATE:   Added debug switch arg. NK 17Dec2012
    # UPDATE:   Allow "space" for tact legend to be specified.  defaults to "right"   6-Apr-2010
    # UPDATE:   Allow for case when group is not specified (remains NULL).  5-Apr-2010


    # TO DO IDEA:   make a codes.expression to match different variables in the tact.vars.
    #			Nk 29-Aug-2011


    #  Version of xytact that uses panel.superpose.tact instead of panel.superpost.4.   This
    #     function requires a separate data frame specification and doesn't try to reshape the
    #     'data' argument.    NK 11-June-2009

    #	!!! NOTE:   This approach should be changed to use the "subscripts" instead to pull in tact data
    #          from aligned columns in the parent data frame.   ...
    #  See this post:  http://www.nabble.com/Drawing-rectangles-in-multiple-panels-td11544191.html#a11553636

    # and:  http://www.nabble.com/Overlay-plots-from-different-data-sets-using-the-Lattice-package-td14824421.html#a14824822

    #  Also,  ggplot might be a better package to draw in data from different sources to the same plot...

    # Front end to the lattice "xyplot" function to plot time-activity data overlaying
    #     the usual X-Y data time series plot.   Uses a special panel.superpose.4 funcation, which
    #     calls a special panel.tact function.   type="t" indicates data should be plotted
    #     as a segmented rectangle depicting events occuring in time.    tact code data are converted
    #     to integers so they can be passed to xyplot.

    #  The function expects a "molten" data set with a measure (value) variable and everything else
    #        id variables.   tact.var contains current id variable names that contain tact information
    #        that are to be melted (converted) into the long format..

    #  Uses the reshape "melt" command to pile time-activity data in the dataframe and
    #    convert factors to numeric codes that can be passed to the xyplot function.

    #  If groups is passed as non-NULL, then we interact it with the "variable" containing the
    #      tact codes.      groups can be an interaction or other expression to begin with.  This
    #      allows us to plot multiple time series overlaid with tact data.  It's possible that
    #    more than one tact will be overlaid on the plot, so just be aware of this.

    #   IMPORTANT NOTE:   If transparent or semi-transparent code colors are not used then non-tact
    #               groups after the first one plotted can be covered up by subsequent tacts.

    #   TODO:   We should write panel.tact so that it deals with multiple tact groups and plots them
    #        stacked with a spacer of some kine (see the orginal plot.tact function)...    7-June-09

    #require(heR.Misc)
    #require(lattice)
    #require(grid)
    #if (!is.null(layers)) require(latticeExtra)
    #require(reshape)

    if (missing(data))
      stop("Must specify a dataframe to use in `data'.")

    #  If we have no tact variables, then NULL out everything related to tact (just in case
    #				maybe not totally necessary...)   --NK 26-Jan-2012
    if (missing(tact.var) || is.null(tact.var)) {
      warning("No specified time-activity variable(s) in `tact.var'.")
      tact.var <- NULL
      tact.data <- NULL
      codes <- NULL
      codes.colors <- NULL
      key <- NULL
      fill <- FALSE
      pattern <- FALSE
    }


    #if (any(names(data) %in% "value"))
    #  stop("Please rename data names to something beside `value', which is reserved by the melt function in reshape.")

    #superpose.symbol <- trellis.par.get("superpose.symbol")
    #superpose.line <- trellis.par.get("superpose.line")

    #cat("xytact1\n")
    #environment(x) <- environment()   # look for groups object here in the function frame...
    # Can either be an expression evaluated in the data argument dataframe or an
    #		object, such as a dataframe, passed in.... NK 12-14-2011
    #groups <- eval(substitute(groups), data, environment(x))

    #print(ls(envir=parent.frame(n=1)))  #  not sure what frame this really is....
    #print(ls(envir=parent.frame(n=2)))    #  seems to be frame where function was called.
    lastframe <- parent.frame(sys.parent())   # frame where this function was called
    #  Go up one level to calling function to look for values of group variable...
    #groups <- eval(substitute(groups), data, parent.frame(n=2))   # this one worked too.
    environment(x) <- lastframe
    ## hmmm we don't seem to need this next line and it muchs stuff up,.. 23Sept2015 NK
    ######groups <- eval(substitute(groups), data, environment(x))
    #print(ls(envir=lastframe))

    #cat("xytact2\n")
    #print(head(groups))

    #if (is.character(groups))
    #	l <- levels(as.factor(eval(substitute(get(groups)), env=data)))
    #else {

    # Don't do this...
    #####environment(x) <- environment()

    #l <- levels(as.factor(eval(substitute(groups), env=data)))
    #}

    # Don't currently use this...
    ######l <- levels(as.factor(groups.eval))
    #print(l)

    superpose.symbol <- trellis.par.get("superpose.symbol")
    if (is.null(col)) col <- superpose.symbol$col
    #col <- rep(superpose.symbol$col, length.out=length(l))


    # get unique set of events across all tact.vars.
    if (!is.null(tact.var)) {

      cat("Using tact variables:",ifelse(length(tact.var)>1,paste(tact.var,collapse="|"),tact.var),"\n")

      if (missing(codes) || is.null(codes)) {
        codes <- c()
        #  The c() seems to return only the underlying codes of factors???????? Is this new?
        for (i in tact.var)
          codes <- c(codes, as.character(tact.data[[i]]))
        codes <- unique(codes)
        if (all(is.na(codes))) stop("No code values found for variable(s) in `tact.var'.")
        #  sort them in case we want to use numeric sorting....
        codes <- as.character(codes[!is.na(codes)])
        codes <- codes[order(codes)]
      }


      if (debug > 1) { cat("Codes:\n");  print(codes)}

      if (missing(codes.colors) || is.null(codes.colors)) {
        #  NEW.  Have user-specified function to automatically assign
        #    codes based on sorted value... NK 25-Sept-2015
        #cat("pre-ordered codes:", codes)
        #codes <- codes[order(codes)]    order above...
        color.fun <- match.fun(color.fun)
        codes.colors <- color.fun(n=length(codes), alpha=alpha.tact)
      } else {
        codes.colors <- rep(codes.colors, length.out=length(codes))
      }
      cat("Using tact codes: ",ifelse(length(codes)>1,paste(codes,collapse="|"),codes),"\n")
      cat("Using tact colors: ",ifelse(length(codes.colors)>1,paste(codes.colors,collapse="|"),codes.colors),"\n")
      cat("Using tact group factors: ",ifelse(length(group.factor)>1,paste(group.factor,collapse="|"),group.factor),"\n")


      key <- list(space=key.space,
                  text = list(labels=codes),
                  columns=key.columns,
                  title=ifelse(length(tact.var)>1,paste(tact.var, collapse=" +\n"), tact.var))

      if (pattern) {
        pattern.type <- rep(pattern.type, length.out=length(codes))
        #if (is.null(pattern.angle))
        #	pattern.angle <- sample(-45:45, length(codes))
        #else pattern.angle <- rep(pattern.angle, length.out=length(codes))
        pattern.angle <- rep(pattern.angle, length.out=length(codes))
        pattern.width <- rep(pattern.width, length.out=length(codes))
        pattern.col <- rep(pattern.col, length.out=length(codes))
        fill.col <- "transparent"
      }

      if (fill) fill.col  <- codes.colors

      if (!pattern & fill) pattern.type="none"

      key$prectangles <- list(pwidth=pattern.width, ptype=pattern.type, pangle=pattern.angle,
                              pcol=pattern.col, col=fill.col)

      # NK 17Dec2012 - add this in case we aren't using my new
      #	 draw.key that allows for patterns.... drop back to just solid color fill.
      key$rectangles <- list(col=fill.col)

    }

    # hide the key?
    if (!show.outside.key) key <- NULL


    #cat("What are groups?\n")
    #print(head(groups))
    #print(sys.parents())
    ##print(sys.frames())
    #print(lastframe)
    #print(environment())
    ##print(sys.frame(0))
    ##print(sys.frame(-1))
    ##print(sys.frame(-2))
    ##print(sys.frame(-3))
    ##print(sys.frame(-4))

    #print(sys.frames()[[length(sys.frames())]])
    #print(sys.frames()[[4]])

    #this.env <- environment()

    # Seems to work with a passing the 4th environment for all uses of this function so far,
    #	  but may not work in the future.   Basically, we are evaluating the groups argument
    #	  into a data object that must be "found" by subsequent function calls -- so we
    #	   need to specify the proper environment to look in.  #4 seems to be the most recent
    #		environment containing the evaluated groups...Actually, let's just make
    #		it the last environment in the sys.frames list, no actually we have to set
    #		it to 4 for some reason.   Not sure what is going on exactly (?)

    #  Actually, forget all that stuff, the following seems to work to let xyplot know
    #    that the groups data is in the same environment as the formula, x (actually
    #		the same environments as sys.frames()[[4]] for some of the runs...). NK 13-Dec-2011

    environment(x) <- environment()

    # Have to trick xyplot into evaluating groups in the calling function.....
    if (debug) cat("xytact:  plotting data...\n")
    p <- xyplot(x, data=data, groups=groups, group.factor=group.factor,
                #groups=evalq(groups, envir=sys.frame(1)),
                tact.data=tact.data, tact.var=tact.var,
                col=col, panel=panel_superpose_tact,
                codes=codes, codes.color=codes.colors,
                fill=fill, pattern=pattern, pattern.type=pattern.type,
                pattern.angle=pattern.angle, pattern.width=pattern.width,
                pattern.col=pattern.col, key = key,
                #legend=list(inside=list(fun=grid.legend.2(pch=rep(16, length.out=length(l)),
                #									col=col, label=l,vgap=0.05, draw=FALSE))),
                debug=debug, abline=abline, show.labels=show.labels,
                show.inside.key=show.inside.key, ...)

    #if (!is.null(layers))
    #  if (!is.list(layers))
    #    stop("`layers' must be a list of layers for adding to the plot")
    #else for (i in length(layers)) p <- p + layers[[i]]

    return(p)
  }
