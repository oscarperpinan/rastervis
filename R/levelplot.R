setGeneric('levelplot')

.levelplot <- function(df,
                       nly, ## Number of layers
                       bb, ## Bounding box
                       isll, ## Is Lon-Lat?
                       nms, ## Names of the layers
                       rat, ##Raster Attribute Table
                       anyFactor, ## TRUE if any layer is factor
                       isFactor, ## TRUE if all layers are factor
                       margin = list(), 
                       maxpixels = 1e5,
                       par.settings = rasterTheme(),
                       between = list(x = 0.5, y = 0.2),
                       as.table = TRUE,
                       xlab, ylab,
                       main = NULL,
                       names.attr,
                       scales = list(),
                       xscale.components = xscale.raster,
                       yscale.components = yscale.raster,
                       zscaleLog = NULL,
                       colorkey = list(space = 'right'),
                       panel = panel.levelplot, pretty = FALSE, 
                       contour = FALSE, region = TRUE, labels = FALSE,
                       FUN.margin = NULL,
                       scales.margin = NULL, axis.margin = NULL,
                       ..., att)
{
    
    ## Extract components from par.settings              
    if (is.function(par.settings))
        par.settings <- par.settings()
    ## The first two columns store coordinates.     
    dat <- df[, -c(1, 2)]
    ## Ensure valid names
    nms <- make.names(nms, unique = TRUE)
    names(dat) <- nms

    ## If zscaleLog is not NULL, transform the values and
    ## choose a function to calculate the labels
    if (identical(FALSE, zscaleLog)) zscaleLog <- NULL
    if (!is.null(zscaleLog) && !isFactor){
        zlogbase <- if (is.logical(zscaleLog)) {
                        10
                    } else {
                        if (is.numeric(zscaleLog)) {
                            zscaleLog
                        } else {
                            if (zscaleLog == "e") {
                                exp(1)
                            }
                        }}
        if (zlogbase==10) {
            zscale.components <- zscale.components.log10ticks
        } else {
            zscale.components <- zscale.components.logpower
        }
        dat <- log(dat, zlogbase)
        ## Update the data content of the original data.frame
        df[, -c(1, 2)] <- dat
    }
    ## Calculate the range (for zscale.components). This
    ## sapply/range combination is needed for logical
    ## RasterStacks, see issue #31
    rangeZ <- if (NCOL(dat) > 1) {
                  range(sapply(dat, range, na.rm = TRUE))
              } else {
                  range(dat, na.rm = TRUE)
              }
    zlim <- extendrange(dat,
                        r = rangeZ,
                        f = lattice.getOption("axis.padding")$numeric)

    ##aspect and scales(from sp::spplot.grid,
    ##sp::longlat.scales, sp::mapasp)
    if (inherits(bb, "Extent")) {
		xlim=c(raster::xmin(bb), raster::xmax(bb))
		ylim=c(raster::ymin(bb), raster::ymax(bb))
	} else {
		xlim=c(terra::xmin(bb), terra::xmax(bb))
		ylim=c(terra::ymin(bb), terra::ymax(bb))	
	}
    if (isTRUE(isll)){

        aspect=(diff(ylim)/diff(xlim))/cos((mean(ylim) * pi)/180)

        xscale.components <-
            if (identical(xscale.components,
                          xscale.raster))
                xscale.raster.EW
            else if (identical(xscale.components,
                               xscale.raster.subticks))
                xscale.raster.EWsubticks
            else xscale.components
        yscale.components <-
            if (identical(yscale.components,
                          yscale.raster))
                yscale.raster.NS
            else if (identical(yscale.components,
                               yscale.raster.subticks))
                yscale.raster.NSsubticks
            else yscale.components

    } else { ## !isLonLat
        aspect='iso'
    }

    ## Colorkey 
    if (region==FALSE) colorkey <- FALSE

    colorkey.default <- list(space = 'right')
    if (isTRUE(colorkey)) {
        has.colorkey <-  TRUE
        colorkey <- colorkey.default
    } else {
        has.colorkey <- is.list(colorkey)
    }

    ## Build the zscale.components and colorkey paying
    ## attention to zscaleLog
    if (!is.null(zscaleLog) && !isFactor){
        zscale <- zscale.components(zlim, zlogbase)
        colorkey.default = modifyList(colorkey.default,
                                      list(labels = zscale,
                                           raster = TRUE,
                                           interpolate = TRUE))
    }
    ## Factor variables need special colorkey
    if (isFactor) {
        ratLevels <- rat[, att]
        labs <- ratLevels
        nLabs <- length(ratLevels)
        ## define the breaks
        my.at <- seq(0.5, nLabs + 0.5,
                     length = nLabs + 1)
        ## the labels will be placed vertically centered
        my.labs.at <- seq_len(nLabs)

        colorkey.default <- modifyList(colorkey.default,
                                       list(
                                           at = my.at,
                                           height = min(1, 0.05*nLabs),
                                           labels = list(labels = labs,
                                                         at=my.labs.at))
                                       )
    }
    ## Finally construct colorkey with user definition
    ## (colorkey) and defaults defined by zscale, and
    ## isFactor
    if (has.colorkey) {
        if (is.logical(colorkey)){
            colorkey = colorkey.default
        } else {
            colorkey = modifyList(colorkey.default, colorkey)
        }
    }

    ## Construct the margins
    if (is.logical(margin))
        margin <- drawMargin(margin)

    if (!is.null(FUN.margin)) {
        warning('FUN.margin is deprecated. Use margin as a list instead.')
        margin <- modifyList(margin, list(margin = FUN.margin))
    }
    if (!is.null(axis.margin)) {
        warning('axis.margin is deprecated. Use margin as a list instead.')
        margin <- modifyList(margin, list(axis = axis.margin))
    }
    if (!is.null(scales.margin)) {
        warning('scales.margin is deprecated. Use margin as a list instead.')
        margin <- modifyList(margin, list(scales = scales.margin))
    }

    margin <- do.call('constructMargin', margin)
    ## Disable margins for multilayer and categorical
    ## rasters
    if (nly > 1 | anyFactor)
        margin <- drawMargin(FALSE)
    if (isTRUE(margin$x$draw)){
        par.settings <-
            modifyList(par.settings,
                       list(
                           layout.heights = list(
                               top.padding = 10,
                               xlab.key.padding = 3))
                       )
    }
    if (isTRUE(margin$y$draw)){
        ## put the colorkey at the bottom to leave space for
        ## the margin
        if (has.colorkey) {
            colorkey <- modifyList(colorkey,
                                   list(space='bottom'))
        }
        par.settings <-
            modifyList(par.settings,
                       list(
                           layout.widths = list(
                               right.padding = 10))
                       )
    }

    ## Which panel did the user requested? (panel.levelplot
    ## or panel.levelplot.raster)
    requestedPanel <- panel

    ## Build a custom panel useful for zscaleLog with contour
    panelMixed <- function(...,
                           contour, region,
                           at, labels,
                           levelPanel=requestedPanel,
                           lwd=c(1, 0.4) # Two line widths,
                                        # for main and
                                        # minor contour
                                        # lines
                           ){
        ##Draw the regions with the panel requested by the
        ##user
        if (region) levelPanel(..., at=at,
                               contour=FALSE, labels=FALSE)
        has.labels <- (is.logical(labels) &&
                       labels) ||
            is.list(labels)
        mainTicks=(as.character(zscale$labels)!=FALSE)
        mainLabels <- list(labels=zscale$labels[mainTicks],
                           cex=0.8)
        minorTicks=!mainTicks
        ## Contour lines for main divisions (log10Ticks or
        ## logpower) Their width is lwd[1]
        panel.contourplot(...,
                          at=zscale$at[mainTicks], lwd=lwd[1],
                          labels=if (has.labels) {
                                     mainLabels
                                 } else {FALSE},
                          region=FALSE, contour=TRUE)
        ## Contour lines for minor divisions (only log10Ticks)
        if (any(minorTicks)) {
            panel.contourplot(...,
                              at=zscale$at[minorTicks],
                              lwd=lwd[2],
                              region=FALSE, contour=TRUE)
        }
    }

    ## Finally, the panel function depends on zscaleLog and
    ## contour
    panel <- if (!is.null(zscaleLog) &&
                 isTRUE(contour) &&
                 !isFactor) {
                 panelMixed
             } else {
                 requestedPanel
             }

    ## Names of each panel
    if (missing(names.attr)){
        names.attr <- nms
    } else {
        ## names.attr <- as.character(names.attr)
        if (length(names.attr) != nly)
            stop('Length of names.attr should match number of layers.')
    }
    ## Build the formula for levelplot
    form <- as.formula(paste(paste(nms,
                                   collapse='+'),
                             'x*y', sep='~'))


    ## For each layer: is the raster completely filled with
    ## NA?  If the object is a multilayer Raster and there
    ## is at least one layer that with non-missing values,
    ## levelplot works correctly. If it is a RasterLayer and
    ## is filled with NA or if all the layers are NA then we
    ## have to provide an empty panel.
    if (all(is.na(dat))) {
        region <- FALSE
        colorkey <- FALSE
        margin <- drawMargin(FALSE)
        pretty <-  TRUE
    }

    ## And finally, the levelplot call
    p <- levelplot(form, data = df,
                   scales = scales,
                   aspect = aspect,
                   xlab = xlab, ylab = ylab, main  =  main,
                   par.settings = par.settings,
                   between = between,
                   as.table = as.table,
                   xscale.components = xscale.components,
                   yscale.components = yscale.components,
                   colorkey = colorkey,
                   contour = contour, region = region,
                   labels = labels,
                   strip = strip.custom(factor.levels = names.attr),
                   panel = panel, pretty = pretty, ...)

    ## Colorkey Title
    if (has.colorkey && !is.null(colorkey$title))
    {
        space <- colorkey$space
        title <- colorkey$title
        title.gpar <- if (is.null(colorkey$title.gpar)) list()
                      else colorkey$title.gpar
        ## Modify the legend field constructed by levelplot,
        ## replacing draw.colorkey by drawCK, and adding the
        ## title and title.gpar components.
        p$legend[[space]] <- list(fun = drawCK,
                                  args = list(key = modifyList(
                                                  p$legend[[space]]$args$key,
                                                  list(title = title,
                                                       title.gpar = title.gpar)),
                                              space = space))
    }

    ## panel.levelplot uses level.colors to encode values
    ## with colors. It does not work properly with
    ## categorical data and col.regions
    if (isFactor) p <- update(p, at = my.at)

    ## Plot the margins if required
    if (is.null(p$legend)) p$legend <- list()
    if (isTRUE(margin$y$draw)) {
        right <- list(
            fun = legendGeneric,
            args = list(p,
                        FUN = margin$y$FUN,
                        scaleAxis = margin$y$scales,
                        axis.margin = margin$y$axis,
                        side = 'y'))
        p$legend <- modifyList(p$legend, list(right = right))
    }
    
    if (isTRUE(margin$x$draw)){
        top <- list(
            fun = legendGeneric,
            args = list(p,
                        FUN = margin$x$FUN,
                        scaleAxis = margin$x$scales,
                        axis.margin = margin$x$axis, 
                        side = 'x'))
        p$legend <- modifyList(p$legend, list(top = top))
    }
    ## Here is the result!
    p
}

setMethod('levelplot',
          signature(x = 'Raster', data = 'missing'),
          definition = function(x, data = NULL, layers,
              margin = list(), 
              maxpixels = 1e5,
              par.settings = rasterTheme(),
              between = list(x = 0.5, y = 0.2),
              as.table = TRUE,
              xlab = if(isLonLat(x)) 'Longitude' else NULL,
              ylab = if(isLonLat(x)) 'Latitude' else NULL,
              main = NULL,
              names.attr,
              scales = list(),
              xscale.components = xscale.raster,
              yscale.components = yscale.raster,
              zscaleLog = NULL,
              colorkey = list(space = 'right'),
              panel = panel.levelplot, pretty = FALSE, 
              contour = FALSE, region = TRUE, labels = FALSE,
              FUN.margin = NULL,
              scales.margin = NULL, axis.margin = NULL,
              ..., att = 1L) {
              
              ## Subset the object if layers are defined
              if (!missing(layers)) {
                  object <- raster::subset(x, subset=layers)
              } else {
                  object <- x
              }
              
              ## The plot display a sample of the whole object defined
              ## with maxpixels
              objectSample <- sampleRegular(object, size = maxpixels,
                                            asRaster = TRUE)
              
              ## Is factor?
              factorLayers <- raster::is.factor(object)
              isFactor <- all(factorLayers)
              anyFactor <- any(factorLayers)

              if (anyFactor & !isFactor) {
                  stop('Raster* with factor and numeric layers cannot be displayed.')
              }

              if (isFactor) {
                  rat <- raster::levels(object)
                  ## It works correctly only if all the layers
                  ## share the same RAT
                  if (length(rat)>1 && any(!duplicated(rat)[-1])){
                      stop('all the layers must share the same RAT.')
                  } else {
                      rat <- as.data.frame(rat[[1]])
                      ## choose which level to use for the legend
                      if (is.numeric(att)) att = att + 1
                      ratID <- rat$ID
                      objectSample <- subs(objectSample,
                                           data.frame(ratID, seq_along(ratID)))
                      names(objectSample) <- names(object)
                  }
              } else rat <- NULL
              
              ## Convert to a data.frame for conventional levelplot
              df <- raster::as.data.frame(objectSample, xy=TRUE)
              ## Number of layers
              nly  <- nlayers(object)
              ## Names of layers
              nms <- names(object)

              ##aspect and scales(from sp::spplot.grid,
              ##sp::longlat.scales, sp::mapasp)
              bb <- extent(object)
              isll  <- isLonLat(object)

              p <- .levelplot(df, nly, bb, isll, nms, rat, anyFactor,
                              isFactor, margin, maxpixels,
                              par.settings, between, as.table, xlab,
                              ylab, main, names.attr, scales,
                              xscale.components, yscale.components,
                              zscaleLog, colorkey, panel, pretty,
                              contour, region, labels, FUN.margin,
                              scales.margin, axis.margin, ...,
                              att = att)
              p
          }
          )

setMethod('levelplot',
          signature(x = 'SpatRaster', data = 'missing'),
          definition = function(x, data = NULL, layers,
              margin = list(), 
              maxpixels = 1e5,
              par.settings = rasterTheme(),
              between = list(x = 0.5, y = 0.2),
              as.table = TRUE,
              xlab = if(is.lonlat(x)) 'Longitude' else NULL,
              ylab = if(is.lonlat(x)) 'Latitude' else NULL,
              main = NULL,
              names.attr,
              scales = list(),
              xscale.components = xscale.raster,
              yscale.components = yscale.raster,
              zscaleLog = NULL,
              colorkey = list(space = 'right'),
              panel = panel.levelplot, pretty = FALSE, 
              contour = FALSE, region = TRUE, labels = FALSE,
              FUN.margin = NULL,
              scales.margin = NULL, axis.margin = NULL,
              ..., att = 1L) {
              
              ## Subset the object if layers are defined
              if (!missing(layers)) {
                  object <- terra::subset(x, subset=layers)
              } else {
                  object <- x
              }
              
              ## The plot display a sample of the whole object defined
              ## with maxpixels
              objectSample <- spatSample(object, method="regular", size = maxpixels,
                                            as.raster = TRUE)
              
              ## Is factor?
              factorLayers <- terra::is.factor(object)
              isFactor <- all(factorLayers)
              anyFactor <- any(factorLayers)

              if (anyFactor & !isFactor) {
                  stop('Raster* with factor and numeric layers cannot be displayed.')
              }

              if (isFactor) {
                  rat <- terra::levels(object)
                  ## It works correctly only if all the layers
                  ## share the same RAT
                  if (length(rat)>1 && any(!duplicated(rat)[-1])){
                      stop('all the layers must share the same RAT.')
                  } else {
                      rat <- as.data.frame(rat[[1]])
                      ## choose which level to use for the legend
                      if (is.numeric(att)) att = att + 1
                      ratID <- rat$ID
                      objectSample <- subst(objectSample,
                                           ratID, seq_along(ratID))
                      names(objectSample) <- names(object)
                  }
              } else rat <- NULL
              
              ## Convert to a data.frame for conventional levelplot
              df <- terra::as.data.frame(objectSample, xy=TRUE)
              ## Number of layers
              nly  <- nlyr(object)
              ## Names of layers
              nms <- names(object)
              
              ##aspect and scales(from sp::spplot.grid,
              ##sp::longlat.scales, sp::mapasp)
              bb <- ext(object)
              isll  <- is.lonlat(object)

              p <- .levelplot(df, nly, bb, isll, nms, rat, anyFactor,
                              isFactor, margin, maxpixels,
                              par.settings, between, as.table, xlab,
                              ylab, main, names.attr, scales,
                              xscale.components, yscale.components,
                              zscaleLog, colorkey, panel, pretty,
                              contour, region, labels, FUN.margin,
                              scales.margin, axis.margin, ...,
                              att = att)
              p
          }
          )
