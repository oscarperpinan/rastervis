constructMargin <- function(draw = TRUE, FUN = 'mean',
                            scales = NULL, axis = FALSE,
                            ..., x = NULL, y = NULL){
    x.margin <- y.margin <- list(draw = draw, FUN = FUN,
                                 axis = axis)
    if (!is.null(scales)){
        x.margin$scales <- scales$x
        y.margin$scales <- scales$y
    }
    if (!is.null(x)){
        if (is.logical(x))
            x <- list(draw = x)
        x.margin[names(x)] <- x
    }
    if (!is.null(y)){
        if (is.logical(y))
            y <- list(draw = y)
        y.margin[names(y)] <- y
    }
    list(x = x.margin, y = y.margin)
}

drawMargin <- function(draw = FALSE){
    margin <- list(x = list(draw = draw),
                   y = list(draw = draw)
                   )
}
    

legendGeneric <- function(p, FUN, scaleAxis, axis.margin, side, ...){
    ## Extract values from the trellis object
    ref <- switch(side,
                  x = p$panel.args.common$x,
                  y = p$panel.args.common$y)
    refLim <- switch(side,
                     x = p$x.limits,
                     y = p$y.limits)
    pz <- p$panel.args.common$z

    ## Aggregate z values with FUN
    ag <- aggregate(pz ~ ref, FUN = FUN)
    rgZ <- range(ag$pz, na.rm = TRUE)
    ## If NULL is provided, use range
    if (is.null(scaleAxis)) scaleAxis <- rgZ
    ## 1-element vector implies the lower extreme of the scale. Use
    ## the maximum value for the upper extreme.
    else if (length(scaleAxis) == 1)
        scaleAxis <- c(scaleAxis, rgZ[2])
    ## If a 2-elements vector is provided, substitute the NA's with
    ## the corresponding element of the range.
    if (is.na(scaleAxis[1])) scaleAxis[1] <- rgZ[1]
    if (is.na(scaleAxis[2])) scaleAxis[2] <- rgZ[2]
    if (scaleAxis[1] > scaleAxis[2])
        stop(paste(side, "component of scales.margin is incorrectly defined. Lower limit is higher than upper limit."))
    ## Finally, trunc values above and below the scale limits.
    ag$pz <- pmax(ag$pz, scaleAxis[1])
    ag$pz <- pmin(ag$pz, scaleAxis[2])
    ## Horizontal axis, corrected to 0..1 values
    refScaled <- 1/diff(refLim) * (ag$ref - refLim[1])
    ## Vertical axis, corrected to 0..1 values
    valsScaled <- 1/diff(scaleAxis) * (ag$pz - scaleAxis[1])
    ## Extreme values (scaled) without NA
    rVals <- range(valsScaled, na.rm = TRUE)
    rRef <- range(refScaled, na.rm = TRUE)
    ## Create grobs
    ## Polygon
    pol <- switch(side,
                  x = polygonGrob(y = c(0, valsScaled, 0),
                      x = c(rRef[1], refScaled, rRef[2]), 
                      gp = gpar(col = "black", fill = 'grey')),
                  y = polygonGrob(x = c(0, valsScaled, 0),
                      y = c(rRef[1], refScaled, rRef[2]), 
                      gp = gpar(col = "black", fill = 'grey'))
                  )
    ## Layout
    lg <- switch(side,
                 x = grid.layout(nrow = 1, ncol = 1,
                     widths = unit(1, 'null'),
                     heights = unit(3, 'lines')),
                 y = grid.layout(nrow = 1, ncol = 1,
                     heights= unit(1, 'null'),
                     widths = unit(3, 'lines'))
                 )
    ## Frame
    fg <- frameGrob(layout = lg,
                    name = paste0('legend', side))

    ## Axis is disabled by default
    gpAxis <- gpar(col = 'darkgrey', fontsize = 7)
    if (is.list(axis.margin)) {
        gpAxis <- modifyList(gpAxis, axis.margin)
        axis.margin <- TRUE
    }
    if (isTRUE(axis.margin)) {
        axis <- switch(side,
                       x = linesGrob(rRef[1], c(0, 1),
                           gp = gpAxis),
                       y =   linesGrob(c(0, 1), rRef[2],
                           gp = gpAxis)
                       )
    
        ticks <- switch(side,
                        x = segmentsGrob(x0 = rRef[1], y0 = c(0,1),
                            x1 = unit(rRef[1], 'npc') - unit(.5, 'char'),
                            y1 = c(0, 1),
                            gp = gpAxis),
                        y = segmentsGrob(c(0, 1), rRef[2],
                            c(0, 1),
                            unit(rRef[2], 'native') + unit(.5, 'char'),
                            gp = gpAxis)
                        )

        labText <- prettyNum(scaleAxis, digits = 2)
        labs <- switch(side,
                       x = textGrob(labText,
                           x = unit(rRef[1], 'npc') - unit(1, 'lines'),
                           y = unit(c(0, 1), 'npc'),
                           just = 'right',
                           gp = gpAxis),
                       y = textGrob(labText,
                           x = c(0, 1),
                           y = unit(rRef[2], 'npc') + unit(1.5, 'lines'),
                           just = 'left',
                           gp = gpAxis)
                       )

        fg <- placeGrob(fg, axis, row = 1, col = 1)
        fg <- placeGrob(fg, ticks, row = 1, col = 1)
        fg <- placeGrob(fg, labs, row = 1, col = 1)
    }
    ## Final result
    fg <- placeGrob(fg, pol, row = 1, col = 1)
    fg
}

legendArrow <- function(size = 1, unitLab='', keyScale)
{
    ## Layout
    lg <- grid.layout(nrow = 1, ncol = 1,
                      widths = unit(1, 'null'),
                      heights = unit(3, 'lines'))
    ## Frame
    fg <- frameGrob(layout = lg,
                    name = 'arrowKey')
    ## Label
    labGrob <-  textGrob(paste(size, unitLab),
                           x = 0, y = 0.1,
                           just = c('left', 'bottom'),
                         gp = gpar(col = 'black', cex = 0.8))
    ## Arrow
    arrGrob <- segmentsGrob(x0 = 0, y0 = 0.05,
                            x1 = size * keyScale, y1 = 0.05,
                            arrow = arrow(length=unit(5e-2, 'npc')),
                            default.units = 'native')
    ## Place objects
    fg <- placeGrob(fg, arrGrob, row = 1, col = 1)
    fg <- placeGrob(fg, labGrob, row = 1, col = 1)
    fg
}

