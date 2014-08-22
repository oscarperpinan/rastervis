legendX <- function(p, FUN, scale.x, add.axis, ...){
  px <- p$panel.args.common$x
  py <- p$panel.args.common$y
  pz <- p$panel.args.common$z
  lim <- p$x.limits
  ag <- aggregate(pz ~ px, FUN = FUN)
  if (is.null(scale.x)) scale.x <- range(ag$pz, na.rm = TRUE)

  ## Horizontal axis, corrected to 0..1 values
  ref <- 1/diff(lim) * (ag$px - lim[1])
  ## Vertical axis, corrected to 0..1 values
  vals <- 1/diff(scale.x) * (ag$pz - scale.x[1])
  ## Extreme values without NA
  rVals <- range(vals, na.rm = TRUE)
  rRef <- range(ref, na.rm = TRUE)
  
  pol <- polygonGrob(y = c(rVals[1], vals, rVals[1]),
                     x = c(rRef[1], ref, rRef[2]), 
                     name = 'polX',
                     gp = gpar(col = "black", fill = 'grey'))

  axis <- linesGrob(rRef[1], c(0, 1),
                    name = "axisLegendX",
                    gp = gpar(col = 'darkgrey'))

  ticks <- segmentsGrob(x0 = rRef[1], y0 = c(0,1),
                        x1 = unit(rRef[1], 'npc') - unit(.5, 'char'),
                        y1 = c(0, 1),
                        name = "ticksLegendX",
                        gp = gpar(col = 'darkgrey'))

  labText <- prettyNum(scale.x, digits = 2)##round(scale.x, 3)
  labs <- textGrob(labText,
                   x = unit(rRef[1], 'npc') - unit(1, 'lines'),
                   y = unit(c(0, 1), 'npc'),
                   just = 'right',
                   name = 'ticksLegendX',
                   gp = gpar(fontsize = 7, col = 'darkgrey'))
  
  lg <- grid.layout(nrow = 1, ncol = 1,
                    widths = unit(1, 'null'),
                    heights = unit(3, 'lines'))
  fg <- frameGrob(layout = lg, name = 'legendX')

  ## Axis is disabled by default
  if (isTRUE(add.axis)) {
      fg <- placeGrob(fg, axis, row = 1, col = 1)
      fg <- placeGrob(fg, ticks, row = 1, col = 1)
      fg <- placeGrob(fg, labs, row = 1, col = 1)
  }
  ## Final result
  fg <- placeGrob(fg, pol, row = 1, col = 1)
  fg

}



legendY <- function(p, FUN, scale.y, add.axis, ...){
  px <- p$panel.args.common$x
  py <- p$panel.args.common$y
  pz <- p$panel.args.common$z
  lim <- p$y.limits
  ag <- aggregate(pz~py, FUN=FUN)
  if (is.null(scale.y)) scale.y <- range(ag$pz, na.rm = TRUE)
  
  ref <- 1/diff(lim) * (ag$py - lim[1])
  vals <- 1/diff(scale.y) * (ag$pz - scale.y[1])

  rVals <- range(vals, na.rm = TRUE)
  rRef <- range(ref, na.rm = TRUE)

  pol <- polygonGrob(x = c(rVals[1], vals, rVals[1]),
                     y = c(rRef[1], ref, rRef[2]), 
                     name = 'polY',
                     gp = gpar(col = "black", fill = 'grey'))

  axis <- linesGrob(c(0, 1), rRef[2],
                    name = "axisLegendY",
                    gp = gpar(col = 'darkgrey'))

  ticks <- segmentsGrob(c(0, 1), rRef[2],
                        c(0, 1), unit(rRef[2], 'native') + unit(.5, 'char'),
                        name = "ticksLegendY",
                        gp = gpar(col = 'darkgrey'))

  labText <- prettyNum(scale.y, digits = 2)##round(scale.y, 3)
  labs <- textGrob(labText,
                   x = c(0, 1), y = unit(rRef[2], 'npc') + unit(1.5, 'lines'),
                   just = 'left',
                   name = 'ticksLegendY',
                   gp = gpar(fontsize = 7, col = 'darkgrey'))
  
  lg <- grid.layout(nrow = 1, ncol = 1,
                    heights= unit(1, 'null'),
                    widths = unit(3, 'lines'))
  fg <- frameGrob(layout = lg, name = 'legendY')

  if (isTRUE(add.axis)) {
      fg <- placeGrob(fg, axis, row = 1, col = 1)
      fg <- placeGrob(fg, ticks, row = 1, col = 1)
      fg <- placeGrob(fg, labs, row = 1, col = 1)
  }
  fg <- placeGrob(fg, pol, row = 1, col = 1)
  fg
}
