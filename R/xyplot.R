globalVariables('group.value')


setGeneric('xyplot')
########################################################################
##xyplot for directions created with xyLayer
########################################################################

xyplotTime <- function(x, dirLayer, tt,
                       stat,
                       xlab, ylab,
                       digits,
                       par.settings,
                       auto.key, ...)
{
    ## zonal calculations defined by the direction
    z <- zonal(x, dirLayer, fun = stat, digits = digits)
    ## zonal returns a data.frame with terra objects and a matrix with
    ## raster objects.
    z <- as.matrix(z)
    nRows <- nrow(z)
    ## A time series is defined with the result of zonal and
    ## the z-slot of x
    zz <- as.data.frame(t(z[,-1]), row.names='')
    names(zz) <- z[,1]
    zz <- zoo(zz, order.by = tt)
    ## Finally, xyplot
    p <- xyplot(zz, xlab = xlab, ylab = ylab,
                superpose = TRUE, auto.key = auto.key,
                par.settings = par.settings, ...)
    if (is.list(auto.key) | isTRUE(auto.key))
        p
    else
        p +
            glayer(panel.text(x[1], y[1],
                              group.value, cex = 0.7))
}

setMethod('xyplot',
          signature(x = 'RasterStackBrick', data = 'missing'),
          definition = function(x, data = NULL, dirXY = y,
                                stat = 'mean',
                                xlab = 'Time', ylab = '',
                                digits = 0,
                                par.settings = rasterTheme(),
                                auto.key = FALSE, ...)
          {
              tt <- getZ(x)
              if (is.null(tt))
                  stop('z slot of the object is NULL. Use setZ.')

              dirLayer <- xyLayer(x,
                                  dirXY = substitute(dirXY),
                                  vector = FALSE)
              
              xyplotTime(x, dirLayer, tt,
                         stat,
                         xlab, ylab,
                         digits,
                         par.settings,
                         auto.key, ...)
          })

setMethod('xyplot',
          signature(x = 'SpatRaster', data = 'missing'),
          definition = function(x, data = NULL, dirXY = y,
                                stat = 'mean',
                                xlab = 'Time', ylab = '',
                                digits = 0,
                                par.settings = rasterTheme(),
                                auto.key = FALSE, ...)
          {
              tt <- time(x)
              if (is.null(tt))
                  stop('time index of the object is NULL. Use time().')

              dirLayer <- xyLayer(x,
                                  dirXY = substitute(dirXY),
                                  vector = FALSE)
              
              xyplotTime(x, dirLayer, tt,
                         stat,
                         xlab, ylab,
                         digits,
                         par.settings,
                         auto.key, ...)
          })

########################################################################
## xyplot for formula
########################################################################
xyplotFormula <- function(x, data,
                          dirXY, maxpixels,
                          alpha,
                          xscale.components, yscale.components,
                          par.settings, ...)
{
    df <- dfRegular(data, maxpixels)
    
    if (!missing(dirXY))
    {
        dirXY <- xyLayer(data,
                         dirXY = substitute(dirXY),
                         maxpixels = maxpixels)
        
        df <- cbind(df, dirXY)
    }
    
    isFactor <- which(is.factor(data))
    
    if (any(isFactor))
    {
        levelsData <- levels(data)[[isFactor]][[1]][,2]
        df[, isFactor + 2] <- as.factor(
            levelsData[df[, isFactor + 2]])
    }
    p <- xyplot(x = x, data = df,
                alpha = alpha,
                xscale.components = xscale.components,
                yscale.components = yscale.components,
                par.settings = par.settings, ...)
    p
}

setMethod('xyplot', signature(x = 'formula', data = 'Raster'),
          definition = function(x, data, dirXY, maxpixels = 1e5,
            alpha = 0.05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            par.settings = rasterTheme(), ...)
          {

              xyplotFormula(x, data,
                            dirXY, maxpixels,
                            alpha,
                            xscale.components, yscale.components,
                            par.settings, ...)
          }
          )

setMethod('xyplot', signature(x = 'formula', data = 'SpatRaster'),
          definition = function(x, data, dirXY, maxpixels = 1e5,
            alpha = 0.05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            par.settings = rasterTheme(), ...)
          {

              xyplotFormula(x, data,
                            dirXY, maxpixels,
                            alpha,
                            xscale.components, yscale.components,
                            par.settings, ...)
          }
          )
