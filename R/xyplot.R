globalVariables(c('group.value', 'x', 'y'))


setGeneric('xyplot')
########################################################################
##xyplot for directions created with xyLayer
########################################################################

xyplotTime <- function(z, tt,
                       xlab, ylab,
                       par.settings,
                       auto.key, ...)
{
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
              ## Time variable
              tt <- getZ(x)
              if (is.null(tt))
                  stop('z slot of the object is NULL. Use setZ.')

              dirLayer <- xyLayer(x,
                                  dirXY = substitute(dirXY),
                                  vector = FALSE)

              ## zonal calculations defined by the direction
              z <- raster::zonal(x, dirLayer, fun = stat, digits = digits)
              
              xyplotTime(z, tt,
                         xlab, ylab,
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
              ## zonal calculations defined by the direction
              z <- terra::zonal(x, dirLayer, fun = stat, digits = digits)
              ## zonal returns a data.frame with terra objects
              z <- as.matrix(z)
              ## Time variable
              tt <- time(x)
              if (is.null(tt))
                  stop('time index of the object is NULL. Use time().')

              dirLayer <- xyLayer(x,
                                  dirXY = dirXY,
                                  vector = FALSE)
              
              xyplotTime(z, tt,
                         xlab, ylab,
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
        df <- cbind(df, dirXY)
    
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

              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(data,
                                   dirXY = substitute(dirXY),
                                   maxpixels = maxpixels)
                  
              }
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
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(data,
                                   dirXY = substitute(dirXY),
                                   maxpixels = maxpixels)
              }

              xyplotFormula(x, data,
                            dirXY, maxpixels,
                            alpha,
                            xscale.components, yscale.components,
                            par.settings, ...)
          }
          )
