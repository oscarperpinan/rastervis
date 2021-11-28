setGeneric('horizonplot')

horizon <- function(z, dirLayer, tt,
                    stat, digits,
                    origin,
                    xlab, ylab,
                    colorkey, colorkey.digits,
                    scales,
                    ...)
{
    nRows <- nrow(z)
    ## A time series is defined with the result of zonal and
    ## the z-slot of x
    zz <- as.data.frame(t(z[,-1]), row.names = '')
    names(zz) <- z[,1]
    zz <- zoo(zz, order.by = tt)
    ## latticeExtra::horizonplot requires that 'origin' is a
    ## number or a function
    if (is.character(origin)) origin <- match.fun(origin)
    ## Ready to plot
    p <- horizonplot(zz, xlab = xlab, ylab = ylab,
                     layout = c(1, nRows), 
                     colorkey = colorkey, 
                     colorkey.digits = colorkey.digits,
                     origin = origin,
                     scales = scales,
                     par.settings = rasterTheme(),
                     ...)
    p
}

setMethod('horizonplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data = NULL, dirXY = y, 
                              stat = 'mean', digits = 0,
                              origin = mean,
                              xlab = 'Time', ylab = 'direction',
                              colorkey = TRUE, colorkey.digits = 1,
                              scales = list(y = list(relation = "same")),
                              ...)
          {
              dirLayer <- xyLayer(x, substitute(dirXY), vector = FALSE)
              
              ##Calculate the matrix with the zonal function
              z <- raster::zonal(x, dirLayer, fun = stat, digits = digits)
              
              tt <- getZ(x)
              if (is.null(tt))
                  stop('z slot of the object is NULL. Use setZ.')


              horizon(z, dirLayer, tt,
                    stat, digits,
                    origin,
                    xlab, ylab,
                    colorkey, colorkey.digits,
                    scales,
                    ...)
          })


setMethod('horizonplot',
          signature(x='SpatRaster', data='missing'),
          definition=function(x, data = NULL, dirXY = y, 
                              stat = 'mean', digits = 0,
                              origin = mean,
                              xlab = 'Time', ylab = 'direction',
                              colorkey = TRUE, colorkey.digits = 1,
                              scales = list(y = list(relation = "same")),
                              ...)
          {
              dirLayer <- xyLayer(x, substitute(dirXY), vector = FALSE)
              
              ## zonal calculations defined by the direction
              z <- terra::zonal(x, dirLayer, fun = stat, digits = digits)
              ## zonal returns a data.frame with terra objects
              z <- as.matrix(z)

              tt <- time(x)
              if (is.null(tt))
                  stop('time index of the object is NULL. Use time().')

              horizon(z, dirLayer, tt,
                    stat, digits,
                    origin,
                    xlab, ylab,
                    colorkey, colorkey.digits,
                    scales,
                    ...)
          })
