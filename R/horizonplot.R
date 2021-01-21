setGeneric('horizonplot')

setMethod('horizonplot',
          signature(x='RSBoT', data='missing'),
          definition=function(x, data = NULL, dirXY = y, 
                              stat = 'mean', digits = 0,
                              origin = mean,
                              xlab = 'Time', ylab = 'direction',
                              colorkey = TRUE, colorkey.digits = 1,
                              scales = list(y = list(relation = "same")),
                              ...)
          {
              
              
              if (is(x, "SpatRaster"))
              {
                  tt <- time(x)
                  if (is.null(tt))
                      stop('time index of the object is NULL. Use time().')
              } else
              {
                  tt <- getZ(x)
                  if (is.null(tt))
                      stop('z slot of the object is NULL. Use setZ.')
              }
              
              ## zonal calculations defined by the direction
              dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
              z <- zonal(x, dirLayer, fun=stat, digits=digits)
              ## zonal returns a data.frame with terra objects and a matrix with
              ## raster objects.
              z <- as.matrix(z)
              nRows <- nrow(z)
              ## A time series is defined with the result of zonal and
              ## the z-slot of x
              zz <- as.data.frame(t(z[,-1]), row.names='')
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
          })
