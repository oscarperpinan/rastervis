globalVariables('group.value')

##xyplot for directions created with xyLayer
setGeneric('xyplot')

setMethod('xyplot',
          signature(x = 'RSBoT', data = 'missing'),
          definition = function(x, data = NULL, dirXY = y,
                                stat = 'mean',
                                xlab = 'Time', ylab = '',
                                digits = 0,
                                par.settings = rasterTheme(),
                                auto.key = FALSE, ...){

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
              ## Finally, xyplot
              p <- xyplot(zz, xlab=xlab, ylab=ylab,
                          superpose=TRUE, auto.key = auto.key,
                          par.settings=par.settings, ...)
              if (is.list(auto.key) | isTRUE(auto.key))
                  p
              else
                  p +
                      glayer(panel.text(x[1], y[1],
                                        group.value, cex = 0.7))
          }
          )


setMethod('xyplot', signature(x = 'formula', data = 'RoT'),
          definition=function(x, data, dirXY, maxpixels = 1e5,
            alpha = 0.05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            par.settings = rasterTheme(), ...)
          {
                            
              if (is(data, "SpatRaster"))
              {
                  dataSample <- spatSample(data,
                                           size = maxpixels,
                                           as.raster = TRUE)
                  xLayer <- init(dataSample, fun='x')
                  yLayer <- init(dataSample, fun='y')

                  df <- values(dataSample)
                  xLayer <- values(xLayer)
                  yLayer <- values(yLayer)
              }
              else 
                {
                    dataSample <- sampleRegular(data,
                                                size = maxpixels,
                                                asRaster = TRUE)
                    xLayer <- init(dataSample, fun = 'x')
                    yLayer <- init(dataSample, fun = 'y')

                    df <- getValues(dataSample)
                    xLayer <- getValues(xLayer)
                    yLayer <- getValues(yLayer)
                }
                  
              df <- as.data.frame(df)
              names(df) <- nms
              
              df <- cbind(data.frame(x = xLayer, y = yLayer), df)
              
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(dataSample,
                                   dirXY = substitute(dirXY))

                  if (is(data, "Raster"))
                      dirXY <- getValues(dirXY)
                  else
                      dirXY <- values(dirXY)

                  df <- cbind(df, dirXY)
              }

              isFactor <- which(is.factor(data))
              levelsData <- levels(data)[[isFactor]][[1]][,2]
              
              if (any(isFactor))
              {
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
          )
