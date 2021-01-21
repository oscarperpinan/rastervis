setGeneric('hexbinplot')

setMethod('hexbinplot', signature(x='formula', data='RoT'),
          definition=function(x, data, dirXY,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme(),
            ...)
          {
              
              xLayer <- init(data, fun='x')
              yLayer <- init(data, fun='y')
              
              nms <- names(data)
              
              if (is(object, "Raster"))
              {
                  nl <- nlayers(data)
                  idx <- getZ(object)
                  if (is.null(idx))
                      stop('z slot of the object is NULL. Use setZ.')
                  df <- getValues(data)
                  xLayer <- getValues(xLayer)
                  yLayer <- getValues(yLayer)
                  
              } else
              {
                  nl <- nlyr(data)
                  idx <- time(object)
                  if (is.null(idx))
                      stop('time index of the object is NULL. Use time().')
                  
                  df <- values(data)
                  xLayer <- values(xLayer)
                  yLayer <- values(yLayer)
                  
              }
              
              df <- as.data.frame(df)
              names(df) <- make.names(nms)

              df <- cbind(data.frame(x=xLayer, y=yLayer), df)
              
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
              
              
              p <- hexbinplot(x, df,
                              xscale.components = xscale.components,
                              yscale.components = yscale.components,
                              par.settings = par.settings, ...)
              p
          }
          )
