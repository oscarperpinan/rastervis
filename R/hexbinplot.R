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
              nms <- make.names(nms)
              
              if (is(data, "Raster"))
              {
                  nl <- nlayers(data)
                  df <- getValues(data)
                  xLayer <- getValues(xLayer)
                  yLayer <- getValues(yLayer)
                  
              } else
              {
                  nl <- nlyr(data)
                  df <- values(data)
                  xLayer <- values(xLayer)
                  yLayer <- values(yLayer)
                  
              }
              
              df <- as.data.frame(df)

              df <- cbind(data.frame(x=xLayer, y=yLayer), df)
              names(df) <- c("x", "y", nms)              

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
