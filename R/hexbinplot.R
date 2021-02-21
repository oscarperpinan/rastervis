setGeneric('hexbinplot')

setMethod('hexbinplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels = 1e6,
                              xscale.components=xscale.raster,
                              yscale.components=yscale.raster,
                              par.settings=rasterTheme(),
            ...)
          {
              df <- dfRegular(data, maxpixels)
              
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(data,
                                   dirXY = substitute(dirXY),
                                   maxpixels = maxpixels)
                  
                  df <- cbind(df, dirXY)
              }
              
              
              p <- hexbinplot(x, df,
                              xscale.components = xscale.components,
                              yscale.components = yscale.components,
                              par.settings = par.settings, ...)
              p
          }
          )


setMethod('hexbinplot', signature(x='formula', data='SpatRaster'),
          definition=function(x, data, dirXY, maxpixels = 1e6,
                              xscale.components=xscale.raster,
                              yscale.components=yscale.raster,
                              par.settings=rasterTheme(),
            ...)
          {
              df <- dfRegular(data, maxpixels)
              
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(data,
                                   dirXY = substitute(dirXY),
                                   maxpixels = maxpixels)
                  
                  df <- cbind(df, dirXY)
              }
              
              
              p <- hexbinplot(x, df,
                              xscale.components = xscale.components,
                              yscale.components = yscale.components,
                              par.settings = par.settings, ...)
              p
          }
          )
