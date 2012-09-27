# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  August 2012
# Version 0.11
# Licence GPL v3

##xyplot for directions created with xyLayer
setGeneric('hexbinplot')

setMethod('hexbinplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme,
            ...){
            idx=getZ(data)
            ## names replace layerNames with raster version 2.0-04
            rasterVersion <- as.character(packageVersion('raster'))
            nms <- if (compareVersion(rasterVersion, '2.0-04') == -1) layerNames(data) else names(data)

            nl <- nlayers(data)
            xLayer <- getValues(init(data, v='x'))
            yLayer <- getValues(init(data, v='y'))

            ## data <- sampleRandom(data, maxpixels)
            df <- getValues(data)
            df <- as.data.frame(df)
            names(df) <- make.names(nms)

            df <- cbind(data.frame(x=xLayer, y=yLayer), df)

            if (!missing(dirXY)) {
              dirXY <- getValues(xyLayer(data, dirXY=substitute(dirXY)))
              df <- cbind(df, dirXY)
            }

            p <- hexbinplot(x, df,
                            xscale.components=xscale.components,
                            yscale.components=yscale.components,                            
                            par.settings=par.settings, ...)
            p
          }
          )
