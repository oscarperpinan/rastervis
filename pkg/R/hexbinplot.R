##xyplot for directions created with xyLayer
setGeneric('hexbinplot')

setMethod('hexbinplot', signature=c(x='formula', data='Raster'),
          definition=function(x, data, dirXY,
            par.settings=rasterTheme,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            ...){
            idx=getZ(x)
            nms <- layerNames(data)
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
