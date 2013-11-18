setGeneric('hexbinplot')

setMethod('hexbinplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme(),
            ...){
            idx=getZ(data)

            nms <- names(data)

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
