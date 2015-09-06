globalVariables('group.value')

##xyplot for directions created with xyLayer
setGeneric('xyplot')

setMethod('xyplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data=NULL, dirXY=y, stat='mean',
            xlab='Time', ylab='', digits=0,
            par.settings=rasterTheme(),...){

            idx=getZ(x)
            if (is.null(idx)) stop('z slot of the object is NULL.')

            dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, stat, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- xyplot(zz, xlab=xlab, ylab=ylab,
                        superpose=TRUE, auto.key=FALSE,
                        par.settings=par.settings, ...)
            p + glayer(panel.text(x[1], y[1], group.value, cex=0.7))
          }
          )


setMethod('xyplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e5,
            alpha=0.05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme(),...){

            isFactor <- which(is.factor(data))
            levelsData <- levels(data)[[isFactor]][[1]][,2]
            
            if (!missing(dirXY)) {
              dirXY <- xyLayer(data, dirXY=substitute(dirXY))
              names(dirXY) <- 'dirXY'
              data <- stack(data, dirXY)
            }

            df <- as.data.frame(sampleRegular(data,
                                              maxpixels,
                                              xy=TRUE))

            if (any(isFactor)){

                df[, isFactor + 2] <- as.factor(
                    levelsData[df[, isFactor + 2]])
            }

            p <- xyplot(x=x, data=df,
                        alpha=alpha,
                        xscale.components=xscale.components,
                        yscale.components=yscale.components,
                        par.settings=par.settings, ...)
            p
          }
          )
