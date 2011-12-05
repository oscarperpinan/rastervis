# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('densityplot')
setMethod('densityplot',
          signature(x='RasterLayer', data='missing'),
          definition=function (x, data=NULL, maxpixels = 1e+05,
            xlab='', ylab='', main='', col='black',...){
            dat <- raster2dat(x, maxpixels=maxpixels)
            densityplot(dat,
                        data=NULL,
                        pch='.', col=col,
                        xlab=xlab, ylab=ylab, ...) 
          }
          )

  
setMethod('densityplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function (x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=rasterTheme,...){
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              p <- densityplot(~values,
                               data=dat, groups=ind,
                               ## scales=list(x=list(relation='free'),
                               ##   y=list(relation='free', draw=FALSE)),
                               breaks=100, par.settings=par.settings, pch='.',
                               xlab=xlab, ylab=ylab,
                               panel=panel.superpose,
                               panel.groups=function(x, group.value, col.line,...){
                                 panel.densityplot(x, col.line=col.line, plot.points=FALSE,...)
                                 d <- density(x, na.rm=1)
                                 i <- which.max(d$y)
                                 ltext(d$x[i],d$y[i],group.value,adj=c(0.3,0),col=col.line, cex=0.7)
                               }
                               )
            } else {
              p <- densityplot(x, maxpixels = maxpixels, main = main, xlab=xlab, ylab=ylab,...)
            }
            p
          }
          )

                 

setMethod('densityplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings=rasterTheme,...){

            nms <- layerNames(data)
            nl <- nlayers(data)

            data <- sampleRegular(data, maxpixels, asRaster=TRUE)
            df <- getValues(data)
            df <- as.data.frame(df)
            names(df) <- make.names(nms)

            xLayer <- getValues(init(data, v='x'))
            yLayer <- getValues(init(data, v='y'))

            df <- cbind(data.frame(x=xLayer, y=yLayer), df)

            if (!missing(dirXY)) {
              dirXY <- getValues(xyLayer(data, dirXY=substitute(dirXY)))
              df <- cbind(df, dirXY)
            }

            p <- densityplot(x=x, data=df,
                             xscale.components=xscale.components,
                             yscale.components=yscale.components,
                             auto.key = auto.key, 
                             par.settings=par.settings, ...)
            p
          }
            )
