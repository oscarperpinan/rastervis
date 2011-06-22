setGeneric('densityplot')
setMethod('densityplot',
          signature=c(x='RasterLayer', data='missing'),
          definition=function (x, maxpixels = 1e+05,
            xlab='', ylab='', main='', col='black',...){
            dat <- sampleRandom(x, maxpixels)
            densityplot(dat,
                        data=NULL,
                        pch='.', col=col,
                        xlab=xlab, ylab=ylab) 
          }
          )

  
setMethod('densityplot',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function (x, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=rasterTheme,...){
            if (!missing(layer)) x <- subset(x, layer)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              p <- densityplot(~values,
                               data=dat, groups=ind,
                               scales=list(x=list(relation='free'),
                                 y=list(relation='free', draw=FALSE)),
                               breaks=100, par.settings=par.settings, pch='.',
                               xlab=xlab, ylab=ylab,
                               panel=panel.superpose,
                               panel.groups=function(x, group.value, col.line,...){
                                 panel.densityplot(x, col.line=col.line, plot.points=FALSE,...)
                                 d <- density(x)
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

                 
