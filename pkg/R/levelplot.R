setGeneric('levelplot')

setMethod('levelplot',
          signature='Raster',
          definition=function(x, layer,
            margin=TRUE, FUN.margin=mean,
            maxpixels=1e5,
            par.settings=rasterTheme,
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xlab='', ylab='', main='',
            scales=list(draw=TRUE),
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            colorkey=list(space='right'),
            ...) {
            
            if (!missing(layer)) {
              object <- subset(x, layer)
            } else {object <- x}
  
            dat <- sampleRegular(object, size=maxpixels, asRaster=TRUE)
            nms <- layerNames(dat)
            x <- xFromCell(dat, 1:ncell(dat))
            y <- yFromCell(dat, 1:ncell(dat))
            dat <- as.data.frame(getValues(dat))
            names(dat) <- nms
            df <- cbind(data.frame(x=x, y=y), dat)

  
            ##aspect and scales(from sp:::spplot.grid, sp:::longlat.scales, sp:::mapasp)
            bb <- extent(object)
            xlim=c(bb@xmin, bb@xmax)
            ylim=c(bb@ymin, bb@ymax)
  
            if (isLonLat(object)){
              xlab='Longitude'
              ylab='Latitude'
            
              aspect=(diff(ylim)/diff(xlim))/cos((mean(ylim) * pi)/180)
    
              if (!is.null(scales$draw) && scales$draw==TRUE){
                scales=list(x=list(at=pretty(xlim)), y=list(at=pretty(ylim)))
                scales$y$labels=parse(text=sp:::degreeLabelsNS(scales$y$at))
                scales$x$labels=parse(text=sp:::degreeLabelsEW(scales$x$at))
              }
            } else aspect='iso'
          
            ##formula
            form <- as.formula(paste(paste(names(df)[-c(1,2)], collapse='+'), 'x*y', sep='~'))

            if (nlayers(object)==1 && margin) {
              if ((is.logical(colorkey) && colorkey) || is.list(colorkey)){
                colorkey=list(space='bottom')
              } else {}
              if (is.function(par.settings)) par.settings <- par.settings()
              par.settings=modifyList(par.settings,
                list(
                     layout.widths=list(right.padding=10),
                     layout.heights=list(top.padding=10,
                       xlab.key.padding=3)
                     )
                )
            } else {}
              

            p <- levelplot(form, data=df,
                           scales=scales, aspect=aspect,
                           xlab=xlab, ylab=ylab,
                           par.settings=par.settings,
                           between=between,
                           as.table=as.table,
                           xscale.components=xscale.components,
                           yscale.components=yscale.components,
                           colorkey=colorkey,
                           ...)
            
            if (nlayers(object)==1 && margin) {
              update(p,
                     legend=list(
                       right=list(
                         fun=legendY, args=list(p, FUN.margin)),
                       top=list(
                         fun=legendX, args=list(p, FUN.margin))
                       )
                     )
            } else p
          }
            )

