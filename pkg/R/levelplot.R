# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('levelplot')

setMethod('levelplot',
          signature(x='Raster', data='missing'),
          definition=function(x, data=NULL, layers,
            margin=TRUE, FUN.margin=mean,
            maxpixels=1e5,
            par.settings=rasterTheme,
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xlab='', ylab='', main='',
            scales=list(draw=TRUE),
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            zscaleLog=NULL,
            colorkey=list(space='right'),
            contour=FALSE, region=TRUE,
            ...) {
            
            if (!missing(layers)) {
              object <- subset(x, subset=layers)
            } else {object <- x}
  
            dat <- sampleRegular(object, size=maxpixels, asRaster=TRUE)
            nms <- make.names(layerNames(dat))

            ##Extract coordinates from (sampled) raster
            x <- xFromCell(dat, 1:ncell(dat))
            y <- yFromCell(dat, 1:ncell(dat))
          
            
            ## Extract values 
            dat <- getValues(dat)

            ## If zscale is not NULL, transform the values and choose a function
            ## to calculate the labels
            if (!is.null(zscaleLog)){
              zlogbase <- if (is.logical(zscaleLog)) {
                10
              } else {
                if (is.numeric(zscaleLog)) {
                  zscaleLog
                } else {
                  if (zscaleLog == "e") {
                    exp(1)
                  }
                }}
              if (zlogbase==10) {
                zscale.components <- zscale.components.log10ticks
              } else {
                zscale.components <- zscale.components.logpower
              }
              dat <- log(dat, zlogbase)
            }
            ## Calculate the range (for zscale.components)
            lim <- range(dat, finite=TRUE)
            
            ## Convert to a data.frame for conventional levelplot  
            dat <- as.data.frame(dat)
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

            if (region==FALSE) colorkey=FALSE
           
            has.colorkey <- (is.logical(colorkey) && colorkey) || is.list(colorkey)
            if (region==TRUE && !has.colorkey) colorkey=has.colorkey=TRUE
            has.margin <- nlayers(object)==1 && margin
            has.contour <- (contour==TRUE) ##(!missing(contour) && contour==TRUE)

            ## Build the zscale.components and colorkey paying attention to zscaleLog
            if (!is.null(zscaleLog)){
              zscale <- zscale.components(lim, zlogbase)
              if (has.colorkey){
                colorkey.default=list(labels=zscale, raster=TRUE, interpolate=TRUE)
                if (is.logical(colorkey)){
                  colorkey=colorkey.default
                } else {
                  colorkey=modifyList(colorkey, colorkey.default)
                }
              }
              if (has.contour){
                ## panel.mixed=function(..., contour, region), at=zscale$at,
                ##   labels=list(labels=zscale$labels, cex=0.7), lwd=0.5){
                ##   if (region) panel.levelplot(..., contour=FALSE, labels=FALSE)
                ##   panel.contourplot(..., at=at, labels=labels, lwd=lwd,
                ##                     region=FALSE, contour=TRUE)
                panel.mixed <- function(...){panel.contourplot(...)
                }
              }
            }
            
            ## Some fixes for the margin
            if (has.margin && has.colorkey){
              if (is.logical(colorkey)){
                colorkey=list(space='bottom')                
              } else {
                colorkey=modifyList(colorkey, list(space='bottom'))
              }

              if (is.function(par.settings)) par.settings <- par.settings()
              par.settings=modifyList(par.settings,
                list(
                     layout.widths=list(right.padding=10),
                     layout.heights=list(top.padding=10,
                       xlab.key.padding=3)
                     )
                )
            } else {}

            ## Build the formula for levelplot
            form <- as.formula(paste(paste(names(df)[-c(1,2)], collapse='+'), 'x*y', sep='~'))

            ## And finally, the levelplot call
            p <- levelplot(form, data=df,
                           scales=scales, aspect=aspect,
                           xlab=xlab, ylab=ylab,
                           par.settings=par.settings,
                           between=between,
                           as.table=as.table,
                           xscale.components=xscale.components,
                           yscale.components=yscale.components,
                           colorkey=colorkey,
                           contour=contour, region=region,
                           strip=strip.custom(factor.levels=layerNames(object)),
                           panel=if(!is.null(zscaleLog) && has.contour) {
                             panel.mixed
                           } else {
                             panel.levelplot},
                           ...)
            ## with the margins if needed
            if (nlayers(object)==1 && margin) {
              update(p,
                     legend=list(
                       right=list(
                         fun=legendY, args=list(p, FUN.margin)),
                       top=list(
                         fun=legendX, args=list(p, FUN.margin))
                       ))
            } else p
          }
            )
