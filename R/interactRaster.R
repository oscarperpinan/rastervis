## chooseRegion: no visible binding for global variable ‘xin’
## chooseRegion: no visible binding for global variable ‘yin’
globalVariables(c('xin', 'yin'))

setGeneric('identifyRaster', function(object, ...){standardGeneric('identifyRaster')})

setMethod('identifyRaster', signature(object='Raster'),
          definition=function(object, layer=1, values=FALSE, pch=13, cex=0.6, col='black',...){
            lay <- layer[1]
            nl <- nlayers(object)

            objNames <- names(object)

            if (is.character(lay)) lay <- which(lay==objNames)
            if (length(lay)<1 || lay > nl) stop('Incorrect value of layer.')
            prefix <- lattice:::lattice.getStatus('current.prefix')
            ll <- lattice:::lattice.getStatus('current.panel.positions', prefix=prefix)
            trellisObject <- trellis.last.object()
            pnl <- which(ll==lay, arr.ind=TRUE)
            trellis.focus('panel', column=pnl[2], row=pnl[1], ...)
            trellisType <- as.character(trellisObject$call)[1]
            if (trellisType=='splom'){
              idx <- panel.link.splom(pch=pch, cex=cex, col=col,...)
            } else {
              vals <- round(getValues(object), 2)
              if (nl==1) {
                lbl <- vals
              } else {
                lbl <- vals[,lay]
              }
              subs <- seq_len(ncell(object))
                idx <- panel.identify(subscripts = subs, labels = lbl,
                                      pch = pch, cex = cex, col = col,...)
            }
            trellis.unfocus()
            if (values) return(suppressWarnings(extract(object, idx))) else return(idx)
          }
          )



chooseRegion <- function(sp=TRUE, proj=as.character(NA)){
    trellis.focus('panel', 1, 1)
    x <- trellis.panelArgs()$x
    y <- trellis.panelArgs()$y
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    bx <- as.numeric()
    by <- as.numeric()
    
    while (TRUE){
        ll <- grid.locator(unit='native')
        if (!is.null(ll)){
            lpoints(ll, col='black', cex=0.7, pch=3)
            lx <- convertX(unit(ll$x, 'native'), 'points', FALSE)
            ly <- convertY(unit(ll$y, 'native'), 'points', FALSE)
            bx <- c(bx, lx)
            by <- c(by, ly)
        } else {    
            break
        }
    }
    trellis.unfocus()  
      
    testPiP<- point.in.polygon(px, py, bx, by)
    inside <- (testPiP != 0)
    pointsInside <- data.frame(xin=x[inside], yin=y[inside])
    spPoints <- SpatialPoints(coords=pointsInside, proj4string=CRS(proj))
      
    print(trellis.last.object() + layer(lpoints(xin, yin), data=pointsInside))

    if (isTRUE(sp)) return(spPoints) else return(pointsInside)
}
