setGeneric('identifyRaster', function(object, ...){standardGeneric('identifyRaster')})

setMethod('identifyRaster', signature=(object='Raster'),
          definition=function(object, layer=1, values=FALSE, pch=13, cex=0.6, col='black',...){
            lay <- layer[1]
            if (is.character(lay)) lay <- which(lay==layerNames(object))
            if (length(lay)<1 || lay > nlayers(object)) stop('Incorrect value of layer.')
            prefix <- lattice:::lattice.getStatus('current.prefix')
            ll <- lattice:::lattice.getStatus('current.panel.positions', prefix=prefix)
            trellisObject <- trellis.last.object()
            pnl <- which(ll==lay, arr.ind=TRUE)
            trellis.focus('panel', column=pnl[2], row=pnl[1], ...)
            trellisType <- as.character(trellisObject$call)[1]
            if (trellisType=='splom'){
              idx <- panel.link.splom(pch=pch, cex=cex, col=col,...)
            } else {
              lbl <- round(getValues(object), 2)[,lay]
              subs <- seq_len(ncell(object))
              idx <- panel.identify(subscripts=subs, label=lbl, pch=pch, cex=cex, col=col,...)
            }
            trellis.unfocus()
            if (values) return(suppressWarnings(extract(object, idx))) else return(idx)
          }
          )



chooseRegion <- function(sp=TRUE, proj=CRS('+proj=latlon +ellps=WGS84')){
  trellis.focus('panel', 1, 1)
  x <- trellis.panelArgs()$x
  y <- trellis.panelArgs()$y
  xy <- xy.coords(x, y, recycle = TRUE)
  x <- xy$x
  y <- xy$y
  px <- convertX(unit(x, "native"), "points", TRUE)
  py <- convertY(unit(y, "native"), "points", TRUE)
  pointsData <- cbind(px, py)

  border <- as.numeric()

  while (TRUE){
    ll <- grid.locator(unit='native')
    if (!is.null(ll)){
      lpoints(ll, col='black', cex=0.7, pch=3)
      lx <- convertX(unit(ll$x, 'native'), 'points', FALSE)
      ly <- convertY(unit(ll$y, 'native'), 'points', FALSE)
      border <- rbind(border, c(lx, ly))
    } else {    
      break
    }
  }
  trellis.unfocus()  

  inside <- in.out(border, pointsData)
  pointsInside <- SpatialPoints(coords=data.frame(xin=x[inside], yin=y[inside]), proj4string=proj)

  spPoints <- list('sp.points', pointsInside, cex=0.5)
  print(update(trellis.last.object(), sp.layout=spPoints))
  
  if (sp) return(pointsInside) else return(inside)
}
