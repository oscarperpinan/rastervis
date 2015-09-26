if (!isGeneric("plot3D")) {
	setGeneric("plot3D", function(x,...)
		standardGeneric("plot3D"))
}	

setMethod("plot3D", signature(x='RasterLayer'), 
          function(x, maxpixels=1e5, zfac=1, drape=NULL,
                   col=terrain.colors, at=100, rev=FALSE,
                   useLegend=TRUE,
                   adjust=TRUE, ...) { 
              ## much of the below code was taken from example(surface3d) in the rgl package
              if (requireNamespace("rgl", quietly = TRUE)){

                  x <- sampleRegular(x, size=maxpixels, asRaster=TRUE)
                  X <- xFromCol(x,1:ncol(x))
                  Y <- yFromRow(x, nrow(x):1)
                  Z <- t((getValues(x, format='matrix'))[nrow(x):1,])
                  
                  background <- min(Z, na.rm=TRUE) - 1
                  Z[is.na(Z)] <- background
                  
                  zlim <- range(Z)
                  zlen <- zlim[2] - zlim[1] + 1
                  xlen <- max(X) - min(X)
                  ylen <- max(Y) - min(Y)
                  if (adjust) {
                      adj <- 4*zlen/min(ylen,xlen)
                      X <- X * adj
                      Y <- Y * adj
                  } 
                  
                  if (!is.null(drape)){
                      x <- sampleRegular(drape, size=maxpixels, asRaster=TRUE)
                      Zcol <- t((getValues(x, format='matrix'))[nrow(x):1,])
                      background <- min(Zcol, na.rm=TRUE) - 1
                      Zcol[is.na(Zcol)] <- background
                      zlim <- range(Zcol)
                  } else {
                      Zcol <- Z
                  }
                  colorTable <- x@legend@colortable
                  if (useLegend & length(colorTable)>1) {
                      color <- colorTable
                  } else {
                      if (length(at)==1) at <- do.breaks(zlim, at)
                      if (rev) {
                          if (is.function(col)) {
                              col <- rev(col(length(at)))
                          } else {
                              col <- rev(col)
                          }
                      }
                      color <- level.colors(Zcol, at=at, col.regions=col)
                  }
                  ## Open a device only if there is none active
                  if (rgl::rgl.cur() == 0) rgl::open3d()
                  
                  if (background==min(Zcol)) {
                      trans <- Zcol
                      trans[] <- 1.0
                      trans[Zcol==background] <- 0
                      rgl::surface3d(X, Y, Z*zfac, color=color, back="lines", alpha=trans, ...)
                  } else {
                      rgl::surface3d(X, Y, Z*zfac, color=color, back="lines", ...)
                  }
              } else stop("to use this function you need to install the 'rgl' package")
              }
              )
