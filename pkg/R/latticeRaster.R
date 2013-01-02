
                                        # Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

##Customization of lattice
xscale.raster <- function(lim, ...){
  ans <- xscale.components.default(lim, ...)
  ans$top=FALSE
  ans}

yscale.raster <- function(lim, ...){
  ans <- yscale.components.default(lim, ...)
  ans$right=FALSE
  ans}


xscale.raster.subticks <- function(lim, ...){
  ans <- xscale.components.subticks(lim, ...);
  ans$top=FALSE;
  ans}

yscale.raster.subticks <- function(lim, ...){
  ans <- yscale.components.subticks(lim, ...)
  ans$right=FALSE
  ans}

xscale.raster.EW <- function(lim, ...){
  ans <- xscale.components.default(lim, ...);
  ans$bottom$labels$labels <- parse(text=sp:::degreeLabelsEW(as.numeric(
                                      ans$bottom$labels$labels)))
  ans$top=FALSE  
  ans}

xscale.raster.EWsubticks <- function(lim, ...){
  ans <- xscale.components.subticks(lim, ...)
  idx <- (ans$bottom$labels$labels!=' ')
  ans$bottom$labels$labels[idx] <- parse(text=sp:::degreeLabelsEW(as.numeric(
                                           ans$bottom$labels$labels[idx])))
  ans$top=FALSE  
  ans}

xscale.raster.NS <- function(lim, ...){ ## useful for hovmoller
  ans <- xscale.components.default(lim, ...);
  ans$bottom$labels$labels <- parse(text=sp:::degreeLabelsNS(as.numeric(ans$bottom$labels$labels)))
  ans$top=FALSE  
  ans}

xscale.raster.NSsubticks <- function(lim, ...){
  ans <- xscale.components.subticks(lim, ...)
  idx <- (ans$bottom$labels$labels!=' ')
  ans$bottom$labels$labels[idx] <- parse(text=sp:::degreeLabelsNS(as.numeric(
                                           ans$bottom$labels$labels[idx])))
  ans$top=FALSE  
  ans}

yscale.raster.NS <- function(lim, ...){
  ans <- yscale.components.default(lim, ...);
  ans$left$labels$labels <- parse(text=sp:::degreeLabelsNS(as.numeric(ans$left$labels$labels)))
  ans$right=FALSE  
  ans}

yscale.raster.NSsubticks <- function(lim, ...){
  ans <- yscale.components.subticks(lim, ...)
  idx <- (ans$left$labels$labels!=' ')
  ans$left$labels$labels[idx] <- parse(text=sp:::degreeLabelsNS(as.numeric(
                                           ans$left$labels$labels[idx])))
  ans$right=FALSE  
  ans}


rasterTheme <- function(pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')), ...) {
  theme <- custom.theme.2(pch=pch, cex=cex, region=region, ...)
  theme$strip.background$col='transparent'
  theme$strip.shingle$col='transparent'
  theme$strip.border$col='transparent'
  theme$add.line$lwd=.4
  theme
}

RdBuTheme <- function(region=brewer.pal(9, 'RdBu'), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

BuRdTheme <- function(region=rev(brewer.pal(9, 'RdBu')), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

PuOrTheme <- function(region=brewer.pal(9, 'PuOr'), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

GrTheme <- function(region=rev(brewer.pal(9, 'Greys')), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

BTCTheme <- function(region=BTC(n=9), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

streamTheme <- function(region='black',
                        symbol=brewer.pal(n=5, name='Blues'),
                        alpha=0.6, 
                        panel.background=list(col='gray20'),
                        ...){
  theme <- rasterTheme(region=region, symbol=symbol, ...)
  theme <- modifyList(theme, list(panel.background=panel.background))
  theme
}

##Auxiliary function for densityplot, histogram and bwplot
raster2dat <- function(x, FUN, maxpixels){
  nl <- nlayers(x)
  if (maxpixels < ncell(x)) {
    dat <- sampleRandom(x, maxpixels)
  } else {
    dat <- getValues(x)
  }
  if (nl>1){
    dat <- as.data.frame(dat)
    ##http://r.789695.n4.nabble.com/Column-order-in-stacking-unstacking-td3349953.html
    idx <- sprintf("%s%03d", "X", 1:nl) 
    names(dat) <- idx
    dat <- stack(dat)
    z <- getZ(x)
    if (!missing(FUN) & !is.null(z)){
      FUN <- match.fun(FUN)   
      dat$ind <- factor(FUN(z))[dat$ind]
    } else {
      ## names replace layerNames with raster version 2.0-04
      rasterVersion <- as.character(packageVersion('raster'))
      nms <- if (compareVersion(rasterVersion, '2.0-04') == -1) layerNames(x) else names(x)

      nms <- reorder(factor(nms), 1:nl)
      dat$ind <- nms[dat$ind]
    }
    dat
  } else {
    dat ##nl==1 --> raster2dat gives a vector 
  }
}
