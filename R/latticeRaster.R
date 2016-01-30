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

degreeLabelsEW2 <- function(x){
    x <- ifelse(x < -180, x + 360, x)
    x <- ifelse(x > 180, x - 360, x)
    pos = sign(x) + 2
    if (any(x == -180)) 
        pos[x == -180] = 2
    if (any(x == 180)) 
        pos[x == 180] = 2
    dir = c("*W", "", "*E")
    paste(abs(x), "*degree", dir[pos])    
    }

xscale.raster.EW <- function(lim, ...){
  ans <- xscale.components.default(lim, ...)
  labs <- as.numeric(ans$bottom$labels$labels)
  ans$bottom$labels$labels <- parse(text=degreeLabelsEW2(labs))
  ans$top=FALSE  
  ans}

xscale.raster.EWsubticks <- function(lim, ...){
    ans <- xscale.components.subticks(lim, ...)
    idx <- (ans$bottom$labels$labels!=' ')
    labs <- as.numeric(ans$bottom$labels$labels[idx])
    ans$bottom$labels$labels[idx] <- parse(text=degreeLabelsEW2(labs))
    ans$top=FALSE  
    ans}

xscale.raster.NS <- function(lim, ...){ ## useful for hovmoller
  ans <- xscale.components.default(lim, ...)
  labs <- as.numeric(ans$bottom$labels$labels)
  ans$bottom$labels$labels <- parse(text=degreeLabelsNS(labs))
  ans$top=FALSE  
  ans}

xscale.raster.NSsubticks <- function(lim, ...){
  ans <- xscale.components.subticks(lim, ...)
  idx <- (ans$bottom$labels$labels!=' ')
  labs <- as.numeric(ans$bottom$labels$labels[idx])
  ans$bottom$labels$labels[idx] <- parse(text=degreeLabelsNS(labs))
  ans$top=FALSE  
  ans}

yscale.raster.NS <- function(lim, ...){
  ans <- yscale.components.default(lim, ...)
  labs <- as.numeric(ans$left$labels$labels)
  ans$left$labels$labels <- parse(text=degreeLabelsNS(labs))
  ans$right=FALSE  
  ans}

yscale.raster.NSsubticks <- function(lim, ...){
  ans <- yscale.components.subticks(lim, ...)
  idx <- (ans$left$labels$labels!=' ')
  labs <- as.numeric(ans$left$labels$labels[idx])
  ans$left$labels$labels[idx] <- parse(text=degreeLabelsNS(labs))
  ans$right=FALSE  
  ans}


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
      nms <- names(x)
      nms <- reorder(factor(nms), 1:nl)
      dat$ind <- nms[dat$ind]
    }
    dat
  } else {
    dat ##nl==1 --> raster2dat gives a vector 
  }
}
