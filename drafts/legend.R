library(grid)


denX <- function(x,...){
    dens <- density(x)
    maxh <- max(dens$y)
    polygonGrob(x = dens$x, y = dens$y,
                 gp =
                 gpar(col = "transparent",
                      fill = "grey"),
                 default.units = "native",
                vp=viewport(clip='on',
                  yscale=c(-0.05, 1.05)*maxh,
                  width=unit(1, 'npc'),
                  height=unit(10, 'mm'),
                  xscale=range(x))
                )
}

denY <- function(x,...){
    dens <- density(x)
    maxh <- max(dens$y)
    polygonGrob(x = dens$y, y = dens$x,
                 gp =
                 gpar(col = "transparent",
                      fill = "grey"),
                 default.units = "native",
                vp=viewport(clip='on',
                  xscale=c(0, maxh),
                  height=unit(1, 'npc'),
                  width=unit(10, 'mm'),
                  yscale=range(x))
                )
}

histY <- function(x, lim, breaks=100,...){
    h <- hist(x, plot=FALSE, breaks=breaks)
    maxh <- max(h$counts)
    segmentsGrob(x0 = 0, y0 = h$mids,
                 x1=h$counts, y1=h$mids,
                 gp =
                 gpar(col = "black",
                      fill = "grey"),
                 default.units = "native",
                vp=viewport(clip='on',
                  xscale=c(0, maxh),
                  height=unit(1, 'npc'),
                  width=unit(10, 'mm'),
                  yscale=lim
                ))
}

histX <- function(x, lim, breaks=100,...){
    h <- hist(x, plot=FALSE, breaks=breaks)
    maxh <- max(h$counts)
    segmentsGrob(y0 = 0, x0 = h$mids,
                 y1=h$counts, x1=h$mids,
                 gp =
                 gpar(col = "black",
                      fill = "grey"),
                 default.units = "native",
                vp=viewport(clip='on',
                  yscale=c(0, maxh),
                  width=unit(1, 'npc'),
                  height=unit(10, 'mm'),
                  xscale=lim##lattice:::extend.limits(range(x))
                ))
}


x=rexp(1e5)
y=rnorm(1e5)

p <- xyplot(y~x)

xlim <- p$x.limits
ylim <- p$y.limits

update(p, legend=list(
              right=list(fun=histY, args=list(y, ylim)),
              top=list(fun=histX, args=list(x, xlim)))
       )
       

legendX <- function(p, FUN=max, ...){
  x <- p$panel.args.common$x
  y <- p$panel.args.common$y
  z <- p$panel.args.common$z
  lim <- p$x.limits
  ag <- aggregate(z~x, FUN=FUN)
  rngAg <- range(ag$z)
  minZ <- min(ag$z)
  lenX <- length(ag$x)
  polygonGrob(x = c(ag$x[1], ag$x, ag$x[lenX]), y = c(minZ, ag$z, minZ),
            gp = gpar(col = "black", fill = 'grey'),
            default.units = "native",
            vp=viewport(clip='on',
              yscale=lattice:::extend.limits(rngAg),
              just='bottom',
              width=unit(1, 'npc'),
              height=unit(8, 'native'),
              xscale=lim 
              ))
}


legendY <- function(p, FUN=max, ...){
  x <- p$panel.args.common$x
  y <- p$panel.args.common$y
  z <- p$panel.args.common$z
  lim <- p$y.limits
  ag <- aggregate(z~y, FUN=FUN)
  rngAg <- range(ag$z)
  minZ <- min(ag$z)
  lenY <- length(ag$y)
  polygonGrob(y = c(ag$y[1], ag$y, ag$y[lenY]), x = c(minZ, ag$z, minZ),
            gp = gpar(col = "black", fill = 'grey'),
             default.units = "native",
            vp=viewport(clip='off',
              xscale=lattice:::extend.limits(rngAg),
              just='left',
              height=unit(1, 'npc'),
              width=unit(8, 'native'),
              yscale=lim 
              ))
}

## legendX <- function(p, FUN=max, ...){
##   x <- p$panel.args.common$x
##   y <- p$panel.args.common$y
##   z <- p$panel.args.common$z
##   lim <- p$x.limits
##   ag <- aggregate(z~x, FUN=FUN)
##   rngAg <- range(ag$z)
  
##   vp=viewport(clip='off',
##     yscale=rngAg, xscale=lattice:::extend.limits(lim),
##     width=unit(1.02, 'npc'),
##     height=unit(15, 'mm')
##     )

##   fg <- frameGrob(vp=vp)

##   lg <- linesGrob(x=ag$x, y=ag$z, vp=vp, default.units='native')
##   axis <- yaxisGrob(vp=vp, main=FALSE)

##   fg <- packGrob(fg, lg)
##   fg <- packGrob(fg, axis)
  
## }


## vp<-viewport(clip='on',
##     yscale=rngAg, xscale=lim,
##     width=unit(0.5, 'npc'),
##     height=unit(150, 'mm'),
##               name="view")

## yAxis <- yaxisGrob(name = "axis2")

## lg <- linesGrob(x=ag$x, y=ag$z,
##                      default.units='native',
##                      name="dataLines")
## tree <- gTree(name="Tree", vp=vp,
##               children=gList(yAxis, lg))
## grid.draw(tree)
