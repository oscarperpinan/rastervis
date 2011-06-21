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
  linesGrob(x = ag$x, y=ag$z,
            gp =
            gpar(col = "black",
                 fill = "grey"),
            default.units = "native",
            vp=viewport(clip='on',
              yscale=rngAg,
              width=unit(1, 'npc'),
              height=unit(10, 'mm'),
              xscale=lim ##lattice:::extend.limits(range(x))
              ))
}
  

legendY <- function(p, FUN=max, ...){
  x <- p$panel.args.common$x
  y <- p$panel.args.common$y
  z <- p$panel.args.common$z
  lim <- p$y.limits
  ag <- aggregate(z~y, FUN=FUN)
  rngAg <- range(ag$z)
  linesGrob(x = ag$z, y=ag$y,
            gp =
            gpar(col = "black",
                 fill = "grey"),
            default.units = "native",
            vp=viewport(clip='on',
              xscale=rngAg,
              height=unit(1, 'npc'),
              width=unit(0.1, 'npc'),
              yscale=lim ##lattice:::extend.limits(range(x))
              ))
  
}


obj <- raster(SISmm, 1)
p <- spplot(obj, colorkey=FALSE)

update(p, legend=list(
            right=list(
              fun=legendY, args=list(p, sd)),
            top=list(
              fun=legendX, args=list(p, mean))
            )
       )


legendY <- function(p, FUN=max, ...){
  x <- p$panel.args.common$x
  y <- p$panel.args.common$y
  z <- p$panel.args.common$z
  lim <- p$y.limits
  ag <- aggregate(z~y, FUN=FUN)
  rngAg <- range(ag$z)
    lg <- linesGrob(x = ag$z, y=ag$y,
            gp =
            gpar(col = "black",
                 fill = "grey"),
            default.units = "native")
rg <- rectGrob(gp=gpar(col='grey', fill='transparent'))
  fg <- frameGrob(layout=grid.layout(1, 1),
                  vp=viewport(clip='on',
              xscale=rngAg,
              height=unit(1, 'npc'),
              width=unit(0.1, 'npc'),
              yscale=lim ##lattice:::extend.limits(range(x))
              ))
  fg <- placeGrob(fg, lg, 1, 1)
  fg <- placeGrob(fg, rg, 1, 1)
  fg
}
