# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

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
              yscale=rngAg,
              just='bottom',
              width=unit(1, 'npc'),
              height=unit(8, 'native'),
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
  minZ <- min(ag$z)
  lenY <- length(ag$y)
  polygonGrob(y = c(ag$y[1], ag$y, ag$y[lenY]), x = c(minZ, ag$z, minZ),
            gp = gpar(col = "black", fill = 'grey'),
             default.units = "native",
            vp=viewport(clip='off',
              xscale=rngAg,
              just='left',
              height=unit(1, 'npc'),
              width=unit(8, 'native'),
              yscale=lim ##lattice:::extend.limits(range(x))
              ))
}
