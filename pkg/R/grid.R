# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

legendX <- function(p, FUN, scale.x,...){
  px <- p$panel.args.common$x
  py <- p$panel.args.common$y
  pz <- p$panel.args.common$z
  lim <- p$x.limits
  ag <- aggregate(pz~px, FUN=FUN)
  if (is.null(scale.x)) scale.x <- range(ag$pz)
  minZ <- scale.x[1]
  lenX <- length(ag$px)
  polygonGrob(x = c(ag$px[1], ag$px, ag$px[lenX]), y = c(minZ, ag$pz, minZ),
            gp = gpar(col = "black", fill = 'grey'),
            default.units = "native",
            vp=viewport(clip='on',
              yscale=scale.x,
              just='bottom',
              width=unit(1, 'npc'),
              height=unit(8, 'native'),
              xscale=lim ##lattice:::extend.limits(range(x))
              ))
}


legendY <- function(p, FUN, scale.y,...){
  px <- p$panel.args.common$x
  py <- p$panel.args.common$y
  pz <- p$panel.args.common$z
  lim <- p$y.limits
  ag <- aggregate(pz~py, FUN=FUN)
  if (is.null(scale.y)) scale.y <- range(ag$pz)
  minZ <- scale.y[1]
  lenY <- length(ag$py)
  polygonGrob(y = c(ag$py[1], ag$py, ag$py[lenY]), x = c(minZ, ag$pz, minZ),
            gp = gpar(col = "black", fill = 'grey'),
             default.units = "native",
            vp=viewport(clip='off',
              xscale=scale.y,
              just='left',
              height=unit(1, 'npc'),
              width=unit(8, 'native'),
              yscale=lim ##lattice:::extend.limits(range(x))
              ))
}
