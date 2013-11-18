logTicks <- function(lim, loc=1:9){
  expSeq <- seq(floor(lim[1]), ceiling(lim[2]))
  all <- outer(loc, 10^expSeq)
  ok <- (10^lim[1]<= all & all <= 10^lim[2])
  ans <- log10(all[ok])
  ans
  }

## Mostly copied from latticeExtra::xscale.components.log10ticks
zscale.components.log10ticks <- function(lim, logbase=10){
  at <- logTicks(lim, loc = 1:9)
  at.major <- logTicks(lim, loc = 1)
  major <- at %in% at.major
  tck <- ifelse(major, 1, 0.5)
  txt <- paste(10, '^', at, sep='')       ##as.character(tick.at)
  txt <- ifelse(major, parse(text=txt), FALSE) ##txt[!major] <- FALSE
  labels <- list(at=at, labels=txt)
  labels
}

## Mostly copied from lattice:::formattedTicksAndLabels.default
zscale.components.logpower <- function(lim, logbase){
  if (logbase==exp(1)) logbase='e'
  logpaste <- paste(as.character(logbase), "^", sep = "")
  at <- seq(floor(lim[1]), ceiling(lim[2]))##pretty(lim)
  ok <- at >= lim[1] & at <= lim[2]
  at <- at[ok]
  txt <- paste(logpaste, at, sep='') ##format(at, trim=TRUE), sep='')
##  at <- log(at, base=logbase)
  labels <- list(at=at, labels=parse(text=txt))
  labels
}
