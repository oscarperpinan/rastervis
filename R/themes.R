## General theme
rasterTheme <- function(region = magma(10),
                        pch = 19, cex = 0.7,
                        strip.background = list(col = 'transparent'),
                        strip.shingle = list(col = 'transparent'),
                        strip.border = list(col = 'transparent'),
                        add.lines = list(lwd = .4),
                        ...)
{
    ## Check which arguments in dots are used by simpleTheme and custom.theme
    dots <- list(...)
    argsDots <- names(dots)
    argsCT <- union(names(formals(simpleTheme)),
                    names(formals(custom.theme)))
    common <- argsDots %in% argsCT
    ## Call custom.theme.2 with the arguments it can understand
    theme <- do.call(custom.theme.2,
                     c(list(pch = pch, cex = cex, region = region),
                       dots[common]))
    ## Modify the result with additional arguments
    theme <- modifyList(theme,
                        c(dots[!common],
                          list(strip.background = strip.background,
                               strip.shingle = strip.shingle,
                               strip.border = strip.border,
                               add.lines = add.lines)))
    theme
}


## Additional themes using wrapper functions. Only the color palette changes.
viridisTheme <- function(region = viridis(10), ...) {
  rasterTheme(region = region, ...)
}

magmaTheme <- function(region = magma(10), ...) {
  rasterTheme(region = region, ...)
}

infernoTheme <- function(region = inferno(10), ...) {
  rasterTheme(region = region, ...)
}

plasmaTheme <- function(region = plasma(10), ...) {
  rasterTheme(region = region, ...)
}

YlOrRdTheme <- function(region=brewer.pal(9, 'YlOrRd'), ...) {
  theme <- rasterTheme(region=region, ...)
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


## This is a special theme for the streamplot function. The region
## colors are all black. It uses a palette for symbols
streamTheme <- function(region='black',
                        symbol=brewer.pal(n=5, name='Blues'),
                        alpha=0.6, 
                        panel.background=list(col='gray20'),
                        ...){
    theme <- rasterTheme(region=region, symbol=symbol,
                         panel.background=panel.background,
                         ...)
    theme
}
