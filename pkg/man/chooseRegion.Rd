\name{chooseRegion}
\alias{chooseRegion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chooseRegion(sp = TRUE, proj = CRS("+proj=latlon +ellps=WGS84"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
%%     ~~Describe \code{sp} here~~
}
  \item{proj}{
%%     ~~Describe \code{proj} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sp = TRUE, proj = CRS("+proj=latlon +ellps=WGS84"), 
    ...) 
{
    trellis.focus("panel", 1, 1)
    x <- trellis.panelArgs()$x
    y <- trellis.panelArgs()$y
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    pointsData <- cbind(px, py)
    border <- as.numeric()
    while (TRUE) {
        ll <- grid.locator(unit = "native")
        if (!is.null(ll)) {
            lpoints(ll, col = "black", cex = 0.7, pch = 3)
            lx <- convertX(unit(ll$x, "native"), "points", FALSE)
            ly <- convertY(unit(ll$y, "native"), "points", FALSE)
            border <- rbind(border, c(lx, ly))
        }
        else {
            break
        }
    }
    trellis.unfocus()
    inside <- in.out(border, pointsData)
    pointsInside <- SpatialPoints(coords = data.frame(xin = x[inside], 
        yin = y[inside]), proj4string = proj)
    spPoints <- list("sp.points", pointsInside, cex = 0.5)
    print(update(trellis.last.object(), sp.layout = spPoints))
    if (sp) 
        return(pointsInside)
    else return(inside)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
