
library(raster)
library(rasterVis)

old <- getwd()
##change to your folder...
setwd('CMSAF')
listFich <- dir(pattern='2008')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- month.abb

levelplot(SISmm)

levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)

library(maptools)
proj <- CRS('+proj=latlon +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)

p <- levelplot(SISmm, layers=1, FUN.margin=median)
p + layer(sp.lines(mapaSHP, lwd=0.8, col='darkgray'))

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)

Aug <- raster(SISmm, 8)

meanAug <- cellStats(Aug, mean)

levelplot(Aug-meanAug, par.settings=RdBuTheme)

library(colorspace)
myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(Aug, par.settings=myTheme, contour=TRUE)

library(dichromat)
myTheme <- rasterTheme(region=dichromat(terrain.colors(15)))
levelplot(Aug, par.settings=myTheme)

##Relation between the January & February versus July radiation for four
##differents longitude regions.
xyplot(Jan+Feb~Jul|cut(x, 4), data=SISmm, auto.key=list(space='right'))
##Faster with hexbinplot
hexbinplot(Jan~Jul|cut(x, 6), data=SISmm)

splom(SISmm)

histogram(SISmm)
densityplot(SISmm)
bwplot(SISmm)

histogram(SISmm, FUN=as.yearqtr)

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
dirXY <-xyLayer(r, sqrt(x^2 + y^2))
dirXY

library(zoo)

url <- "ftp://ftp.wiley.com/public/sci_tech_med/spatio_temporal_data/"
sst.dat = read.table(paste(url, "SST011970_032003.dat", sep=''), header = FALSE) 
sst.ll = read.table(paste(url, "SSTlonlat.dat", sep=''), header = FALSE)

spSST <- SpatialPointsDataFrame(sst.ll, sst.dat)
gridded(spSST) <- TRUE
proj4string(spSST) = "+proj=longlat +datum=WGS84"
SST <- brick(spSST)

idx <- seq(as.Date('1970-01-01'), as.Date('2003-03-01'), by='month')
idx <- as.yearmon(idx)
SST <- setZ(SST, idx)
layerNames(SST) <- as.character(idx)
hovmoller(SST, contour=FALSE, panel=panel.levelplot.raster,
          interpolate=TRUE, par.settings=RdBuTheme)

horizonplot(SST)

df <- expand.grid(x=seq(-2, 2, .1), y=seq(-2, 2, .1))
df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))

r <- rasterFromXYZ(df)
projection(r) <- CRS("+proj=longlat +datum=WGS84")

vectorplot(r, par.settings=RdBuTheme)

levelplot(SISmm)

##Do not close the last graphical window.  Use the left button of the
##mouse to identify points and the right button to finish

chosen <- identifyRaster(SISmm, layer=3, values=TRUE)
chosen

##Use the left button of the mouse to build a border with points, and
##the right button to finish.  The points enclosed by the border will
##be highlighted and returned as a SpatialPoints object.
reg <- chooseRegion()
summary(reg)
