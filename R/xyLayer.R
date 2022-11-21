globalVariables('y')

##Create a Layer from a custom function of the coordinates
xyLayer <- function(object, dirXY=y, vector = TRUE, maxpixels)
{
    if (is(object, "Raster"))
    {
        if (!missing(maxpixels))
        {
            object <- sampleRegular(object,
                                    size = maxpixels,
                                    asRaster = TRUE)
        }
        y <- raster::init(object, fun='y')
        x <- raster::init(object, fun='x')
    }        
    else
    {
        if (!missing(maxpixels))
        {
            object <- spatSample(object,
		  		 method = "regular",
                                 size = maxpixels,
                                 as.raster = TRUE)
        }
        y <- terra::init(object, fun='y')
        x <- terra::init(object, fun='x')
    }
    isLanguage <- try(is.language(dirXY), silent=TRUE)
    if (inherits(isLanguage, "try-error") || !isLanguage)
        dirXY <- substitute(dirXY)
    dirLayer <- eval(dirXY)
    ## Return a numeric vector is vector = TRUE
    if (isTRUE(vector))
    {
        if (is(object, "Raster"))
            raster::values(dirLayer)
        else
            terra::values(dirLayer, mat = FALSE)
    }
    else 
        dirLayer
}
