globalVariables('y')

##Create a Layer from a custom function of the coordinates
xyLayer <- function(object, dirXY=y, vector = TRUE, maxpixels){
    if (!missing(maxpixels))
    {
        if (is(object, "Raster"))
            object <- sampleRegular(object,
                                    size = maxpixels,
                                    asRaster = TRUE)
        
        else
            object <- spatSample(object,
		  		 method = "regular",
                                 size = maxpixels,
                                 as.raster = TRUE)
    }
    y <- init(object, fun='y')
    x <- init(object, fun='x')
    isLanguage <- try(is.language(dirXY), silent=TRUE)
    if (class(isLanguage)=='try-error' || !isLanguage)
        dirXY <- substitute(dirXY)
    dirLayer <- eval(dirXY)
    ## Return a numeric vector is vector = TRUE
    if (isTRUE(vector))
    {
        if (is(object, "Raster"))
            getValues(dirLayer)
        else
            values(dirLayer, mat = FALSE)
    }
    else 
        dirLayer
}
