setClassUnion("RoT",
              c("Raster",
                "SpatRaster"))

setClassUnion("RLoT",
              c("RasterLayer",
                "SpatRaster"))

setClassUnion("RSoT",
              c("RasterStack",
                "SpatRaster"))

setClassUnion("RBoT",
              c("RasterBrick",
                "SpatRaster"))

setClassUnion("RSBoT",
              c("RasterStackBrick",
                "SpatRaster"))
