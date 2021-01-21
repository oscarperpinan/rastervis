globalVariables('y')

##Create a Layer from a custom function of the coordinates
xyLayer <- function(object, dirXY=y){
  y <- init(object, fun='y')
  x <- init(object, fun='x')
  isLanguage <- try(is.language(dirXY), silent=TRUE)
  if (class(isLanguage)=='try-error' || !isLanguage) dirXY <- substitute(dirXY)
  dirLayer <- eval(dirXY)
}
