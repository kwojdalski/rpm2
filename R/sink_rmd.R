sinkRmd <- function(x, arglist,  ...){
  if(arglist$title=="filename") arglist$title <- gsub("\\.Rmd", "\\.R", basename(x))
  y1 <- do.call(.rmdHeader, arglist)
  y2 <- .rmdknitrSetup(file=x, ...)
  y3 <- list(...)$rmd.template
  if(is.null(y3)) y3 <- rmd.template
  sink(x)
  sapply(c(y1, y2, y3), cat)
  sink()
}
