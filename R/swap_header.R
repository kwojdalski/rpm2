
swapHeader <- function(x, arglist){
  if(arglist$title=="filename") arglist$title <- gsub("\\.Rmd", "\\.R", basename(x))
  header <- do.call(.rmdHeader, arglist)
  l <- readLines(x)
  ind <- which(l=="---")
  l <- l[(ind[2] + 1):length(l)]
  l <- paste0(l, "\n")
  sink(x)
  sapply(c(header, l), cat)
  sink()
}
