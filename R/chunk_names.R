
# @knitr fun_chunkNames
chunkNames <- function(path, rChunkID="# @knitr", rmdChunkID="```{r", append.new=FALSE){
  files <- list.files(path, pattern = ".R$", full = TRUE)
  assert_that(length(files) > 0, msg = glue("No .R files in {path}"))
  l1 <- lapply(files, readLines)
  l1 <- rapply(l1, function(x) x[substr(x, 1, nchar(rChunkID)) == rChunkID], how = "replace")
  l1 <- rapply(l1, function(x, p) gsub(paste0(p, " "), "", x), how = "replace", p = rChunkID)
  if(!append.new) return(l1)


  rmd <- gsub("\\.R", "\\.Rmd", basename(files))
  rmd <- file.path(dirname(path), "docs/Rmd", rmd)
  rmd <- rmd[sapply(rmd, file.exists)]
  assert_that(length(rmd) > 0, 'No .Rmd files') # Rmd files must exist
  files.ind <- match(gsub("\\.Rmd", "", basename(rmd)), gsub("\\.R", "", basename(files))) # Rmd exist for which R script
  l2 <- lapply(rmd, readLines)
  l2 <- rapply(l2, function(x) x[substr(x, 1, nchar(rmdChunkID))==rmdChunkID], how="replace")
  l2 <- rapply(l2, function(x, p) gsub(paste0(p, " "), "", x), how="replace", p=gsub("\\{", "\\\\{", rmdChunkID))
  l2 <- rapply(l2, function(x) gsub("}", "", sapply(strsplit(x, ","), "[[", 1)), how="replace")
  sapply(1:length(rmd), appendRmd, rmd.files=rmd, rChunks=l1[files.ind], rmdChunks=l2, ID=rmdChunkID)
}

