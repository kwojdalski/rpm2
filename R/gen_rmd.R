# @knitr fun_genRmd
genRmd <- function(
  path, replace = FALSE,
  header.args = list(title = "filename", author = NULL, theme = "united",
                     highlight = "zenburn", toc = FALSE, keep.md = TRUE, ioslides = FALSE, include.pdf = FALSE),
  update.header=FALSE, ...
){

  assert_that(is.character(path), msg = 'path is not a character')
  files <- list.files(path, pattern = ".R$", full = TRUE)
  assert_that(length(files) > 0, msg = glue("No files in {path}"))
  rmd <- gsub("\\.R", "\\.Rmd", basename(files))
  rmd <- file.path(dirname(path), "docs/Rmd", rmd)
  if(!(replace | update.header)) rmd <- rmd[!sapply(rmd, file.exists)]
  if(update.header) rmd <- rmd[sapply(rmd, file.exists)]
  assert_that(length(rmd) > 0, msg = glue('No .Rmd files in {rmd}'))



  if(update.header){
    sapply(rmd, swapHeader, arglist=header.args)
    cat("yaml header updated for each .Rmd file.\n")
  } else {
    sapply(rmd, sinkRmd, arglist=header.args, ...)
    cat(".Rmd files created for each .R file.\n")
  }
}
