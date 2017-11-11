
# @knitr fun_rmdHeader
# Generate Rmd files
# Rmd yaml front-matter
# called by genRmd
.rmdHeader <- function(title="filenames", author="Matthew Leonawicz", theme="united", highlight="zenburn", toc=FALSE, keep.md=TRUE, ioslides=FALSE, include.pdf=FALSE){
  if(toc) toc <- "true" else toc <- "false"
  if(keep.md) keep.md <- "true" else keep.md <- "false"
  if(ioslides) hdoc <- "ioslides_presentation" else hdoc <- "html_document"
  rmd.header <- "---\n"
  if(!is.null(title)) rmd.header <- paste0(rmd.header, 'title: ', title, '\n')
  if(!is.null(author)) rmd.header <- paste0(rmd.header, 'author: ', author, '\n')
  rmd.header <- paste0(rmd.header, 'output:\n  ', hdoc, ':\n    toc: ', toc, '\n    theme: ', theme, '\n    highlight: ', highlight, '\n    keep_md: ', keep.md, '\n')
  if(ioslides) rmd.header <- paste0(rmd.header, '    widescreen: true\n')
  if(include.pdf) rmd.header <- paste0(rmd.header, '  pdf_document:\n    toc: ', toc, '\n    highlight: ', highlight, '\n')
  rmd.header <- paste0(rmd.header, '---\n')
  rmd.header
}
