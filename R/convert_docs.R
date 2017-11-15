
# convertDocs -------------------------------------------------------------


# @knitr fun_convertDocs
# Rmd <-> Rnw document conversion
# Main conversion function
convertDocs <- function(path, rmdChunkID=c("```{r", "}", "```"), rnwChunkID=c("<<", ">>=", "@"), emphasis="replace", overwrite=FALSE, ...){
  assert_that(is.character(path), msg = 'path is not a character')
  type <- basename(path)
  dots <- list(...)
  if(is.null(recursive <- dots$recursive)) recursive <- FALSE
  rmd.files <- list.files(path, pattern=".Rmd$", full=TRUE, recursive=recursive)
  rnw.files <- list.files(path, pattern=".Rnw$", full=TRUE, recursive=recursive)

  if(rmdChunkID[1]=="```{r") rmdChunkID[1] <- paste0(rmdChunkID[1], " ")
  if(type=="Rmd"){
    assert_that(length(rmd.files) > 0, msg = glue("No .Rmd files in {path}"))
    outDir <- file.path(dirname(path), "Rnw")
    if(is.null(doc.class <- dots$doc.class)) doc.class <- "article"
    if(is.null(doc.packages <- dots$doc.packages)) doc.packages <- "geometry"
    doc.class.string    <- paste0("\\documentclass{", doc.class, "}")
    doc.packages.string <- paste0(sapply(doc.packages, function(x) paste0("\\usepackage{", x, "}")), collapse="\n")
    if("geometry" %in% doc.packages) doc.packages.string <- c(doc.packages.string, "\\geometry{verbose, tmargin=2.5cm, bmargin=2.5cm, lmargin=2.5cm, rmargin=2.5cm}")
    header.rnw <- c(doc.class.string, doc.packages.string, "\\begin{document}\n")#,
    #paste0("<<highlight, echo=FALSE>>=\nknit_theme$set(knit_theme$get('", theme, "'))\n@\n"))
  } else if(type=="Rnw") {
    assert_that(length(rnw.files) > 0, msg = 'No .Rnw files in {path}')
    outDir <- file.path(dirname(path), "Rmd")
  } else stop("path must end in 'Rmd' or 'Rnw'.")
  if(type=="Rmd"){
    # a_ply(rmd.files, 1, function(x){
    #   .swap(file = x, header=header.rnw, outDir=outDir, rmdChunkID=rmdChunkID, rnwChunkID=rnwChunkID, emphasis=emphasis, overwrite=overwrite, ...)
    # })
    walk(rmd.files, function(file){
      .swap(file, header=header.rnw, outDir=outDir, rmdChunkID=rmdChunkID, rnwChunkID=rnwChunkID, emphasis=emphasis, overwrite=overwrite, ...)
    })
    cat(".Rmd to .Rnw file conversion complete.\n")
  } else {
    walk(rnw.files, function(file){
      .swap(file, header=NULL, outDir=outDir, rmdChunkID = rmdChunkID, rnwChunkID = rnwChunkID, emphasis=emphasis, overwrite=overwrite, ...)
    })
    cat(".Rnw to .Rmd file conversion complete.\n")
  }
}
