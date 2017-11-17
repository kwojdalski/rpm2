
# convertDocs -------------------------------------------------------------


# @knitr fun_convertDocs
# Rmd <-> Rnw document conversion
# Main conversion function
convertDocs <- function(type = 'Rmd', path_folder = NULL,
                        new_path_folder = NULL,
                        rmdChunkID=c("```{r", "}", "```"),
                        rnwChunkID=c("<<", ">>=", "@"), emphasis="replace", overwrite=FALSE, copy_other_files = TRUE, ...){
  #assert_that(is.character(type), msg = 'path is not a character')
  if(is.null(type)) type <- basename(path_folder)
  assert_that(type %in% c('Rmd', 'Rnw'), msg = 'Type must be Rmd or "Rnw"')

  dots <- list(...)
  if(is.null(recursive <- dots$recursive)) recursive <- FALSE
  rmd.files <- list.files(path_folder, pattern=".Rmd$", full=TRUE, recursive=recursive)
  rmd.files %>% str_extract('.*(?=\\.Rmd)')
  rnw.files <- list.files(path_folder, pattern=".Rnw$", full=TRUE, recursive=recursive)
  if(copy_other_files ) {

    files_to_copy <- list.files(path_folder, full=TRUE, recursive=recursive)
    #new_type = if(type == 'Rmd') 'Rnw' else 'Rmd'
    files_to_copy <- files_to_copy[!str_detect(files_to_copy, paste0('\\.', type))]
    new_dir <- str_replace(files_to_copy, path_folder, new_path_folder)
    walk(dirname(new_dir), dir.create, recursive = TRUE)
    file.copy(from = files_to_copy, to = new_dir)
  }


  if(rmdChunkID[1]=="```{r") rmdChunkID[1] <- paste0(rmdChunkID[1], " ")
  if(type=="Rmd"){
    assert_that(length(rmd.files) > 0, msg = glue("No .Rmd files in {path_folder}"))
    if(is.null(new_path_folder))                   outDir       <- file.path(dirname(path_folder), "Rnw") else outDir <- file.path(new_path_folder)
    if(is.null(doc.class <- dots$doc.class))       doc.class    <- "article"
    if(is.null(doc.packages <- dots$doc.packages)) doc.packages <- "geometry"
    doc.class.string    <- paste0("\\documentclass{", doc.class, "}")
    doc.packages.string <- paste0(sapply(doc.packages, function(x) paste0("\\usepackage{", x, "}")), collapse="\n")
    if("geometry" %in% doc.packages) doc.packages.string <- c(doc.packages.string, "\\geometry{verbose, tmargin=2.5cm, bmargin=2.5cm, lmargin=2.5cm, rmargin=2.5cm}")
    header.rnw <- c(doc.class.string, doc.packages.string, "\\begin{document}\n")#,
    #paste0("<<highlight, echo=FALSE>>=\nknit_theme$set(knit_theme$get('", theme, "'))\n@\n"))
  } else if(type=="Rnw") {
    assert_that(length(rnw.files) > 0, msg = 'No .Rnw files in {path_folder}')
    if(is.null(new_path_folder)) outDir <- file.path(dirname(path_folder), "Rmd") else outDir <- file.path(dirname(path_folder), new_type_folder)
  } else stop("path_folder must end in 'Rmd' or 'Rnw'.")
  if(type=="Rmd"){
    walk(rmd.files, function(file){
      to_save <- .swap(file, header = header.rnw, rmdChunkID = rmdChunkID,  rnwChunkID = rnwChunkID, emphasis = emphasis,  ...)
      .save_converted_files(to_save$lines, ext = to_save$ext, out.ext = to_save$out.ext, overwrite = TRUE, file = file,
                            path_folder = path_folder, new_path_folder = new_path_folder)
    })
    cat(".Rmd to .Rnw file conversion complete.\n")
  } else {
    walk(rnw.files, function(file){
      to_save <- .swap(file, header = NULL, outDir = outDir, rmdChunkID = rmdChunkID, rnwChunkID = rnwChunkID, emphasis = emphasis, overwrite = overwrite, ...)
      .save_converted_files(to_save$lines, ext = to_save$ext, out.ext = to_save$out.ext, overwrite = TRUE, file = file,
                            path_folder = path_folder, new_path_folder = new_path_folder)
    })
    cat(".Rnw to .Rmd file conversion complete.\n")
  }
}
