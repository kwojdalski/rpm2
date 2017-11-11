
# moveDocs ----------------------------------------------------------------
# @knitr fun_moveDocs
# Organization documentation
moveDocs <- function(path.docs, type=c("md", "html","pdf"), move=TRUE, copy=FALSE, remove.latex=TRUE, latexDir="latex"){
  if(any(!(type %in% c("md", "html","pdf")))) stop("type must be among 'md', 'html', and 'pdf'")
  stopifnot(move | copy)
  if(path.docs=="." | path.docs=="./") path.docs <- getwd()
  if(strsplit(path.docs, "/")[[1]][1]==".."){
    tmp <- strsplit(path.docs, "/")[[1]][-1]
    if(length(tmp)) path.docs <- file.path(getwd(), paste0(tmp, collapse="/")) else stop("Check path.docs argument.")
  }
  for(i in 1:length(type)){
    if(type[i]=="pdf") origin <- "Rnw" else origin <- "Rmd"
    path.i <- file.path(path.docs, origin)
    infiles <- list.files(path.i, pattern=paste0("\\.", type[i], "$"), full=TRUE)
    if(type[i]=="pdf"){
      extensions <- c("tex", "aux", "log")
      all.pdfs <- basename(list.files(path.docs, pattern=".pdf$", full=T, recursive=T))
      pat <- paste0("^", rep(gsub("pdf", "", all.pdfs), length(extensions)), rep(extensions, each=length(all.pdfs)), "$")
      latex.files <- unlist(sapply(1:length(pat), function(p, path, pat) list.files(path, pattern=pat[p], full=TRUE), path=path.i, pat=pat))
      print(latex.files)
      if(length(latex.files)){
        if(remove.latex){
          unlink(latex.files)
        } else {
          dir.create(file.path(path.docs, latexDir), showWarnings=FALSE, recursive=TRUE)
          file.rename(latex.files, file.path(path.docs, latexDir, basename(latex.files)))
        }
      }
    }
    if(length(infiles)){
      infiles <- infiles[basename(dirname(infiles))==origin]
      if(length(infiles)){
        if(type[i]=="html"){
          html.dirs <- gsub("\\.html", "_files", infiles)
          dirs <- list.dirs(path.i, recursive=FALSE)
          ind <- which(dirs %in% html.dirs)
          if(length(ind)){
            html.dirs <- dirs[ind]
            html.dirs.recur <- list.dirs(html.dirs)
            for(p in 1:length(html.dirs.recur))	dir.create(gsub("/Rmd", "/html", html.dirs.recur[p]), recursive=TRUE, showWarnings=FALSE)
            subfiles <- unique(unlist(lapply(1:length(html.dirs.recur), function(p, path) list.files(path[p], full=TRUE), path=html.dirs.recur)))
            subfiles <- subfiles[!(subfiles %in% html.dirs.recur)]
            file.copy(subfiles, gsub("/Rmd", "/html", subfiles), overwrite=TRUE)
            if(move) unlink(html.dirs, recursive=TRUE)
          }
        }
        outfiles <- file.path(path.docs, type[i], basename(infiles))
        if(move) file.rename(infiles, outfiles) else if(copy) file.copy(infiles, outfiles, overwrite=TRUE)
      }
    }
  }
}
