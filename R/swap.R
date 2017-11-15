
# .swap -------------------------------------------------------------------

.swap <- function(file, header=NULL, outDir, rmdChunkID, rnwChunkID, emphasis, overwrite, ...){
  # Additional arguments
  title      <- list(...)$title
  author     <- list(...)$author
  highlight  <- list(...)$highlight
  standalone <- list(...)$standalone
  is_child   <- list(...)$is_child
  ext <- tail(strsplit(file, "\\.")[[1]], 1)
  # Strip any html style lines
  l <- readLines(file) %>% {.[substr(., 1, 7)!="<style>"]}


  # My additional stuff
  if(is.null(is_child)){
    is_child  <- gsub(pattern = paste0('.*', ext, '\\/'), '', x = file, perl = T) %>%
    {sapply(regmatches(., gregexpr("\\/", ., perl=T)), length)} %>% as.logical()
    if(is_child && is.null(standalone)) standalone <- F
  }


  # if(!is.null(standalone) && !is.null(is_child) && !is_child) standalone <- FALSE
  if(is.null(standalone)) standalone <- T
  if(is.null(is_child))   is_child <- F

  if(ext=="Rmd"){
    from <- rmdChunkID; to <- rnwChunkID
    hl.default <- "solarized-light"
    out.ext <- "Rnw"
    if(!is_child){
      assert_that(length(which(l=="---")) > 0, msg = glue('{file} seems to be a child document'))
      h.ind <- 1:which(l=="---")[2]
      h <- l[h.ind]
      t.ind <- which(substr(h, 1, 7) == "title: ")
      a.ind <- which(substr(h, 1, 8) == "author: ")
      highlight.ind <- which(substr(h, 1, 11)=="highlight: ")
    }



    # check for different conditions

    if(is.null(title) && exists('t.ind') && length(t.ind))                     title     <- substr(h[t.ind], 8, nchar(h[t.ind])) else if(is.null(title)) title <- ""
    if(is.null(author) && exists('a.ind') && length(a.ind))                    author    <- substr(h[a.ind], 9, nchar(h[a.ind])) else if(is.null(author)) author <- ""
    if(is.null(highlight) && exists('highlight.ind') && length(highlight.ind)) highlight <- substr(h[highlight.ind], 12, nchar(h[highlight.ind])) else if(is.null(highlight)) highlight <- hl.default else if(!(highlight %in% knit_theme$get())) highlight <- hl.default
    if(!is.null(title))  header  <- c(header, paste0("\\title{", title, "}"))
    if(!is.null(author)) header <- c(header, paste0("\\author{", author, "}"))
    if(!is.null(title))  header  <- c(header, "\\maketitle\n")



    header <- c(header, paste0("<<highlight, echo=FALSE>>=\nknit_theme$set(knit_theme$get('", highlight, "'))\n@\n"))
  } else if(ext=="Rnw") {
    from <- rnwChunkID
    to   <- rmdChunkID
    hl.default <- "tango"
    out.ext <- "Rmd"
    # maketitle is a title page from start to maketitle line
    if(standalone){
      begin.doc  <- which(l=="\\begin{document}")
      make.title <- which(l=="\\maketitle")
      assert_that(length(begin.doc) > 0, msg = glue("{{file}} is corrupted, could not find \\begin{document}", .open = "{{", .close = "}}"))
      assert_that(length(make.title) > 0, msg = glue("{{file}} is corrupted, could not find \\maketitle of the document", .open = "{{", .close = "}}"))
    } else {
      begin.doc <- 1
      make.title <- 1
    }


    if(length(make.title)) h.ind <- 1:make.title else h.ind <- 1:begin.doc
    h <- l[h.ind]
    t.ind <- which(substr(h, 1, 6)=="\\title")
    a.ind <- which(substr(h, 1, 7)=="\\author")
    highlight.ind <- which(substr(l, 1, 11)=="<<highlight")
    if(is.null(title) & length(t.ind)) title <- substr(h[t.ind], 8, nchar(h[t.ind])-1)
    if(is.null(author) & length(a.ind)) author <- substr(h[a.ind], 9, nchar(h[a.ind])-1)
    if(length(highlight.ind)){
      l1 <- l[highlight.ind+1]
      h1 <- substr(l1, nchar("knit_theme$set(knit_theme$get('") + 1, nchar(l1) - nchar("'))\n"))
      if(!(h1 %in% knit_theme$get())) h1 <- hl.default
    }
    # This part is all about headers. H.chunks are header chunks so it must be
    # evaluated only if this is going to be a standalone document

    if(is.null(highlight) & length(highlight.ind)) highlight <- h1 else if(is.null(highlight)) highlight <- hl.default else if(!(highlight %in% knit_theme$get())) highlight <- hl.default
    if(!is.null(standalone) && standalone){
      header <- .rmdHeader(title = title, author = author, highlight = highlight)
      h.chunks <- .swapChunks(from = from, to = to, x = h, offset.end = 0)
      header <- c(header, h.chunks[[1]][h.chunks[[2]]]) %>% paste0(collapse = "\n")
    }
  }
  # l is a main part of the document

  if(standalone && !is_child) l <- paste0(l[-h.ind], "\n") else l %<>% {paste0(., "\n")}

  l <- .swapHeadings(from = from, to = to, x = l)
  chunks <- .swapChunks(from = from, to = to, x = l)
  l <- chunks[[1]]

  if(ext == "Rmd"){
    l <- .swapEmphasis(x = l, emphasis = emphasis)
    l[-chunks[[2]]] <- sapply(l[-chunks[[2]]], function(v, p, r) gsub(p, r, v), p="_", r="\\\\_")
  }

  if(standalone){
    if(!is.null(header)) header %<>% str_replace('(\n?)$', '\n')
    l <- c(header, l)}
  if(ext == "Rmd") {
    l %<>% .swapItems(type = 'Rmd')
    l <- c(l, "\n\\end{document}\n")
  }
  if(ext == "Rnw"){
    l %<>% .swapItems(type = 'Rnw') %>% .swapEquations()
    ind <- tryCatch({
      ret <- which(substr(l, 1, 1) == "\\")
      if(length(ret) || !ret) simpleError('Zero substr found!')
      ret}, error=function(error) return(1))

    # drop any remaining lines beginning with a backslash
    l <- l[-ind]
  }


  if(!is.null(is_child) && is_child) outDir <- {if(ext =='Rmd') gsub(ext,'Rnw',x = file) else gsub(ext,'Rmd',x = file)} %>% {gsub(paste0('\\/', basename(.)),'',.)}

  if(!file.exists(outDir)) dir.create(outDir, showWarnings = FALSE)
  outfile <- file.path(outDir, gsub(paste0("\\.", ext), paste0("\\.", out.ext), basename(file)))

  # changing comments
  if(ext == 'Rmd'){
    # maybe'^'
    l %<>% {gsub('<!-- (.*) -->', '%\\1', ., perl=T)}
  } else {
    l %<>% {gsub('^%(.*)', '<!--\\1 -->', ., perl=T)}
  }
  # Change Rnw childs to Rmd and the opposite, depending on the case

  if(ext == 'Rmd'){
    l %<>% {gsub("\\.Rmd(\'|\")","\\.Rnw\\1",.,perl=T)}
  } else {
    l %<>% {gsub("\\.Rnw(\'|\")","\\.Rmd\\1",.,perl=T)}
  }
  if(!file.exists(dirname(outfile))){
    dir.create(dirname(outfile), recursive = TRUE)
    cat(glue("Directory {dirname(outfile)} created\n"))
  }
  if(overwrite || !file.exists(outfile)){
    sink(outfile)
    sapply(l, cat)
    # writeLines(l,outfile)
    sink(NULL)
    print(glue("Writing {outfile}"))
  }

}
