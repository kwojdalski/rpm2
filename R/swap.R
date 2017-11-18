
# .swap -------------------------------------------------------------------

.swap <- function(file, header=NULL, rmdChunkID, rnwChunkID, emphasis, ...){
  # Additional arguments
  title      <- list(...)$title
  author     <- list(...)$author
  highlight  <- list(...)$highlight
  bibliography   <- list(...)$bibliography
  bib_style  <- list(...)$bib_style
  standalone <- list(...)$standalone
  is_child   <- list(...)$is_child
  ext <- tail(strsplit(file, "\\.")[[1]], 1)

  # Strip any html style lines
  l <- readLines(file) %>% {.[substr(., 1, 7)!="<style>"]}

  # Checking for child document. Child document is defined by existing header (for .Rmd files) ---------------------------------------------------------------------------------------

  if(is.null(is_child)){
    is_child  <- str_replace(file, pattern = paste0('.*', ext, '\\/'), '') %>%
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
    #assert_that(length(which(l=="---")) > 0, msg = glue('{file} seems to be a child document'))

    if(length(which(l=="---")) == 2){
        h.ind         <- 1:which(l=="---")[2]
        h             <- l[h.ind]
        t.ind         <- which(substr(h, 1, 7) == "title: ")
        a.ind         <- which(substr(h, 1, 8) == "author: ")
        highlight.ind <- which(substr(h, 1, 11)=="highlight: ")
        b.ind         <- str_which(h, 'bibliography: ')



    # check for different conditions

      if(is.null(title) && exists('t.ind') && length(t.ind))                     title     <- substr(h[t.ind], 8, nchar(h[t.ind])) else if(is.null(title)) title <- ""
      if(is.null(author) && exists('a.ind') && length(a.ind))                    author    <- substr(h[a.ind], 9, nchar(h[a.ind])) else if(is.null(author)) author <- ""
      if(is.null(highlight) && exists('highlight.ind') && length(highlight.ind)) highlight <- substr(h[highlight.ind], 12, nchar(h[highlight.ind])) else if(is.null(highlight)) highlight <- hl.default else if(!(highlight %in% knit_theme$get())) highlight <- hl.default
      if(is.null(bibliography) && exists('b.ind') && length(b.ind))  bibliography <- str_extract(h[b.ind], '(?<=: ).*')  else if(is.null(bibliography)) bibliography <- ""
      if(is.null(bib_style)) bib_style <- 'unsrt'
      if(!is.null(title))  header  <- c(header, paste0("\\title{", title, "}"))
      if(!is.null(author)) header <- c(header, paste0("\\author{", author, "}"))
      if(!is.null(title))  header  <- c(header, "\\maketitle\n")

      # End of file ---------------------------------------------------------------------------------
      if(!is.null(bibliography)){
        end_of_file <- paste0("\\bibliographystyle{", bib_style, "}")
        end_of_file <- c(end_of_file, paste0("\\bibliography{", bibliography, "}"))
        end_of_file <- c(end_of_file, '\\end{document}')
      }


      # if knit_theme not found add it. Else dont do anything
      if(!any(str_detect(l, "highlight, echo=FALSE"))){
        header <- c(header, paste0("<<highlight, echo=FALSE>>=\nknit_theme$set(knit_theme$get('", highlight, "'))\n@\n"))
      }
  }

  } else if(ext=="Rnw") {
    from <- rnwChunkID
    to   <- rmdChunkID
    hl.default <- "tango"
    out.ext <- "Rmd"
    # maketitle is a title page from start to maketitle line
    if(length(which(l=="\\begin{document}")) == 1){
      begin.doc  <- which(l=="\\begin{document}")
      make.title <- which(l=="\\maketitle")
      assert_that(length(begin.doc) > 0,
                  msg = glue("{{file}} is corrupted, could not find \\begin{document}", .open = "{{", .close = "}}"))
      assert_that(length(make.title) > 0,
                  msg = glue("{{file}} is corrupted, could not find \\maketitle of the document", .open = "{{", .close = "}}"))
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
      header   <- .rmdHeader(title = title, author = author, highlight = highlight)
      h.chunks <- .swapChunks(from = from, to = to, x = h, offset.end = 0)
      header   <- c(header, h.chunks[[1]][h.chunks[[2]]]) %>% paste0(collapse = "\n")
    }
  }
  # The main part l stands for main lines ---------------------------------------------------------------------------------------
  if(exists('h.ind') && length(h.ind) > 0) l <- paste0(l[-h.ind])

  l      <- .swapHeadings(from = from, to = to, x = l)
  chunks <- .swapChunks(from = from, to = to, x = l)
  l      <- chunks[[1]]




  if(ext == "Rmd") {
    l <- .swapEmphasis(x = l, emphasis = emphasis)
    l[-chunks[[2]]] <- sapply(l[-chunks[[2]]], function(v, p, r) gsub(p, r, v), p="_", r="\\\\_")
    l %<>% .swapItems(type = 'Rmd')
    if(!is_child) l <- c(l, "\n\\end{document}\n")
    l %<>% str_replace('<!-- (.*) -->', '%\\1')
    # change Rnw childs to Rmd and the opposite, depending on the case
    l %<>% str_replace("\\.Rmd(\'|\")", "\\.Rnw\\1")
    l %<>% .swapCitations(type = "Rmd")
  } else if(ext == "Rnw"){
    l %<>% .swapItems(type = 'Rnw') %>% .swapEquations()
    ind <- tryCatch({
      ret <- which(substr(l, 1, 1) == "\\")
      if(length(ret) || !ret) simpleError('Zero substr found!')
      ret
    }, error=function(error) return(1))
    l %<>% .swapCitations(type = "Rnw")

    # drop any remaining lines beginning with a backslash
    l <- l[-ind]
    # changing comments
    l %<>% str_replace('^%(.*)', '<!--\\1 -->')
    # change Rnw childs to Rmd and the opposite, depending on the case
    l %<>% str_replace("\\.Rnw(\'|\")", "\\.Rmd\\1")
  }

  if(exists('end_of_file')) l <- c(l, end_of_file)
  if(exists("h")) l <- c(header, l)

  l%<>% str_replace('(\n?)$', '\n')

  return(list(lines = l, ext = ext, out.ext = out.ext,  is_child = is_child))
}
