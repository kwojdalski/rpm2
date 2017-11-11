
# @knitr fun_swapChunks
# Rmd <-> Rnw document conversion
# Conversion support functions
# called by .swap()
# .swapChunks changes code chunks in rnw to code chunks in RMD, e.g. in rnw it should start with <<< and end with @ to be correct
.swapChunks <- function(from, to, x, offset.end=1){
  gsbraces <- function(txt) gsub("\\{", "\\\\{", txt)
  nc <- nchar(x)
  # chunk.start.open <- substr(x, 1, nchar(from[1]))==from[1]

  from %<>% {gsub('(\\{|\\})','\\\\\\1',x=.,perl=T)}
  chunk.start.open <- grepl(paste0('^',gsub('\\s','',from[1])),x =x,perl=T )


  # chunk.start.close <- substr(x, nc-offset.end-nchar(from[2])+1, nc - offset.end)==from[2]
  chunk.start.close <- grepl(paste0(from[2],'(\n)?$'),x = x,perl=T)
  chunk.start <- which(chunk.start.open & chunk.start.close)
  chunk.end <- which(substr(x, 1, nchar(from[3]))==from[3])# & nc==nchar(from[3]) + offset.end)
  x[chunk.start] <- gsub(from[2], to[2], gsub(from[1], to[1], x[chunk.start]))
  x[chunk.end] <- gsub(paste0(from[3],'\\n$'), to[3], x[chunk.end])
  if(!length(x[chunk.end])) warning('Chunk end is incorrect')
  if(!length(x[chunk.start])) warning('Chunk start is incorrect')
  chunklines <- as.numeric(unlist(mapply(seq, chunk.start, chunk.end)))
  list(x, chunklines)
}


