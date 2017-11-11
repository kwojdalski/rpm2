
# @knitr fun_swapHeadings
# Rmd <-> Rnw document conversion
# Conversion support functions
# called by .swap()
.swapHeadings <- function(from, to, x){
  nc <- nchar(x)
  ind <- which(substr(x, 1, 1)=="\\")
  if(!length(ind)){ # assume Rmd file
    ind <- which(substr(x, 1, 1)=="#")
    ind.n <- rep(1, length(ind))
    for(i in 2:6){
      ind.tmp <- which(substr(x[ind], 1, i)==substr("######", 1, i))
      if(length(ind.tmp)) ind.n[ind.tmp] <- ind.n[ind.tmp] + 1 else break
    }
    for(i in 1:length(ind)){
      n <- ind.n[i]
      input <- paste0(substr("######", 1, n), " ")
      h <- x[ind[i]]
      h <- gsub("\\*", "_", h) # Switch any markdown boldface asterisks in headings to double underscores
      heading <- gsub("\n", "", substr(h, n+2, nc[ind[i]]))
      #h <- gsub(input, "", h)
      if(n <= 2) subs <- "\\" else if(n==3) subs <- "\\sub" else if(n==4) subs <- "\\subsub" else if(n >=5) subs <- "\\subsubsub"
      output <- paste0("\\", subs, "section{", heading, "}\n")
      x[ind[i]] <- gsub(h, output, h)
    }
  } else { # assume Rnw file
    ind <- which(substr(x, 1, 8)=="\\section")
    if(length(ind)){
      for(i in 1:length(ind)){
        h <- x[ind[i]]
        heading <- paste0("## ", substr(h, 10, nchar(h)-2), "\n")
        x[ind[i]] <- heading
      }
    }
    ind <- which(substr(x, 1, 4)=="\\sub")
    if(length(ind)){
      for(i in 1:length(ind)){
        h <- x[ind[i]]
        z <- substr(h, 2, 10)
        if(z=="subsubsub") {p <- "##### "; n <- 19 } else if(substr(z, 1, 6)=="subsub") { p <- "#### "; n <- 16 } else if(substr(z, 1, 3)=="sub") { p <- "### "; n <- 13 }
        heading <- paste0(p, substr(h, n, nchar(h)-2), "\n")
        x[ind[i]] <- heading
      }
    }
  }
  x
}
