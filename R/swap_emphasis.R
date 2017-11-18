# @knitr fun_swapEmphasis
# Rmd <-> Rnw document conversion
# Conversion support functions
# called by .swap()
# I know I use '**' strictly for bold font in Rmd files.
# For now, this function assumes:
# 1. The only emphasis in a doc is boldface or typewriter.
# 2. These instances are always preceded by a space, a carriage return, or an open bracket,
# 3. and followed by a space, period, comma, or closing bracket.
.swapEmphasis <- function(x, emphasis="remove",
                          pat.remove=c("`", "\\*\\*", "__"),
                          pat.replace=pat.remove,
                          replacement=c("\\\\texttt\\{", "\\\\textbf\\{", "\\\\textbf\\{", "\\}", "\\}", "\\}")){

  assert_that(emphasis %in% c("remove", "replace"), msg = 'emphasis must equal to "remove" or "replace')
  n <- length(pat.replace)
  rep1 <- replacement[1:n]
  rep2 <- replacement[(n+1):(2*n)]
  prefix <- c(" ", "^", "\\{", "\\(")
  suffix <- c(" ", ",", "-", "\n", "\\.", "\\}", "\\)")
  n.p <- length(prefix)
  n.s <- length(suffix)
  pat.replace <- c(paste0(rep(prefix, n), rep(pat.replace, each=n.p)), paste0(rep(pat.replace, each=n.s), rep(suffix, n)))
  replacement <- c(paste0(rep(gsub("\\^", "", prefix), n), rep(rep1, each=n.p)), paste0(rep(rep2, each=n.s), rep(suffix, n)))
  if(emphasis=="remove") for(k in 1:length(pat.remove)) x <- sapply(x, function(v, p, r) gsub(p, r, v), p=pat.remove[k], r="")
  if(emphasis=="replace") for(k in 1:length(pat.replace)) x <- sapply(x, function(v, p, r) gsub(p, r, v), p=pat.replace[k], r=replacement[k])
  names(x) <- NULL
  x
}

# @knitr fun_swap
# Rmd <-> Rnw document conversion
# Conversion support functions
# called by .convertDocs()
