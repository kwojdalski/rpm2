# @knitr fun_rmdknitrSetup
# Rmd knitr setup chunk
# called by genRmd
.rmdknitrSetup <- function(file, include.sankey = FALSE){
  x <- paste0('\n```{r knitr_setup, echo=FALSE}\nopts_chunk$set(cache=FALSE, eval=FALSE, tidy=TRUE, message=FALSE, warning=FALSE)\n')
  if(include.sankey) x <- paste0(x, 'read_chunk("../../code/proj_sankey.R")\n')
  x <- paste0(x, 'read_chunk("../../code/', gsub("\\.Rmd", "\\.R", basename(file)), '")\n```\n')
  x
}
