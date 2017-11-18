.swapCitations <- function(doc, type = NULL){
  assert_that(!is.null(type), msg = 'type argument not provided')
  assert_that(type %in% c('Rnw', 'Rmd'), msg = 'type argument must be either Rmd or Rnw')
  if(type == 'Rmd'){
    cit.idx <- str_which(doc, ' @[a-zA-Z0-9]')
    doc[cit.idx] %<>% str_replace_all('@([a-zA-Z0-9]*)', '\\\\cite\\{\\1\\}')
  }else if(type == 'Rnw') {
    cit.idx <- str_which(doc, '\\\\cite')
    doc[cit.idx] %<>% str_replace_all('\\\\cite\\{(.*?)\\}', '@\\1')
  }
  return(doc)
}
