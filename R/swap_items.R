
# .swapItems --------------------------------------------------------------

.swapItems <- function(doc, type = NULL){

  if(type == 'Rnw'){
    ret <- doc%>%
      str_replace('\\\\begin\\{itemize\\}','') %>%
      str_replace('\\\\item','\\*') %>%
      str_replace('\\\\end\\{itemize\\}','')
    return(ret)
  } else if(type == 'Rmd') {

    assert_that(is.list(doc) || is.character(doc), msg = 'doc should be a list')

    # start idx
    items.idx <- doc %>% str_which('\\*')
    start_idx <- ifelse(doc[items.idx - 1] == '\n', items.idx - 1, NA) %>% {.[!is.na(.)]} %>% {add(.,seq_along(.)-1)}
    for(i in start_idx) doc <- append(doc, values = '\\begin{itemize}', after = i)
    # end idx
    items.idx <- doc %>% str_which('\\*')
    end_idx   <- ifelse(doc[items.idx + 1] == '\n', items.idx + 1, NA) %>% {.[!is.na(.)]} %>% {add(.,seq_along(.)-1)}
    for(i in end_idx) doc <- append(doc, values = '\\end{itemize}', after = i)
    ret <- doc %>% str_replace('\\*', '\\\\item') #%>%
      # str_replace('\\\\begin\\{itemize\\}','') %>%
      # str_replace('\\\\end\\{itemize\\}','')
    return(ret)
  }

}

