
# .swapItems --------------------------------------------------------------

.swapItems <- function(doc){
  doc%>%
  {gsub('\\\\begin\\{itemize\\}','',.,perl=T)} %>%
  {gsub('\\\\item','\\*',.,perl=T)} %>%
  {gsub('\\\\end\\{itemize\\}','',.,perl=T)}
}

