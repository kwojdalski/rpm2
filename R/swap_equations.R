# .swapEquations ----------------------------------------------------------

.swapEquations <- function(doc){
  doc %>%
  {gsub('\\\\begin\\{equation\\}','\\$\\$',.,perl=T)} %>%
  {gsub('\\\\end\\{equation\\}','\\$\\$',.,perl=T)}
}
