to_pdf <- function(file, parentcite = T, remove_tmp_files=T) {

  file %<>%{gsub('\\.[A-Za-z]*$','',., perl=T)}
  tex_name <- paste0(file,'.tex')
  rnw_name <- paste0(file, '.Rnw')
  Sweave2knitr(rnw_name)
  #tex_tmp_name <- gsub("[.]([^.]+)$", "-knitr.\\1", file)
  tex_tmp_name <- gsub("[.]([^.]+)$", "-knitr.Rnw", rnw_name)
  knit(tex_tmp_name, output = tex_name)
  if (parentcite)
    readLines(tex_name) %>%
    {gsub(pattern = '\\cite', '\\parencite', .)} %>%
    writeLines(tex_name)

  tex <- paste0(file, '.tex')
  tex <<-tex
  texi2pdf(tex, clean=T)
  texi
  # if(remove_tmp_files){
  #   Sys.sleep(3)
  #   list.files() %>%
  #     .[{grepl(file,.)}]%>%
  #     .[{!grepl('(*Rmd$)|(.Rnw$)',.)}] %>%
  #     file.remove()
  # }
  #
}
