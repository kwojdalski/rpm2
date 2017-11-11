appendRmd <- function(x, rmd.files, rChunks, rmdChunks, ID){
  r1 <- rmdChunks[[x]]
  r2 <- rChunks[[x]]
  r.new <- r2[!(r2 %in% r1)]
  if(length(r.new)){
    r.new <- paste0(ID, " ", r.new, "}\n```\n", collapse="") # Hard coded brace and backticks
    sink(rmd.files[x], append=TRUE)
    cat("\nNEW_CODE_CHUNKS\n")
    cat(r.new)
    sink()
    paste(basename(rmd.files[x]), "appended with new chunk names from .R file")
  }
  else glue("No new chunk names appended to {basename(rmd.files[x])}")
}
