
# @knitr fun_newProject
newProject <- function(name, path,
                       dirs=c("code", "data", "docs", "plots", "workspaces"),
                       docs.dirs=c("diagrams", "ioslides", "notebook", "Rmd/include", "md", "html", "Rnw", "pdf", "timeline", "tufte"),
                       overwrite = FALSE){

  assert_that(is.character(name), msg = 'name is not a character')
  name <- file.path(path, name)
  if(file.exists(name) && !overwrite) stop("This project already exists.")
  dir.create(name, recursive = TRUE, showWarnings = FALSE)
  if(!file.exists(name)) stop("Directory appears invalid.")

  path.dirs <- file.path(name, dirs)
  sapply(path.dirs, dir.create, showWarnings = FALSE)
  path.docs <- file.path(name, "docs", docs.dirs)
  if("docs" %in% dirs) sapply(path.docs, dir.create, recursive=TRUE, showWarnings=FALSE)
  if(overwrite) cat("Project directories updated.\n") else cat("Project directories created.\n")
}
