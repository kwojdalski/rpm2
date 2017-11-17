
.save_converted_files <- function(l                = NULL,
                                  is_child         = NULL,
                                  ext              = NULL,
                                  file             = NULL ,
                                  out.ext          = NULL,
                                  overwrite        = NULL,
                                  path_folder      = NULL,
                                  new_path_folder  = NULL) {

  out_dir <- str_replace(dirname(file), path_folder, new_path_folder)
  outfile <- file.path(out_dir,
                       str_replace(basename(file), paste0("\\.", ext), paste0("\\.", out.ext)))

  if(!file.exists(dirname(outfile))){
    dir.create(dirname(outfile), recursive = TRUE)
    cat(glue("Directory {dirname(outfile)} created\n"))
  }

  if(overwrite || !file.exists(outfile)){
    sink(outfile)
    sapply(l, cat)
    # writeLines(l,outfile)
    sink(NULL)
    print(glue("Writing {outfile}"))
  }

}
