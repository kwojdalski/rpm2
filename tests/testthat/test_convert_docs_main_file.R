require(pacman)
p_load(assertthat,
glue,
magrittr,
stringr,
plyr,
purrr,
purrrlyr,
knitr,
readr
)
convertDocs(type = 'Rmd', path_folder = './data/Rmd2/', new_path_folder = './data/Rnw', recursive = F, standalone = T, is_child = F)


debugonce(convertDocs)
debugonce(rpm2:::.swapItems)

convertDocs(type = 'Rmd', path_folder = './data/Rmd2/', new_path_folder = './data/Rnw', recursive = F)
