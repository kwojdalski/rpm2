require(rpm2)
require(assertthat)
require(magrittr)
require(stringr)
require(plyr)
debugonce(rpm2:::.swap)
debugonce(rpm2:::.swapCitations)
debugonce(convertDocs)
file.remove(list.files('./data/Rnw', recursive = T, full.names = T))
convertDocs(type = 'Rmd', path_folder = './data/Rmd2/', new_path_folder = './data/Rnw', recursive = T)
rmarkdown::render('./data/Rmd/masters_thesis.Rmd', intermediates_dir = 'intermediates_dir')

