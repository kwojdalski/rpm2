debugonce(rpm2:::.swapCitations)
readr::read_lines('./data/Rnw/chapters/subchapters/2_1_1_Modern_Portfolio_Theory_and_CAPM.Rnw') %>%
  rpm2:::.swapCitations(type = 'Rnw')
