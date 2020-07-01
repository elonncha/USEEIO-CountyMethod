if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get sector-level GDP for all counties at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A data frame contains state GDP for all counties at a specific year.
GetCountyGDP = function(year) {
  filename = '../data/extdata/GACounty_GDPbySector.csv'
  SectorLevelLineCode = c(1,3,6,10,11,12,34,35,36,45,50,59,68,75,82,83)
  
  total = readr::read_csv(filename)
  total = total %>% select(1:4, which(colnames(total) == paste0('gdp',as.character(year))))  # filter by sepecific year
  colnames(total)[5] = 'GDP'
  total = total %>% filter(LineCode %in% SectorLevelLineCode) %>%  # retain only sector-level lines
                    mutate(GDP = as.numeric(GDP) * 1000) # NA if not available
                    
  totalCol = total %>% select(-1) %>% spread(GeoName, GDP) %>% relocate(Georgia, .after = Description)
  
    
  return(total)
}
