
# check the availability of packages
if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)

#' Get industry-level Compensation for state of Georgia at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state Compensation for state of Georgia at a specific year.
getGAEmpCompensation = function(year) {
  GAEmpCompensation = useeior::State_Compensation_2007_2017 %>% 
    filter(GeoName =='Georgia') %>% 
    select(GeoName, LineCode, Description, contains(as.character(year)))
  return(GAEmpCompensation)
}


#' Get industry-level Tax for state of Georgia at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state Tax for state of Georgia at a specific year.
getGATax = function(year) {
  # Load pre-saved state Tax 2007-2017
  GATax = useeior::State_Tax_2007_2017 %>% filter(GeoName =='Georgia') %>% GA
  GATax = StateTax[, c("GeoName", "LineCode", as.character(year))]
  return(GATax)
}