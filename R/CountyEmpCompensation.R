if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get GA county FIPS list
#' @return A data frame contains all 159 names and FIPS for all counties in Georgia
getGACountyFIPS = function() {
  CountyCodes = readxl::read_xlsx('data/extdata/USgeocodes.xlsx') %>% 
    filter(State =='13') %>% 
    filter(County !='000') %>%
    mutate(FIPS = paste0(State,County)) %>%
    select(Name, FIPS)
  return(CountyCodes)
} 


#' Get county-level Employment data at a specific year.
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @param ownership Character, A character string specifying the ownership title. "total" "private" "localgov" "stategov" "federalgov" "totalgov"
#' @param industrylevel Character, A character string between 2-6 specifying digits of NAICS code of interest, only 2 works for now
#' @param datatype Character, A character string specifying topics of interest. "all" "compensation" "establishment" "employment"
#' @return A list of data frames containing data asked for at a specific year.
GAcountyFIPS = getGACountyFIPS()
ObtainCountyEmploymentData = function(year, ownership, industrylevel, datatype) {
  CountyEmpList = list()
  switch_ind = switch(industrylevel, "2" = 2)
  #switch_ind = switch(industrylevel, "3" = 3, "4" = 4, "5" = 5, "6" = 6)
  switch_type = switch(datatype, "all" = c(5,6,7), "compensation" = 7, "establishment" = 5, "employment" = 6)
  switch_own = switch(ownership, "all" = 0, "private" = 5, "localgov" = 3, "stategov" = 2, "federalgov" = 1)
  
  for (fips in GAcountyFIPS['FIPS']) {
    filename = paste0(fips,'.csv')
    year = as.character(year)
    url = paste0('http://www.bls.gov/cew/data/api/', paste0(year, paste0('/a/area/', filename)))
    temp = readr::read_csv(url) %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) %>%
      rename(FIPS = area_fips, ownership = own_code, NAICS = industry_code, establishment = annual_avg_estabs, employment = annual_avg_emplvl, compensation = total_annual_wages) %>%
      mutate(FIPS = as.character(FIPS), ownership = as.character(ownership)) %>%
      filter(ownership != "0", ownership != "8", ownership != "9") %>%
      filter(as.numeric(NAICS) / 10 ^ switch_ind <= 1, as.numeric(NAICS) / 10 ^ switch_ind >= 0.11) %>%
      select(1:5,switch_type) 
    
    if (switch_own != 0) {
      temp = temp %>% 
        filter(ownership == switch_own) 
    } else {
      temp = temp %>% 
        group_by(NAICS) %>% 
        summarise_if(is.numeric, sum)
    }
      
      
      
  }
  
}