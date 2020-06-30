if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get GA county FIPS list
#' @return A data frame contains all 159 names and FIPS for all counties in Georgia
getGACountyFIPS = function() {
  CountyCodes = readxl::read_xlsx('data/extdata/US_geocodes.xlsx') %>% 
    filter(State =='13') %>% 
    filter(County !='000') %>%
    mutate(FIPS = paste0(State,County)) %>%
    select(Name, FIPS)
  return(CountyCodes)
} 


#' Get state-level total Employment data including establishment, employment, and total annual wages at a specific year.
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @param ownership Character, A character string specifying the ownership title. "all" "private" "localgov" "stategov" "federalgov" 
#' @param ByNAICS Boolean, A boolean value that takes the value of "TRUE" if you need to obtain break-down table 
#' @return A list of data frames containing data asked for at a specific year.
GetGAEmploymentData = function(year, ownership, ByNAICS) {
  switch_own = switch(ownership, "all" = c("0","1","2","3","5"), "private" = "5", "localgov" = "3", "stategov" = "2", "federalgov" = "1")
  #aggregated table
  filename = paste0("data/extdata/GA_Compensation_", paste0(year, ".csv"))
  GAtotal = read_csv(filename) %>% 
    filter(industry_code == '10', own_code %in% switch_own) %>%
    select(area_fips, own_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages)
  if(!ByNAICS) {
    return(as.data.frame(GAtotal))
  } else {
    #refilter for breakdown table
    GA_NAICS = read_csv(filename) %>% 
      filter(as.numeric(industry_code) / 10 ^ 6 <= 1, as.numeric(industry_code) / 10 ^ 6 >= 0.11) %>%
      select(area_fips, own_code, year, industry_code, annual_avg_estabs, annual_avg_emplvl, total_annual_wages)
    #adjusted for NA
    for (code in unique(GA_NAICS$own_code)) {
      adjCode = 999999
      temp = GA_NAICS %>% filter(own_code == code)
      ownerSumComp = sum(temp[temp$industry_code != adjCode,]$total_annual_wages)
      ownerSumEmp = sum(temp[temp$industry_code != adjCode,]$annual_avg_emplvl)
      trueSumComp = GAtotal[GAtotal$own_code == code,]$total_annual_wages
      trueSumEmp = GAtotal[GAtotal$own_code == code,]$annual_avg_emplvl
      CompDif = trueSumComp - ownerSumComp
      EmpDif = trueSumEmp - ownerSumEmp
      GA_NAICS[GA_NAICS$industry_code == adjCode & GA_NAICS$own_code == code,]$total_annual_wages = CompDif
      GA_NAICS[GA_NAICS$industry_code == adjCode & GA_NAICS$own_code == code,]$annual_avg_emplvl = EmpDif
      
    }
    return(GA_NAICS)
  }
}


#' write state-level total Employment data including establishment, employment, and total annual wages at a specific year.
#' @return void.
WriteGAEmploymentData = function() {
  yearseq = seq(2015,2019,1)
  GATotalCompensation = data.frame()
  for (year in yearseq) {
    temp = GetGAEmploymentData(year,"all",FALSE)
    GATotalCompensation = rbind(GATotalCompensation, temp)
    filename = paste0("GA_TotalEmp", paste0(year, '.csv'))
    write.csv(GATotalCompensation, paste0("data/", filename))
  }
}


#' Get county-level establishment count 
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
GetCountyEstablishmentCount = function(year) {
  NAICS_Code = readr::read_csv('data/extdata/QCEWDocumentation/QCEW_industry_titles.csv') %>% 
    filter(as.numeric(industry_code) >= 100000) %>% 
    select(industry_code)
  GAcountyFIPS = getGACountyFIPS() 
  CountyTable = data.frame() %>% rbind(NAICS_Code)
  for (fips in unique(GAcountyFIPS$FIPS)) {
    filename = paste0(fips,'.csv')
    url = paste0('http://www.bls.gov/cew/data/api/', paste0(year, paste0('/a/area/', filename)))
    temp = readr::read_csv(url) %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs) %>%
      filter(own_code %in% c("1","2","3","5")) %>%
      filter(as.numeric(industry_code) / 10 ^ 6 <= 1, as.numeric(industry_code) / 10 ^ 6 >= 0.11) %>%
      group_by(industry_code) %>%
      summarise(annual_avg_establishments = sum(annual_avg_estabs))
    colnames(temp)[2] = paste0('FIPS/', paste0(fips,paste0("/", GAcountyFIPS[GAcountyFIPS$FIPS == fips,1])))
    temp = temp %>% right_join(NAICS_Code, by = 'industry_code') 
    temp[as.vector(is.na(temp[,2])),2] = 0.0
    CountyTable = cbind(CountyTable, as.vector(temp[2])) 
  }
  return(CountyTable)
}









