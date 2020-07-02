if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get GA county FIPS list
#' @return A data frame contains all 159 names and FIPS for all counties in Georgia
getGACountyFIPS = function() {
  CountyCodes = readr::read_csv('../data/extdata/GA_County_FIPS.csv')
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
  filename = paste0("../data/extdata/GA_Compensation_", paste0(year, ".csv"))
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
    
    #adjusted for NA: smoothing under same ownership codes
    for (code in unique(GA_NAICS$own_code)) {
      codetable = GA_NAICS %>% filter(own_code == code, annual_avg_estabs != 0, total_annual_wages != 0)
      avgCompPerEstab = sum(codetable$total_annual_wages) / sum(codetable$annual_avg_estabs)
      CompDif = as.numeric(GAtotal[GAtotal$own_code == code, 'total_annual_wages'] - sum(codetable$total_annual_wages))
      ToBeAdj = GA_NAICS %>% filter(own_code == code, annual_avg_estabs != 0, total_annual_wages == 0)
      TotalEstab = sum(ToBeAdj$annual_avg_estabs)
      for (ind in ToBeAdj$industry_code) {
        position = which(GA_NAICS$own_code == code)[which(GA_NAICS$own_code == code) %in% which(GA_NAICS$industry_code == ind)]
        GA_NAICS$total_annual_wages[position] = GA_NAICS$annual_avg_estabs[position] / TotalEstab * CompDif
      }
    }
        
  }
    return(GA_NAICS %>% filter(own_code %in% switch_own))
}
#' write state-level total Employment data including establishment, employment, and total annual wages at a specific year.
#' @return void.
WriteGAEmploymentData = function() {
  yearseq = seq(2015,2019,1)
  for (year in yearseq) {
    GATotalCompensation = GetGAEmploymentData(year, "all", FALSE)
    filename = paste0("GA_TotalEmp_", paste0(year, '.csv'))
    readr::write_csv(GATotalCompensation, paste0("../data/", filename))
  }
}
WriteGAEmploymentData()


#' Get county-level establishment count 
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
GetCountyEstablishmentCount = function(year) {
  NAICS2 = c('11','21','22','23','31-33','42','44-45','48-49','51','52','53','54','55','56','61','62','71','72','81', '92')
  GAcountyFIPS = getGACountyFIPS() 
  CountyTable = data.frame() %>% rbind(as.data.frame(NAICS2))
  for (fips in unique(GAcountyFIPS$fips)) {
    filename = paste0(fips,'.csv')
    url = paste0('http://www.bls.gov/cew/data/api/', paste0(year, paste0('/a/area/', filename)))
    temp = readr::read_csv(url) %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs) %>%
      filter(own_code %in% c("1","2","3","5"), industry_code %in% NAICS2) %>%
      group_by(industry_code) %>%
      summarise(totalEst = sum(annual_avg_estabs))
    colnames(temp)[2] = paste0('FIPS/', paste0(fips,paste0("/", GAcountyFIPS[GAcountyFIPS$fips == fips,2])))
    temp = temp %>% right_join(as.data.frame(NAICS2), by = c('industry_code'='NAICS2')) 
    temp[as.vector(is.na(temp[,2])),2] = 0
    CountyTable = CountyTable %>% left_join(., temp, by = c('NAICS2' = 'industry_code'))
  }
  return(CountyTable)
}
write_csv(GetCountyEstablishmentCount(2018), "../data/County_TotalEstablishmentCount_2018.csv")

#' Compute county-state establishment ratio: county count normalized by state count
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
ComputeCountyStateEstablishmentRatio = function(year) {
  NAICS_Code = readr::read_csv('../data/extdata/QCEWDocumentation/QCEW_industry_titles.csv') %>% 
    filter(as.numeric(industry_code) >= 100000) %>% 
    select(industry_code)
  GAColumn = GetGAEmploymentData(year,'all', TRUE) %>% group_by(industry_code) %>% summarise(total_estabs = sum(annual_avg_estabs)) %>% right_join(NAICS_Code, by = 'industry_code')
  GAColumn[as.vector(is.na(GAColumn[,2])),2] = 0
  filename = paste0("../data/County_TotalEstablishmentCount_", paste0(year,'.csv'))
  CountyTable = readr::read_csv(filename) %>% mutate(industry_code = as.character(industry_code))
  #row.names(CountyTable) = CountyTable$industry_code
  CountyTable = cbind(CountyTable, GAColumn[,'total_estabs'])
  #check consistency of data
  for (i in 1:nrow(CountyTable)) {
    if (sum(CountyTable[i,2:(ncol(CountyTable)-1)]) != CountyTable[i,ncol(CountyTable)]) {
      CountyTable[i,ncol(CountyTable)] = sum(CountyTable[i,2:(ncol(CountyTable)-1)])
    }
  }
  # calculate ratio
  for (i in 1:nrow(CountyTable)) {
    if (CountyTable[i,ncol(CountyTable)] != 0){
      for (obs in 2:(ncol(CountyTable)-1)) {
        CountyTable[i,obs] = CountyTable[i,obs] / CountyTable[i,ncol(CountyTable)]
      }
    }
  }
  return(CountyTable)
}


#' Compute county compensation estimation based on county-state establishment ratio
#' @param year Integer, A numeric value between 2015-2019 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
ComputeCountyCompensation = function(year) {
  GAColumn = GetGAEmploymentData(year,'all', TRUE) %>% 
    group_by(industry_code) %>% 
    summarise(total_comp = sum(total_annual_wages)) %>% 
    right_join(NAICS_Code, by = 'industry_code')
  GAColumn[is.na(GAColumn)] = 0
  ratioTable = ComputeCountyStateEstablishmentRatio(year) %>% cbind(., GAColumn['total_comp'])
  
  for (row in 1:nrow(ratioTable)) {
    for (county in 2:(ncol(ratioTable)-2)) {
      ratioTable[row,county] = ratioTable[row,county] * ratioTable[row,ncol(ratioTable)]
    }
  }
  return(ratioTable)
}


