if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)
source('CrosswalkGenerator.R')

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
#' @details NOT IN USE, ABANDONED
GetGAEmploymentData = function(year, ownership, ByNAICS) {
  switch_own = switch(ownership, "all" = c("0","1","2","3","5"), "private" = "5", "localgov" = "3", "stategov" = "2", "federalgov" = "1")
  #aggregated table
  filename = paste0("../data/extdata/QCEW_GA_Emp/GA_Emp_", paste0(year, ".csv"))
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

#' Get county-level employment data
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @param type Character, types of employment data,  'emp', 'comp' 'estabs'
#' @return A data frame containing data asked for at a specific year.
GetCountyEmploymentData = function(year, type) {
  NAICS2 = c('11','21','22','23','31-33','42','44-45','48-49','51','52','53','54','55','56','61','62','71','72','81', '92')
  GAcountyFIPS = getGACountyFIPS() 
  CountyTable = data.frame() %>% rbind(as.data.frame(NAICS2))
  
  # compute GA empPerEstab Ratio and CompPerEstab Ratio for filling NA
  GAlevel = readr::read_csv(paste0("../data/extdata/QCEW_County_Emp/", paste0(year,"/13000.csv"))) %>% 
    filter(industry_code %in% NAICS2) %>% 
    select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) %>%
    filter(annual_avg_emplvl != total_annual_wages) %>%
    group_by(industry_code) %>%
    summarise(estabs = sum(annual_avg_estabs), emp = sum(annual_avg_emplvl), comp = sum(total_annual_wages), empPerEstab = emp / estabs, compPerEstab = comp / estabs)
  
  # loop through all counties in GA
  for (fips in unique(GAcountyFIPS$fips)) {
    filename = paste0(GAcountyFIPS[GAcountyFIPS$fips == fips,]$Name,paste0(year,'.csv'))
    url = paste0('../data/extdata/QCEW_County_Emp/', paste0(paste0(year,"/"), filename))
    countyraw = readr::read_csv(url) 
    countytotal = countyraw %>% filter(own_code == '0') %>% select(annual_avg_estabs, annual_avg_emplvl, total_annual_wages) 
    countydetail = countyraw %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) %>%
      filter(own_code %in% c("1","2","3","5"), industry_code %in% NAICS2) 
    
    if (type == 'estabs') { # no estimation needed
      estabsTable = temp %>% group_by(industry_code) %>% summarise(estabs = sum(annual_avg_estabs))
      colnames(estabsTable)[2] = paste0('FIPS/', paste0(fips,paste0("/", GAcountyFIPS[GAcountyFIPS$fips == fips,2])))
      estabsTable = estabsTable %>% right_join(as.data.frame(NAICS2), by = c('industry_code'='NAICS2')) 
      estabsTable[as.vector(is.na(estabsTable[,2])),2] = 0
      CountyTable = CountyTable %>% left_join(., estabsTable, by = c('NAICS2' = 'industry_code'))
    }
    
    if (type %in% c('emp','comp')) { # estimation needed
      key = which(countydetail$annual_avg_emplvl == countydetail$total_annual_wages)
      Table = countydetail %>% 
        select(area_fips, own_code, industry_code, annual_avg_emplvl, total_annual_wages, annual_avg_estabs) %>%
        rename(emp = annual_avg_emplvl, comp = total_annual_wages, estab = annual_avg_estabs)
      ## Step1: substitute NA with estimated value
      for (i in key) {
        Table$emp[i] = round(Table$estab[i] * GAlevel$empPerEstab[GAlevel$industry_code == Table$industry_code[i]],0)
        Table$comp[i] = round(Table$estab[i] * GAlevel$compPerEstab[GAlevel$industry_code == Table$industry_code[i]],0)
      }
      ## Step2: verify the difference between est sum and true sum and apply adjust factor 
      estEmpSum = sum(Table$emp)
      estCompSum = sum(Table$comp)
      trueEmpSum = countytotal$annual_avg_emplvl
      trueCompSum = countytotal$total_annual_wages
      EmpDif = estEmpSum - trueEmpSum
      CompDif = estCompSum - trueCompSum
      
      EmpAdjFactor = 1 - EmpDif / sum(Table$emp[key])
      CompAdjFactor = 1 - CompDif / sum(Table$comp[key])
      for (i in key) {
        Table$emp[i] = round(Table$emp[i] * EmpAdjFactor, 0)
        Table$comp[i] = round(Table$comp[i] * CompAdjFactor ,0)
      }
      
      if (type == 'emp') {
        empTable = Table %>% 
          group_by(industry_code) %>% 
          summarise(totalemp = sum(emp)) %>% 
          right_join(as.data.frame(NAICS2), by = c('industry_code'='NAICS2')) 
        empTable[as.vector(is.na(empTable[,2])),2] = 0
        colnames(empTable)[2] = paste0('FIPS/', paste0(fips,paste0("/", GAcountyFIPS[GAcountyFIPS$fips == fips,2])))
        CountyTable = CountyTable %>% left_join(., empTable, by = c('NAICS2' = 'industry_code'))
      } else {
        compTable = Table %>% 
          group_by(industry_code) %>% 
          summarise(totalcomp = sum(comp)) %>% 
          right_join(as.data.frame(NAICS2), by = c('industry_code'='NAICS2')) 
        compTable[as.vector(is.na(compTable[,2])),2] = 0
        colnames(compTable)[2] = paste0('FIPS/', paste0(fips,paste0("/", GAcountyFIPS[GAcountyFIPS$fips == fips,2])))
        CountyTable = CountyTable %>% left_join(., compTable, by = c('NAICS2' = 'industry_code'))
      }
    }
  }
  
  return(CountyTable)
}


#' Get county-level estab data at BEA summary level: an update of GetCountyEmploymentData
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
ComputeEstabLocationQuotient = function(year) {
  CW = getCrosswalk('bea_summary','naics')
  colnames(CW) = c('BEA','BEA_DES','NAICS','NAICS_DES')
  CW = CW %>% filter(NAICS >= 1e+5)
  CW2 = readr::read_csv('../data/extdata/Crosswalk_CountyGDPtoBEASummaryIO2012Schema.csv')
  GAcountyFIPS = getGACountyFIPS() 
  GAcountyName = sort(GAcountyFIPS$Name)
  
  for (name in GAcountyName) {
    filename = paste0(name,paste0(year,'.csv'))
    url = paste0('../data/extdata/QCEW_County_Emp/', paste0(paste0(year,"/"), filename))
    countyraw = readr::read_csv(url) 
    countydetail = countyraw %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs) %>%
      mutate(industry_code = as.numeric(industry_code)) %>%
      filter(own_code %in% c("1","2","3","5"), industry_code >= 1e+5) %>%
      group_by(industry_code) %>% summarise(estab = sum(annual_avg_estabs))
    colnames(countydetail)[2] = paste0(name, ', GA')
    CW = CW %>% left_join(., countydetail, by = c('NAICS' = 'industry_code'))
    CW[is.na(CW)] = 0
    
  }
  
  CountyEstab = CW %>% 
    group_by(BEA,BEA_DES) %>% 
    summarise_if(is.numeric, sum) %>% 
    select(-3) %>% 
    full_join(.,CW2, by = c('BEA'= 'BEA_2012_Summary_Code')) %>%
    relocate(LineCodeSec, DescriptionSec, LineCodeSum, DescriptionSum, BEA_2012_Summary_Name, .after = BEA_DES) %>%
    select(-2,-4,-7) %>% arrange(LineCodeSum) %>% mutate(LineCodeSec = as.character(LineCodeSec))
  CountyEstab = CountyEstab[!is.na(CountyEstab$LineCodeSec),]
  CountyEstab[is.na(CountyEstab)] = 0
  CountyEstab  = CountyEstab %>% group_by(LineCodeSum) %>% summarise_if(is.numeric, sum)
  
  CountyLQ = CountyEstab
  
  CountyTotal = colSums(CountyLQ[,2:ncol(CountyLQ)])
  GATotal = sum(CountyTotal)
  for (row in (1:nrow(CountyLQ))) {
    GAIndtotal = sum(CountyLQ[row,2:ncol(CountyLQ)])
    for (col in (2:ncol(CountyLQ))) {
      if (GAIndtotal ==0) {
        CountyLQ[row,col] = 0
      } else {
        CountyLQ[row,col] = (CountyLQ[row,col] / CountyTotal[col-1]) / (GAIndtotal / GATotal)
      }
    }
  }
    
  return(CountyLQ)
}
