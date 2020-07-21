library(tidyverse)
source('CrosswalkGenerator.R')
source('supporter.R')

#' GetGeorgiaEmploymentData
#' 
#' This function is to return dataframes containing Georgia employement data from
#' the year of 2015 to 2018, during which QCEW data is available. There are five types 
#' of employment data returned: employment count, emp compensation, and establishment counts,
#' average employment per estab, and average comp per estab.
#' 
#' 
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
GetGeorgiaEmploymentData = function(year) {
  
  GAlevel = readr::read_csv(paste0("../data/extdata/QCEW_County_Emp/", paste0(year,"/13000.csv"))) %>% 
    filter(industry_code %in% NAICS2) %>% 
    select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) %>%
    filter(annual_avg_emplvl != total_annual_wages) %>%
    group_by(industry_code) %>%
    summarise(estabs = sum(annual_avg_estabs), emp = sum(annual_avg_emplvl), comp = sum(total_annual_wages), empPerEstab = emp / estabs, compPerEstab = comp / estabs)
  
  return(GAlevel)
}



#' GetCountyEmploymentData
#' 
#' This function is to return dataframes binding employement data of each county from
#' the year of 2015 to 2018, during which QCEW data is available, into one data frame. 
#' 
#' 
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
#' @export GACounty_Emp_Raw_xxxx.csv
GetCountyEmploymentData = function(year) {
  GAcountyFIPS = getGACountyFIPS() %>% arrange(Name) # county fips
  allcounty = data.frame() # blank data frame
  
  for (fips in unique(GAcountyFIPS$fips)) {
    filename = paste0(GAcountyFIPS[GAcountyFIPS$fips == fips,]$Name,paste0(year,'.csv'))
    url = paste0('../data/extdata/QCEW_County_Emp/', paste0(paste0(year,"/"), filename))
    countyraw = readr::read_csv(url) 
    countytotal = countyraw %>% filter(own_code == '0') %>% select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) 
    countydetail = countyraw %>% 
      select(area_fips, own_code, industry_code, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages) %>%
      filter(own_code %in% c("1","2","3","5"), industry_code %in% NAICS2) %>% rbind(., countytotal)
    allcounty = rbind(allcounty, countydetail)
  }
  ##filename = paste0('../data/GACounty_Emp_Raw_', paste0(year,'.csv'))
  ##readr::write_csv(allcounty, filename)
  return(allcounty)
}



#' EstimateCountyEmploymentData
#' 
#' This function is to return dataframes containing county-level employement data from
#' the year of 2015 to 2018, during which QCEW data is available, using the export from 
#' GetCountyEmployment Data. There are three types of employment data returned: employment count, 
#' emp compensation, and establishment counts, among which emp count and emp compensation.
#' 
#' 
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @param type Character, types of employment data,  'emp', 'comp' 'estabs'
#' @return A data frame containing data asked for at a specific year.
#' @export GACounty_Emp/estabs/Comp_xxxx.csv
EstimateCountyEmploymentData = function(year, type) {
  
  # preparation
  filename = paste0('../data/GACounty_Emp_Raw_', paste0(year,'.csv')) #export from GetCountyEmploymentData
  raw = readr::read_csv(filename) # raw county-lvl data
  GAlevel = GetGeorgiaEmploymentData(year) # call GA emp data
  NAICS2 = GAlevel$industry_code # industry codes
  GAcountyFIPS = getGACountyFIPS() %>% arrange(Name) # county fips
  CountyTable = data.frame() %>% rbind(as.data.frame(NAICS2)) # blank df
  
  ### ESTABLISHMENT 
  if (type == 'estabs') { # no estimation needed，true value output
    estabsTable = raw %>% 
      select(area_fips, industry_code, annual_avg_estabs) %>%
      filter(industry_code %in% NAICS2) %>%
      group_by(area_fips, industry_code) %>%
      summarise(estabs = sum(annual_avg_estabs)) %>% left_join(., GAcountyFIPS, by = c('area_fips' = 'fips')) %>%
      mutate(Name = paste0(Name, paste0('/',area_fips))) %>%
      rename(NAICS2 = industry_code)
    
    output = estabsTable[,c(2,3,4)] %>% arrange(Name) %>% spread(Name, estabs) #spread rows to columns
    output[is.na(output)] = 0  #set NA to 0 (no data loss here since NA appears when there is 0 estab in one industry of a county)
    
    return(output)
  }
  ##filename = paste0('../data/GACounty_estabs_', paste0(year,'.csv'))
  ##readr::write_csv(output, filename)
  

  
  ### EMPLOYMENT/EMP COMP 
  

    
    if (type %in% c('emp','comp')) { # estimation needed, partially estimated value output
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
  
  return(CountyTable)
}






#' ComputeEstabLocationQuotient
#' 
#' Get county-level estab data at BEA summary level: an update of GetCountyEmploymentData
#' 
#' 
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
