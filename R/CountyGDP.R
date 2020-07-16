if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)
source('CrosswalkGenerator.R')
source('CountyEmployment.R')
source('../../stateio/R/UtilityFunctions.R')


GetGeorgiaSummarySectorGDPRatio = function(year) {
  load("../data/extdata/State_GDP_2007_2019.rda")
  cw = unique(readr::read_csv('../data/extdata/Crosswalk_CountyGDPtoBEASummaryIO2012Schema.csv') %>% select(1:4))
  sector_linecode = unique(cw$LineCodeSec)
  summary_linecode = unique(cw$LineCodeSum)
  
  GA_GDP = State_GDP_2007_2019 %>% 
    filter(GeoName == 'Georgia') %>%
    select(2,3,year - 2003) %>%
    filter(LineCode %in% combine(sector_linecode, summary_linecode)) %>% 
    left_join(., cw, by = c('LineCode' = 'LineCodeSum')) %>%
    mutate(GDPRatio = 0)
  colnames(GA_GDP)[3] = 'GDP'
  
  for (i in 1:nrow(GA_GDP)) {
    if (!is.na(GA_GDP$LineCodeSec[i])) {
      GA_GDP$GDPRatio[i] = GA_GDP$GDP[i] / GA_GDP$GDP[which(GA_GDP$LineCode==GA_GDP$LineCodeSec[i])]
    }
  }
  GA_GDP$GDPRatio[is.na(GA_GDP$LineCodeSec)] = 1.0
  return(GA_GDP %>% select(-6) %>% na.omit())
}

#' Get sector-level GDP for all counties at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @param county A string character specifying the county of interest, or 'all' for all data
#' @param axis A numeric value, 0,1. if 0, each geographical unit will be a col, if 1, row
#' @return A data frame contains selected county GDP by BEA sector industries at a specific year.
GetCountyOriginalSectorGDP = function(year, county, axis) {
  filename = '../data/extdata/BEA_County/CAGDP2_GA_2001_2018.csv'
  SectorLevelLineCode = c(3,6,10,11,12,34,35,36,45,50,59,68,75,82,83) # sector level and total 
  total = readr::read_csv(filename) %>% filter(!is.na(LineCode))
  colnum = which(colnames(total) == paste0('gdp',as.character(year)))
  
  total = total %>% select(2,5, which(colnames(total) == paste0('gdp',as.character(year)))) %>% filter(LineCode %in% SectorLevelLineCode) # filter by sepecific year
  colnames(total)[ncol(total)] = 'GDP'
  if (county == 'all'){
    total = total %>%  # retain only sector-level lines
      mutate(GDP = as.numeric(GDP) * 1000) %>% arrange(GeoName)# NA if not available
    totalCol = total %>% spread(GeoName, GDP) %>% relocate(Georgia, .after = LineCode) # transpose the table and put total to the front
    if (axis == 0){return(totalCol)} else if (axis ==1) {return(total)}
    
  } else {
    geoname = paste0(county,', GA')
    total = total %>%  # retain only sector-level lines
      mutate(GDP = as.numeric(GDP) * 1000) %>% # NA if not available
      filter(GeoName == geoname)
    totalCol = total %>% spread(GeoName, GDP) # transpose the table and put total to the front
    if (axis == 0){return(totalCol)} else if (axis ==1) {return(total)}
  }
}


#' Make estimation of blank rows from what GetCountyOriginalSectorGDP returned by county-state establishment ratio
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
EstimateCountySectorGDP = function(year) {
  # CrossWalk to BEA sector
  cw = readr::read_csv('../data/extdata/CrossWalk_NAICS2ToLineCode.csv')
  filename = paste0("../data/GACounty_estabs_", paste0(year,'.csv'))
  CountyEstabCount = readr::read_csv(filename) %>% 
    select(-1) %>%
    right_join(., cw, by = 'NAICS2') %>% 
    relocate(LineCode,.after = NAICS2) %>% 
    group_by(LineCode) %>%
    summarise_if(is.numeric, sum)
  
  # Calculate GDP difference of each Sector
  RawGDP = GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(-1)
  RawGDP[is.na(RawGDP)] = 0
  GDPRowDifference = RawGDP$Georgia - rowSums(RawGDP[,2:ncol(RawGDP)])
  GDPRowDifference[abs(GDPRowDifference)<=2000] = 0
  
  filename2 = '../data/extdata/BEA_County/CAGDP2_GA_2001_2018.csv'
  SectorLevelLineCode = c(3,6,10,11,12,34,35,36,45,50,59,68,75,82,83) # sector level and total 
  t_cs = readr::read_csv(filename2) %>% 
    filter(!is.na(LineCode)) %>% 
    filter(LineCode == 1, GeoName != 'Georgia') %>% 
    arrange(GeoName) %>% select(year - (2001-9))
  colnames(t_cs)[1] = 'countyGDP'
  t_cs = as.numeric(t_cs$countyGDP) * 1000
  GDPColDifference = t_cs - colSums(RawGDP[,2:ncol(RawGDP)])
  
  # Replace NA by Estimated GDP
  # 1. estimate by county/state raio for each industry
  CountyGDP = GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(-1,-2)
  matrixKEY = is.na(CountyGDP)
  for (row in 1:(nrow(CountyGDP))) {
    key = which(is.na(CountyGDP[row,]))
    if (length(key) != 0 && sum(CountyEstabCount[row,key]) != 0) {
      ratio = CountyEstabCount[row,key] / sum(CountyEstabCount[row,key])
      CountyGDP[row,key] = ratio * GDPRowDifference[row]
    } else if (length(key) != 0 && sum(CountyEstabCount[row,key]) == 0) {
      CountyGDP[row,key] = GDPRowDifference[row] / length(key)
    }
  }

  # 2. RAS for data reconciliation
  M0 = CountyGDP
  for (row in (1:nrow(M0))) {
    for (col in (1:ncol(M0))) {
      if (matrixKEY[row,col] == TRUE) {
        M0[row,col] = M0[row,col]
      } else if (matrixKEY[row,col] == FALSE){
        M0[row,col] = 0
      }
    }
  }
  M1 = applyRAS(as.matrix(M0), GDPRowDifference, GDPColDifference, relative_diff = NULL, absolute_diff = 0, max_itr = 100000)
  
  for (row in 1:(nrow(CountyGDP))) {
    for (col in (1:ncol(CountyGDP))) {
      if (matrixKEY[row,col] == TRUE) {
        CountyGDP[row,col] = M1[row,col]
      } else if (matrixKEY[row,col] == FALSE) {
        next
      }
    }
  }

  # Add back original Column to the table
  CountyGDP = cbind(GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(1), CountyGDP)
  return(CountyGDP)
}

#gdp = EstimateCountySectorGDP(2016)


#' Break down sector-level GDP into summary-level GDP by state ratio
#' @param year Integer, A numeric value between 2015-2017 specifying the year of interest
#' @param ite Integer, times of iteration for RAS data reconciliation, 10000 as default
#' @return two data frames containing data asked for at a specific year: Column Error, Row Error
EstimateCountySummaryGDP = function(year, ite = 10000) {
  
  ## step1: initial allocation based on LQ-weighted gdp ratio
  rawratio = GetGeorgiaSummarySectorGDPRatio(year) %>% select(1,4,6)
  sectorGDP = EstimateCountySectorGDP(year)
  summaryGDP = data.frame(LineCode = unique(rawratio$LineCode))
  
  LQ = ComputeEstabLocationQuotient(year)
  for (c in 2:ncol(sectorGDP)) {
    countysector = sectorGDP %>% select(1,c) %>% right_join(.,rawratio, by = c('LineCode'='LineCodeSec'))
    colnames(countysector)[3] = 'LineCodeSum'
    countyLQ = LQ %>% select(1,c)
    colnames(countyLQ)[2] = "LQ"
    county = countysector %>% left_join(., countyLQ, by = 'LineCodeSum') %>% mutate(weightedRatio = 0, normalizedRatio = 0, adjusted = 0)
    
    for (sec in unique(county$LineCode)) {
      sumcodelist = unique(county[county$LineCode == sec,]$LineCodeSum)
      totalLQ = sum(county[county$LineCode == sec,]$LQ)
      if (totalLQ == 0) { next } else {
        for (sum in sumcodelist) {
          county[county$LineCodeSum == sum,]$weightedRatio = county[county$LineCodeSum == sum,]$GDPRatio * (county[county$LineCodeSum == sum,]$LQ / totalLQ)
        }
        for (sum in sumcodelist) {
          county[county$LineCodeSum == sum,]$normalizedRatio = county[county$LineCodeSum == sum,]$weightedRatio / sum (county[county$LineCode == sec,]$weightedRatio)
        }
      }
    }
    
    # compute new gdp ratio
    county$adjusted = county[2] * county$normalizedRatio
    # obtain original ratio for gov spending back becasue 85 is missing
    county[county$LineCode =='83',]$adjusted = as.numeric(county[county$LineCode =='83',][2,2]) * county[county$LineCode =='83',]$GDPRatio
    county[2] = county[8]
    summaryGDP = cbind(summaryGDP, county[2])
  }
  
  
  ### step2: RAS method to reconcile matrix TO BE CONTINUE!!! (RAS based on sector)
  M0 = as.matrix(summaryGDP %>% select(-1))
  
  t_rs = State_GDP_2007_2019 %>% filter(GeoName == 'Georgia', LineCode %in% gdp2015$LineCode) %>% select(year - 2003)
  colnames(t_rs)[1] = 'sectorGDP'
  t_rs = t_rs$sectorGDP
  
  filename = '../data/extdata/BEA_County/CAGDP2_GA_2001_2018.csv'
  SectorLevelLineCode = c(3,6,10,11,12,34,35,36,45,50,59,68,75,82,83) # sector level and total 
  t_cs = readr::read_csv(filename) %>% 
    filter(!is.na(LineCode)) %>% 
    filter(LineCode == 1, GeoName != 'Georgia') %>% 
    arrange(GeoName) %>% select(year - (2001-9))
  colnames(t_cs)[1] = 'countyGDP'
  t_cs = as.numeric(t_cs$countyGDP) * 1000
  
  M1 = applyRAS(M0, t_rs, t_cs, relative_diff = NULL, absolute_diff = 0, max_itr = ite)

  return(summaryGDP)

}


gdp2015 = EstimateCountySummaryGDP(2015)


origdp2015 = GetCountyOriginalSectorGDP(2015, 'all', 0)









