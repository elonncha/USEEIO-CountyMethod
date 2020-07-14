if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)
source('CrosswalkGenerator.R')
source('CountyEmployment.R')

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
  
  for (i in 1:nrow(GA_GDP)) {
    if (!is.na(GA_GDP$LineCodeSec[i])) {
      GA_GDP$GDPRatio[i] = GA_GDP$'2017'[i] / GA_GDP$'2017'[which(GA_GDP$LineCode==GA_GDP$LineCodeSec[i])]
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
      mutate(GDP = as.numeric(GDP) * 1000) # NA if not available
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
  
  # Replace NA by Estimated GDP
  # 1. estimate by county/state raio for each industry
  CountyGDP = GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(-1,-2)
  for (row in 1:(nrow(CountyGDP))) {
    key = which(is.na(CountyGDP[row,]))
    if (length(key) != 0 && sum(CountyEstabCount[row,key]) != 0) {
      ratio = CountyEstabCount[row,key] / sum(CountyEstabCount[row,key])
      CountyGDP[row,key] = ratio * GDPRowDifference[row]
    } else if (length(key) != 0 && sum(CountyEstabCount[row,key]) == 0) {
      CountyGDP[row,key] = GDPRowDifference[row] / length(key)
    }
  }

  # 2. compared total estimation with county total, then apply shrinkage factor to each county to shrink est total to true total
  OriGDP = GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(-1,-2)
  countySum = readr::read_csv('../data/extdata/BEA_County/CAGDP2_GA_2001_2018.csv') 
  countySum = countySum %>% 
    filter(LineCode  == 1, GeoName != 'Georgia') %>%
    select(2, which(colnames(countySum) == paste0('gdp',as.character(year)))) %>% arrange(GeoName)
  colnames(countySum)[2] = 'countySum'
  for (col in 1:ncol(CountyGDP)) {
    estTotal = sum(CountyGDP[,col])
    trueTotal = as.numeric(countySum$countySum[col]) * 1000
    dif = estTotal - trueTotal
    errorRate = abs(dif) / trueTotal
    key = which(is.na(OriGDP[,col]))
    if (length(key) != 0) {
      shrinkFactor = dif / sum(CountyGDP[key,col])
      CountyGDP[key,col] = CountyGDP[key,col] * (1 - shrinkFactor)
    }
  }
  CountyGDP[is.na(CountyGDP)] = 0
  # Add back original Column to the table
  CountyGDP = cbind(GetCountyOriginalSectorGDP(year, 'all', 0) %>% select(1), CountyGDP)
  return(CountyGDP)
}


#' Break down sector-level GDP into summary-level GDP
#' @param year Integer, A numeric value between 2015-2017 specifying the year of interest
#' @return two data frames containing data asked for at a specific year: Column Error, Row Error
EstimateCountySummaryGDP = function(year) {
  ratio = GetGeorgiaSummarySectorGDPRatio(year)
  sectorGDP = EstimateCountySectorGDP(year)
  summaryGDP = data.frame(LineCode = unique(ratio$LineCode))
  
  for (c in 2:ncol(sectorGDP)) {
    summaryGDP$newcol = 0
    colnames(summaryGDP)[c] = colnames(sectorGDP)[c]
    for (i in 1:nrow(summaryGDP)) {
      summaryGDP[i,c] = sectorGDP[sectorGDP$LineCode == ratio$LineCodeSec[which(ratio$LineCode == summaryGDP$LineCode[i])], c] * ratio$GDPRatio[which(ratio$LineCode == summaryGDP$LineCode[i])]
    }
  }
  
  ##TODO: specialty of counties (estabs?)
  return(summaryGDP)
}

