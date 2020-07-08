if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get sector-level GDP for all counties at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @param county A string character specifying the county of interest, or 'all' for all data
#' @param axis A numeric value, 0,1. if 0, each geographical unit will be a col, if 1, row
#' @return A data frame contains selected county GDP by BEA sector industries at a specific year.
GetCountyOriginalGDP = function(year, county, axis) {
  filename = '../data/extdata/GACounty_GDPbySector.csv'
  SectorLevelLineCode = c(3,6,10,11,12,34,35,36,45,50,59,68,75,82,83) # sector level and total 
  total = readr::read_csv(filename) %>% filter(!is.na(LineCode))
  colnum = which(colnames(total) == paste0('gdp',as.character(year)))
  
  total = total %>% select(2:3, which(colnames(total) == paste0('gdp',as.character(year)))) %>% filter(LineCode %in% SectorLevelLineCode) # filter by sepecific year
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

#' Make estimation of blank rows from what GetCountyOriginalGDP returned by county-state establishment ratio
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
EstimateCountyGDP = function(year) {
  # CrossWalk to BEA sector
  cw = readr::read_csv('../data/extdata/CrossWalk_NAICS2ToBEASector.csv')
  filename = paste0("../data/GACounty_estabs_", paste0(year,'.csv'))
  CountyCount = readr::read_csv(filename) %>% 
    select(-1) %>%
    right_join(., cw, by = 'NAICS2') %>% 
    relocate(BEASector,.after = NAICS2) %>% 
    group_by(BEASector) %>%
    summarise_if(is.numeric, sum)

  # Obtain Total GDP of each Sector
  GAColumn = GetCountyOriginalGDP(year, 'all', 0) %>% select(2)
  
  # Calculate GDP difference of each Sector
  RawGDP = GetCountyOriginalGDP(year, 'all', 0) %>% select(-1,-2)
  RawGDP[is.na(RawGDP)] = 0
  GDPDifference = GAColumn$Georgia - rowSums(RawGDP)
  
  # Replace NA by Estimated GDP
  # 1. estimate by county/state raio for each industry
  OriGDP = GetCountyOriginalGDP(year, 'all', 0) %>% select(-1,-2)
  CountyGDP = GetCountyOriginalGDP(year, 'all', 0) %>% select(-1,-2)
  for (row in 1:(nrow(CountyGDP))) {
    key = which(is.na(CountyGDP[row,]))
    if (length(key) != 0 && sum(CountyCount[row,key]) != 0) {
      ratio = CountyCount[row,key] / sum(CountyCount[row,key])
      CountyGDP[row,key] = ratio * GDPDifference[row]
    } else if (length(key) != 0 && sum(CountyCount[row,key]) == 0) {
      CountyGDP[row,key] = GDPDifference[row] / length(key)
    }
  }

  # 2. compared total estimation with county total, then apply shrinkage factor to each county to shrink est total to true total
  trueSum = readr::read_csv('../data/extdata/GACounty_GDPbySector.csv') 
  trueSum = trueSum %>% 
    filter(LineCode  == 1, GeoFips != 13000) %>%
    select(2, which(colnames(trueSum) == paste0('gdp',as.character(year))))
  colnames(trueSum)[2] = 'trueSum'
  for (col in 1:ncol(CountyGDP)) {
    estTotal = sum(CountyGDP[,col])
    trueTotal = as.numeric(trueSum$trueSum[col]) * 1000
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
  CountyGDP = cbind(GetCountyOriginalGDP(year, 'all', 0) %>% select(1), CountyGDP)
  return(CountyGDP)
}

#' Check accuracy of County GDP Estimation
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return two data frames containing data asked for at a specific year: Column Error, Row Error
ShowEstimationAccuracy = function(year) {
  estTable = EstimateCountyGDP(year) 
  estTable = estTable %>% select(-1)
  # Column Error(county total)
  estSum = colSums(estTable)
  trueSum = readr::read_csv('../data/extdata/GACounty_GDPbySector.csv') 
  trueSum = trueSum %>% 
    filter(LineCode  == 1, GeoFips != 13000) %>%
    select(2, which(colnames(trueSum) == paste0('gdp',as.character(year))))
  colnames(trueSum)[2] = 'trueSum'
  ColumnErrorTable = trueSum %>% mutate(trueSum = as.numeric(trueSum) * 1000)
  ColumnErrorTable['estSum'] = estSum
  ColumnErrorTable['dif'] = ColumnErrorTable$estSum - ColumnErrorTable$trueSum
  ColumnErrorTable['errorRate'] = abs(ColumnErrorTable$dif) / ColumnErrorTable$trueSum
  return(ColumnErrorTable)
}


col = ShowEstimationAccuracy(2015)