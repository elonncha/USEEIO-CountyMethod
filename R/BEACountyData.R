if (!require(useeior)) { githubinstall::githubinstall('USEPA/useeior', ref='state_2r') }
if (!require(tidyverse)) { install.packages(tidyverse) }
library(useeior)
library(tidyverse)


#' Get sector-level GDP for all counties at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A data frame contains state GDP for all counties at a specific year.
GetCountyGDP = function(year) {
  filename = '../data/extdata/GACounty_GDPbySector.csv'
  SectorLevelLineCode = c(3,6,10,11,12,34,35,36,45,50,59,68,75,82,83) # sector level and total 
  total = readr::read_csv(filename) %>% filter(!is.na(LineCode))
  colnum = which(colnames(total) == paste0('gdp',as.character(year)))
  
  total = total %>% select(1:4, which(colnames(total) == paste0('gdp',as.character(year))))  # filter by sepecific year
  colnames(total)[5] = 'GDP'
  total = total %>% filter(LineCode %in% SectorLevelLineCode) %>%  # retain only sector-level lines
                    mutate(GDP = as.numeric(GDP) * 1000) # NA if not available
  totalCol = total %>% select(-1) %>% spread(GeoName, GDP) %>% relocate(Georgia, .after = Description) # transpose the table and put total to the front
  return(totalCol)
}


#' Make estimation of blank rows from what GetCountyGDP returned by county-state establishment ratio
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @return A data frame containing data asked for at a specific year.
EstimateGDP = function(year) {
  # CrossWalk to BEA sector
  cw = readr::read_csv('../data/extdata/CrossWalk_NAICS2ToBEASector.csv')
  filename = paste0("../data/County_TotalEstablishmentCount_", paste0(year,'.csv'))
  CountyCount = readr::read_csv(filename) %>% 
    right_join(., cw, by = 'NAICS2') %>% 
    relocate(BEASector,.before = NAICS2) %>% 
    group_by(BEASector) %>%
    summarise_if(is.numeric, sum) %>%
    select(-1)

  # Obtain Total GDP of each Sector
  GAColumn = GetCountyGDP(year)  %>% select(3)
  
  # Calculate GDP difference of each Sector
  RawGDP = GetCountyGDP(year) %>% select(-1,-2,-3)
  RawGDP[is.na(RawGDP)] = 0
  GDPDifference = GAColumn$Georgia - rowSums(RawGDP)
  
  # Replace NA by Estimated GDP
  # 1. estimate by county/state raio for each industry
  OriGDP = GetCountyGDP(year) %>% select(-1,-2,-3)
  CountyGDP = GetCountyGDP(year) %>% select(-1,-2,-3)
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
  CountyGDP = cbind(GetCountyGDP(year) %>% select(1,2,3), CountyGDP)
  return(CountyGDP)
}


#' Check accuracy of County GDP Estimation
#' @param year Integer, A numeric value between 2015-2018 specifying the year of interest
#' @param dim Character, A character string showing whether row error or column error is returned "sector" "county"
#' @return two data frames containing data asked for at a specific year: Column Error, Row Error
ShowEstimationAccuracy = function(year, dim) {
  estTable = EstimateGDP(year) 
  estTable = estTable %>% select(-1,-2,-3)
  if (dim == 'county') {
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
  } else if (dim == 'sector'){
    # Row Error (sector total)
    RowErrorTable = EstimateGDP(year) %>% select(1,2)
    RowErrorTable['estRowSum'] = rowSums(estTable)
    RowErrorTable['trueRowSum'] = EstimateGDP(year)$Georgia
    RowErrorTable['dif'] = rowSums(estTable) - EstimateGDP(year)$Georgia
    RowErrorTable['errorRate'] = abs(RowErrorTable$dif) / RowErrorTable$trueRowSum 
    return(RowErrorTable)
  }
}

col = ShowEstimationAccuracy(2015, 'county')

