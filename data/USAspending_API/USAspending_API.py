import requests
import numpy as np
from bs4 import BeautifulSoup
import pandas as pd
import copy
import os
os.chdir('/Users/eloncha/Documents/GitHub/USEEIOStateMethod/USAspending_API/data')
base_url = 'https://api.usaspending.gov'
stateTable = pd.read_csv('StateAbbreviation.csv')
pscTable = pd.read_csv('RandDCodes.csv').iloc[:,0:6].dropna()
stateCode = stateTable['Code']

### Get NAICS Table
def apiCall(url, method = ['GET','POST'], filter = {}):
    if method =='GET':
        resp = requests.get(url)
        outcome = resp.json()
        return outcome
    elif method == 'POST':
        resp = requests.post(url, json = filter)
        outcome = resp.json()
        return outcome

def getNAICSTable(digit = [2,4,6]):
    topNAICS_url = '/api/v2/references/naics/'
    NAICS2 = pd.DataFrame(apiCall(base_url + topNAICS_url, method='GET')['results']).iloc[:, 0:2]
    NAICS2.columns = ['NAICS2', 'TopSector']
    NAICS4 = pd.DataFrame(columns=['NAICS4', 'MedSector', 'NAICS2'])
    NAICS6 = pd.DataFrame(columns=['NAICS6', 'BotSector', 'NAICS2', "NAICS4"])
    if digit == 2:
        return NAICS2
    elif digit == 4:
        for topCode in NAICS2['NAICS2'].values:
            medNAICS_url = '/api/v2/references/naics/{0}/'.format(str(topCode))
            medNAICS = pd.DataFrame(apiCall(base_url + medNAICS_url, method = 'GET')['results'][0]['children']).iloc[:,0:2]
            medNAICS.columns = ['NAICS4','MedSector']
            medNAICS['NAICS2'] = str(topCode)
            NAICS4 = pd.merge(NAICS4, medNAICS, how = 'outer')
        NAICS4 = pd.merge(NAICS4, NAICS2, how = 'right', on = ['NAICS2'])
        NAICS4 = NAICS4.reindex(columns = ['NAICS2','TopSector', 'NAICS4','MedSector'])
        return NAICS4
    elif digit == 6:
        for topCode in NAICS2['NAICS2'].values:
            medNAICS_url = '/api/v2/references/naics/{0}/'.format(str(topCode))
            medNAICS = pd.DataFrame(apiCall(base_url + medNAICS_url, method = 'GET')['results'][0]['children']).iloc[:,0:2]
            medNAICS.columns = ['NAICS4','MedSector']
            medNAICS['NAICS2'] = str(topCode)
            NAICS4 = pd.merge(NAICS4, medNAICS, how = 'outer')
        NAICS4 = pd.merge(NAICS4, NAICS2, how = 'right', on = ['NAICS2'])
        NAICS4 = NAICS4.reindex(columns = ['NAICS2','TopSector', 'NAICS4','MedSector'])
        for medCode in NAICS4['NAICS4'].values:
            dig2, dig4 = medCode[0:2], medCode[0:4]
            botNAICS_url = '/api/v2/references/naics/{0}/'.format(str(medCode))
            botNAICS = pd.DataFrame(apiCall(base_url + botNAICS_url, method='GET')['results'][0]['children']).iloc[:,0:2]
            botNAICS.columns = ['NAICS6', 'BotSector']
            botNAICS['NAICS2'] = str(dig2)
            botNAICS['NAICS4'] = str(dig4)
            NAICS6 = pd.merge(NAICS6, botNAICS, how='outer')
        NAICS6 = pd.merge(NAICS6, NAICS4, how = 'right', on = ['NAICS2','NAICS4'])
        NAICS6 = NAICS6.reindex(columns = ['NAICS2','TopSector', 'NAICS4','MedSector', 'NAICS6','BotSector'])
        return NAICS6
    else:
        print ('Wrong Digits.')
NAICS2 = getNAICSTable(2)
NAICS4 = getNAICSTable(4)
NAICS6 = getNAICSTable(6)
NAICS6 = pd.read_csv('NAICS6Table.csv')


### search by award (disaggregated)
#https://github.com/fedspendingtransparency/usaspending-api/blob/master/usaspending_api/api_contracts/contracts/v2/search/spending_by_award.md
startDate = '2012-01-01' # manual time setting
endDate = '2012-12-31'
TimePeriodObject = {'start_date': startDate,
                    'end_date': endDate}
AwardTypesObject = ['A','B','C','D'] # contract only
orderList = []
tempList = []

search_url2 = '/api/v2/search/spending_by_award/'
for state in stateCode[10:11]:  #loop through all states
    LocationObject = {'country': 'USA', 'state': state}
    PSCCodeObject = {'require':[["Service", 'B' , 'B5' ]]}
    for id in NAICS6['NAICS6']:  #loop through all NAICS sectors
        NAICSCodeObject = {'require':[str(id)]}
        filterObject = {'time_period': [TimePeriodObject],
                        'place_of_performance_locations':[LocationObject],
                        'naics_codes': NAICSCodeObject,
                        'award_type_codes': AwardTypesObject,
                        'psc_codes': PSCCodeObject
                        } #filterObject
        apiData = {'filters': filterObject,
                   "fields": ["Award ID","Recipient Name","Start Date","End Date","Issued Date","Award Amount","Awarding Agency","Contract Award Type", "Place of Performance Country Code", "Place of Performance State Code","Place of Performance Zip5"],
                   'limit': 100,
                   'page': 1}
        response = apiCall(base_url + search_url2, method = 'POST', filter = apiData)
        orderList.append(response['results'])
        if response['page_metadata']['hasNext'] == True:  #in case there are multiple pages, update filter to fetch next page
            apiData['page'] += 1
            response = apiCall(base_url + search_url2, method = 'POST', filter = apiData)
            orderList.append(response['results'])
        for page in orderList: #store state data in tempList
            for item in page:
                item['NAICS'] = id
                item['PSC'] = 'Service'
                tempList.append(item)
        orderList = []

    print ('State {0} finished'.format(state))
    df = pd.DataFrame(tempList)
    dfname = state + 'awards1.csv'
    df.to_csv(dfname)







### test
search_url2 = '/api/v2/search/spending_by_award/'
startDate = '2012-01-01' # manual time setting
endDate = '2012-12-31'
TimePeriodObject = {'start_date': startDate, 'end_date': endDate}
AwardTypesObject = [['A','B','C','D'], ['02', '03', '04', '05']] # contract A-D and grant 02-05 only , '02', '03', '04', '05'
LocationObject = {'country': 'USA'}

#construct PSCCodeObject
psclist = []
for i in np.arange(0, pscTable.shape[0]) :
    psc = [pscTable.iloc[i,3], pscTable.iloc[i,1][0:2], str(pscTable.iloc[i,1][0:3]), str(pscTable.iloc[i,1][0:])]
    psclist.append(psc)

testNAICS = [541711,541712]
orderList = []
tempList = []

for id in testNAICS:  #loop through all NAICS sectors
    for type in AwardTypesObject[0:1]:
        i = 0
        for psc in psclist:

            NAICSCodeObject = {'require':[str(id)]}
            PSCCodeObject = {'require': [psc]}
            filterObject = {'time_period': [TimePeriodObject],
                        'place_of_performance_locations':[LocationObject],
                        'naics_codes': NAICSCodeObject,
                        'award_type_codes': type,
                        'psc_codes': PSCCodeObject
                        } #filterObject
            apiData = {'filters': filterObject,
                "fields": ["Award ID","Recipient Name","Start Date","End Date","Award Amount","Awarding Agency","Contract Award Type", "Place of Performance State Code","Place of Performance Zip5"],
                'limit': 100,
                'page': 1}
            response = apiCall(base_url + search_url2, method = 'POST', filter = apiData)
            orderList.append(response['results'])
            if response['page_metadata']['hasNext'] == True:  #in case there are multiple pages, update filter to fetch next page
                apiData['page'] += 1
                response = apiCall(base_url + search_url2, method = 'POST', filter = apiData)
                orderList.append(response['results'])
            for page in orderList: #store state data in tempList
                for item in page:
                    item['type'] = type
                    item['NAICS'] = id
                    item['PSC'] = psc[3]
                    tempList.append(item)
            orderList = []
            i += 1
            print ('{0} done, {1} left'.format(psc, pscTable.shape[0] - i))

df = pd.DataFrame(tempList)




dfname = 'test.csv'
df.to_csv(dfname)

