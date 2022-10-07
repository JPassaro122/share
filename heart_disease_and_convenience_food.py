
# This file contains the code that produces the analysis located at:
# https://www.linkedin.com/pulse/heart-disease-pandemic-its-correlation-convenience-density-passaro/
# The Heart Disease Pandemic and its correlation to Convenience Restaurant Density

#!/usr/bin/env python
# coding: utf-8

# The Heart Disease Pandemic and its correlation to Convenience Restaurant Density
# 
# The following notebook aims to answer the question:  Does the number of Convenience Resturaunts in a county relate to the county heart disease death rate.

# The following cell sets up the majority of our needed parameters to perform the analysis.

# In[1]:



import requests
import urllib.request
import time
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
from urllib.request import urlopen
import json 
from geopy.geocoders import Nominatim 
import requests 
import matplotlib.cm as cm
import matplotlib.colors as colors
import folium 
import plotly.figure_factory as ff
from pandasql import sqldf
import geopandas


# The following step accesses data from my computer wich was obtained from the CDC website of causes of death.  I obtained the data by county based on deaths caused by heart disease.  I then ensure that the FIPS code is string and that it is the correct length to use later in the analysis.

# In[9]:


cardio = pd.read_excel(r'C:\Users\jeffp\Documents\Personal\Learning\Coursrera\Assignment\final assignment\cardio_deaths.xlsx')

cardio['fipslen'] = cardio['County Code'].astype(str).map(len)
cardio.loc[cardio['fipslen'] == 4, 'FIPS'] = '0' + cardio['County Code'].astype(str) 
cardio.loc[cardio['fipslen'] != 4, 'FIPS'] = cardio['County Code'].astype(str) 
cardio['dth_rate'] = pd.to_numeric(cardio['Rate'],errors='coerce')
cardio['dth_rate'] = cardio['dth_rate'].astype(float)
cardio.head()


# In the next steps I access a table on Wikipedia that ultimately provides me with the FIPS (County) code and it's centroid lattitude and longitude so that I can pass those parameters into Foursquare.

# In[3]:


url = 'https://en.wikipedia.org/wiki/User:Michael_J/County_table'
html = urlopen(url) 
soup = BeautifulSoup(html, 'html.parser')
table = soup.find('table',{'class':'wikitable sortable'})


# In[4]:


location_data = pd.DataFrame(columns=["fips", "state", "county", "latitude", "longitude"])

for row in table.tbody.find_all("tr"):
    col = row.find_all("td")
    if (col != []):
        fips = col[2].text
        state = col[1].text
        county = col[3].text
        latitude = col[12].text.strip()
        longitude = col[13].text.strip()
        location_data = location_data.append({"fips":fips, "state":state, "county":county, "latitude":latitude, "longitude":longitude}, ignore_index=True)
        
location_data['latitude']=location_data['latitude'].replace('\u00b0','', regex=True)
location_data['longitude']=location_data['longitude'].replace('\u00b0','', regex=True)

location_data.head()


# In the cell below I clean and adjust the data from the Wikipedia page so that it reads in the correct format to pass to Foursquare.

# In[5]:


location_data['latsgn'] = location_data['latitude'].str.slice(0, 1)
location_data['lngsgn'] = location_data['longitude'].str.slice(0, 1)

location_data['lat'] = location_data['latitude'].str.slice(1, 9)
location_data['lat'] = pd.to_numeric(location_data['lat'],errors='coerce')
location_data['lat'] = location_data['lat'].astype(float)

location_data['long'] = location_data['longitude'].str.slice(1, 9)
location_data['long'] = pd.to_numeric(location_data['long'],errors='coerce')
location_data['long'] = location_data['long'].astype(float)

location_data.loc[location_data['latsgn'] == "+", 'lat'] = location_data['lat'] 
location_data.loc[location_data['latsgn'] != "+", 'lat'] = -1*location_data['lat'] 
location_data.loc[location_data['lngsgn'] == "+", 'long'] = location_data['long'] 
location_data.loc[location_data['lngsgn'] != "+", 'long'] = -1*location_data['long'] 

location_data.head()


# The cell below sets up my input parameters needed to access the Foursquare API.  I read in my Client ID, Secret and Version number.  Ulitmately my analysis compares the concept of at convenience resturaunts to the prevalence of heart disease death so I pass the Category IDs required to pull convenience food categories.

# In[6]:


CLIENT_ID = 'XXXXXXXXXXXXXXXXX' # your Foursquare ID
CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXX' # your Foursquare Secret
VERSION = '20180605' # Foursquare API version
CATEGORY_ID = '4bf58dd8d48988d120951735,4bf58dd8d48988d147941735,4bf58dd8d48988d148941735,4bf58dd8d48988d16c941735,4bf58dd8d48988d16e941735,4bf58dd8d48988d16f941735,4bf58dd8d48988d179941735,4bf58dd8d48988d1ca941735,4bf58dd8d48988d1cb941735,4d4ae6fc7a7b7dea34424761,56aa371be4b08b9a8d57350b' #Gun Shop Category

print('Your credentails:')
print('CLIENT_ID: ' + CLIENT_ID)
print('CLIENT_SECRET:' + CLIENT_SECRET)
print('CATEGORY_ID:' + CATEGORY_ID)


# The following cell pulls a subset of data to test the Foursquare approach.  Ultimately it is not used in the final analysis, but was used to test until I was sure the process would work.

# In[7]:


from pandasql import sqldf
cnty = sqldf('select * from location_data limit 5')
cnty


# The following cell loops through Foursquare for each County Centroid pulling back all convenience resturaunts within 20 miles of the county centroid.  My assumpiton is that that distance is consistent with the notion of convenience.  I pull back the venue, location, distance and name in case I wanted to adjust the results after the API call based on these parameters.  I set the limit to 200 but I'm fairly certain Foursquare defaults it to 100.  I create a dataframe called ff_venues with the output.

# In[ ]:


def getNearbyVenues(names, latitudes, longitudes, radius=32186.9, limit=200):
    
    venues_list=[]
    for name, lat, lng in zip(names, latitudes, longitudes):
        #print(name)
            
        # create the API request URL
        url = 'https://api.foursquare.com/v2/venues/explore?&categoryId={}&client_id={}&client_secret={}&v={}&ll={},{}&radius={}&limit={}'.format(
            CATEGORY_ID,
            CLIENT_ID, 
            CLIENT_SECRET, 
            VERSION, 
            lat, 
            lng, 
            radius,
            limit)
            
        # make the GET request
        results = requests.get(url).json()["response"]['groups'][0]['items']
        
        # return only relevant information for each nearby venue
        venues_list.append([(
            name,
            lat, 
            lng,
            v['venue']['id'],
            v['venue']['name'],
            v['venue']['location']['lat'], 
            v['venue']['location']['lng'],
            v['venue']['location']['distance'],
            v['venue']['categories'][0]['name']) for v in results])

    nearby_venues = pd.DataFrame([item for venue_list in venues_list for item in venue_list])
    nearby_venues.columns = ['fips', 
                  'fips lat',
                  'fips long',
                  'venue_id', 
                  'venue_name', 
                  'venue_lat', 
                  'venue_long',
                  'venue_distance',           
                  'venue_category']
    
    return(nearby_venues)

ff_venues = getNearbyVenues(names=location_data['fips'],
                                   latitudes=location_data['lat'],
                                   longitudes=location_data['long']
                                  )
print('Foursquare Data Acquisition Complete')


# The following cells enable me to export to excel so that I don't have to loop through foursquare in the future and gives me a way to explore the data a bit and ensure that what I intend to bring down actually did.

# In[6]:


from xlsxwriter import Workbook
writer = pd.ExcelWriter(r'C:\Users\jeffp\Documents\Desktop\Personal\Learning\Coursrera\Assignment\final assignment\ff_venues.xlsx', engine='xlsxwriter')
ff_venues.to_excel(writer, index = False, header=True)
writer.save()


# In[10]:


ff_venues = pd.read_excel(r'C:\Users\jeffp\Documents\Personal\Learning\Coursrera\Assignment\final assignment\ff_venues.xlsx')
ff_venues.head(10)


# In[11]:


ff_venues['venue_name'] = ff_venues['venue_name'].str.upper()
ff_venues['venue_category'] = ff_venues['venue_category'].str.upper()
categories = sqldf('select venue_category as "Venue Category", count(*) as Count from ff_venues group by venue_category order by count desc')
categories.head(10).style.format({"Count": "{:,.0f}"})           


# In[12]:


venname = sqldf('select venue_name as "Venue Name", count(*) as Count from ff_venues group by venue_name order by count desc')
venname.head(10).style.format({"Count": "{:,.0f}"})


# Ultimately what I want to compare is the number of convenience eating venues with heart disease deaths so I count the number of convenience food venues by FIPS code.  This will allow me to join to heart disease death table and compare the two measures.  I still more comfortable with SQL so I use pandas SQL to accomplisth this task.

# In[16]:


ffsum = sqldf('select fips, count(distinct venue_id) as ffcount from ff_venues group by fips')
ffsum.head()


# In the following cell I join the convenience venue counts to the original heart disease table and using a left join from the heart disease table.  I ensure there are no null values in the venue column by filling with a zero.  I also removed 2 FIPS that were reorganized in the 2010s.

# In[17]:


venue = sqldf('select a.*, b.ffcount from cardio a left join ffsum b on a.fips=b.fips where a.fips not in ("51515","46102") order by Rate desc')
venue['ffcount'] = venue['ffcount'].fillna(0)
venue.head(10)


# I joined the counts above but I have to adjust the counts for the population in the county.  The following cell does that.  As I explored the results of the analysis, the data was quite variable so I grouped the venue rates into deciles to better manage the variation.

# In[18]:


#venue['ff_rate']  = (venue['ffcount']/venue['Population']*100000).round(2)
venue['ff_rate']  = (venue['ffcount']/(venue['Population']/10)*100000).round(2)
venue['ff_bin'] = pd.qcut(venue['ff_rate'], 20, labels = False)
venue['ff_bin']  = venue['ff_bin']+1
venue.head(10)


# In[19]:


max_value = venue['ff_rate'].max()
min_value = venue['ff_rate'].min()
print("max density:") 
print(max_value)
print("Min density:")
print(min_value)


# Below I use scatter plot to evaluate the data in raw form.

# In[20]:


get_ipython().run_line_magic('matplotlib', 'inline')
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
import scipy as sp

sns.lmplot(x="ff_rate", y="dth_rate", data=venue, ci=None, scatter_kws={'s': 50, 'color': '#FF0000'}, line_kws={'lw': 2, 'color': '#4682b4'})
plt.ylim(150, 700)
plt.xlim(0, 3500)
plt.title('Heart Disease Death Rate vs Convenience Food Density \n')
plt.xlabel('\n CFV Density')
plt.ylabel('Hrt Dis Death Rate/100K \n')
plt.show()


# In[21]:


outlier = sqldf('select * from venue where ff_rate > 3000')
outlier


# In[17]:


outlier_ven = sqldf('select * from ff_venues where fips = "21201"')
pd.set_option('display.max_rows', 10)
outlier_ven


# After reviewing the raw data, below I use the dataframe above and recalculate both the heart disease death rate and convenience resturaunt rate by the 20 ranked bins.  This allowed me to utilize and visualize the results in an easier fashion.

# In[22]:


decilesum = sqldf('select ff_bin, sum(Deaths) as deaths, sum(Deaths)/10 as avg_deaths, sum(Population) as population, sum(Population)/10 as avg_pop, sum(ffcount) as ffcount from venue group by ff_bin')
decilesum['dth_rate'] = decilesum['deaths']/decilesum['population']*100000
decilesum['ff_rate']  = decilesum['ffcount']/(decilesum['population']/10)*100000

decileview = decilesum.copy()
decileview.drop(['population', 'deaths'], axis = 1, inplace=True)
decileview.columns = ['Bin','Avg Deaths','Avg Population','Venues Count','Hrt Dis Death Rate/100K','CFV Density/100K']
decileview.head(20).style.format({"Bin": "{:,.0f}", "Avg Deaths": "{:,.0f}", "Avg Population": "{:,.0f}", "Venues Count": "{:,.0f}", "Hrt Dis Death Rate/100K": "{:,.1f}", "CFV Density/100K": "{:,.1f}"})


# In the following cells I explore the data for sharing

# In[23]:


venue['Avg_Population'] = venue['Population']/10
venue['Avg_Deaths'] = venue['Deaths']/10
decile10 = sqldf('select state, county, Avg_Population, Avg_Deaths, dth_rate, ff_rate from venue where ff_bin == 20 and Avg_Population > 1000 order by dth_rate desc')
decile10.columns = ['State','County','Avg Population','Avg HrtDis Deaths','HD Death Rate/100K','CFV Density/100K']
print('Top 10 Counties in Top Convenience Food Venues Decile by Heart Disease Death Rate (Avg Population > 1,000)')
decile10.head(10).style.format({"Avg Population": "{:,.0f}", "Avg HrtDis Deaths": "{:,.0f}", "HD Death Rate/100K": "{:,.1f}", "CFV Density/100K": "{:,.1f}"})


# In[24]:


decile1 = sqldf('select state, county, Avg_Population, Avg_Deaths, dth_rate, ff_rate from venue where ff_bin == 1 and Avg_Population >= 1000 order by dth_rate')
decile1.columns = ['State','County','Avg Population','Avg HrtDis Deaths','HD Death Rate/100K','CFV Density/100K']
print('Top 10 Counties in Bottom Convenience Food Venues Decile by Heart Disease Death Rate (Avg Population > 1,000)')
decile1.head(10).style.format({"Avg Population": "{:,.0f}", "Avg HrtDis Deaths": "{:,.0f}", "HD Death Rate/100K": "{:,.1f}", "CFV Density/100K": "{:,.1f}"})


# After iterating through this process several times it was clear that when plotting this data that these metrics were clearly related and related in a linear distribution.  I suspect the top supression is a function of the 100 venue limit from Foursquare.  I map the data and run a line through it using seaborn.

# In[25]:


get_ipython().run_line_magic('matplotlib', 'inline')
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
import scipy as sp

sns.lmplot(x="ff_bin", y="dth_rate", data=decilesum, order=2, ci=None, scatter_kws={'s': 50, 'color': '#FF0000'}, line_kws={'lw': 2, 'color': '#4682b4'})
plt.ylim(150, 300)
plt.xlim(0, 25)
plt.title('Heart Disease Death Rate vs Convenience Food Density \n', fontsize=18)
plt.xlabel(' \n CFV Density Bin', fontsize=14)
plt.ylabel('Heart Disease Death Rate/100K \n', fontsize=14)
plt.show()


# In addition to showing the relationship via the scatterplot I generate a pearson coorelation below, and as you can see there is a relatively strong coorelation between deaths from heart disease and the number of convenience resturaunts within 20 miles of the county centroid.

# In[26]:


from scipy import stats
stats.pearsonr(decilesum['ff_rate'], decilesum['dth_rate'])


# To further demonstrate the relationship I map both the convenience food density and heart disease death rates by bins of 20 to depict the geographic alignment.

# In[27]:


fips = venue['FIPS'].to_list()
cfvd = venue['ff_bin'].to_list()


# In[28]:


import geopandas
import plotly.figure_factory as ff
from plotly.figure_factory._county_choropleth import create_choropleth
import shapefile
import plotly
import numpy as np

#binning_endpoints = list(np.mgrid[min(cfvd):max(cfvd):9j])
colorscale = ['#00FF00','#22FF00','#33FF00','#44FF00','#55FF00',
              '#88FF00','#99FF00','#AAFF00','#CCFF00','#EEFF00',
              '#FFFF00','#FFDD00','#FFCC00','#FFAA00','#FF8800',
              '#FF7700','#FF6600','#FF3300','#FF2200','#FF1100']

fig = ff.create_choropleth(fips=fips, 
                           values=cfvd,
                           scope=['usa'],
                           colorscale=colorscale,
                           title='Convenience Food Venue Density',
                           legend_title='CFV Density Bin')
fig.layout.template = None
fig.show()


# In[26]:


hddec = venue.copy()
hddec['hd_bin'] = pd.qcut(hddec['dth_rate'], 20, labels = False)
hddec['hd_bin']  = hddec['hd_bin']+1
fips = hddec['FIPS'].to_list()
hd = hddec['hd_bin'].to_list()


# In[27]:


import geopandas
import plotly.figure_factory as ff
from plotly.figure_factory._county_choropleth import create_choropleth
import shapefile
import plotly
import numpy as np

#binning_endpoints = list(np.mgrid[min(hd):max(hd):9j])
colorscale = ['#00FF00','#22FF00','#33FF00','#44FF00','#55FF00',
              '#88FF00','#99FF00','#AAFF00','#CCFF00','#EEFF00',
              '#FFFF00','#FFDD00','#FFCC00','#FFAA00','#FF8800',
              '#FF7700','#FF6600','#FF3300','#FF2200','#FF1100']

fig = ff.create_choropleth(fips=fips, 
                           values=hd,
                           scope=['usa'],
                           colorscale=colorscale,
                           title='Heart Disease Deaths per 100,000 People',
                           legend_title='Heart Disease Death Rate Bin')
fig.layout.template = None
fig.show()


# In[ ]:




