"""
Plot additional charts for MMPE502
trimester 2, 2019
Note: users must obtain an API key in order to download data from FRED.
Below, XXXXX replace my api key, you need to insert yours.
"""

import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib as mpl
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)
#plt.style.use('ggplot')
#plt.style.use('seaborn-white')
plt.style.use('seaborn-ticks')
#plt.style.use('presentation')
plt.style.reload_library()
import numpy as np
import pandas as pd
#from scipy import linalg, optimize
from datetime import datetime, date, timedelta
from fredapi import Fred
fred = Fred(api_key='XXXXX')

print(plt.style.available)

# GDP, nominal versus real
GDPseries={}
GDPseries['US Nominal GDP']=fred.get_series('GDP')
GDPseries['US Real GDP, chained 2009 dollars']=fred.get_series('GDPC1')
GDPseries['NZ Real GDP, constant price']=fred.get_series('NAEXKP01NZQ189S')
GDPseries['NZ Nominal GDP']=fred.get_series('NZLGDPNQDSMEI')

#A191RO1Q156NBEA is growth rate
gdpLevel = pd.DataFrame(GDPseries)
gdpUSA = gdpLevel['1960-01-01':'2018-10-01']

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(9,5), sharey='all')
ax2.plot(gdpUSA["US Nominal GDP"], color='navy', linestyle='--', linewidth=1.2, label='U.S. Nominal GDP')
ax2.plot(gdpUSA["US Real GDP, chained 2009 dollars"], color='red', linestyle='-',linewidth=1.2, label='US Real GDP, chained (2009) dollars')
plt.autoscale(enable=True, axis='x', tight=True)
ax2.set_title('GDP, the United States')
ax2.legend(loc='upper left')
ax2.set_ylabel('Biilions of U.S. dollars')
ax2.set_xlim([gdpUSA['US Nominal GDP'].index[0],gdpUSA['US Nominal GDP'].index[-1]])
fig.tight_layout()
plt.savefig('USgdp.png',dpi=72, format='png')
plt.show()

# ---New Zealand GDP
gdpNZ = gdpLevel['1988-01-01':'2018-10-01']
gdpNZreal = gdpNZ['NZ Real GDP, constant price']* 0.000001*0.001
gdpNZcurrent = gdpNZ['NZ Nominal GDP']* 0.000001*0.001

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(9,5), sharey='all')
ax2.plot(gdpNZreal, linewidth=1.2, linestyle='-', label='NZ Real GDP, constant price')
ax2.plot(gdpNZcurrent, linestyle='--', linewidth=1.2, label='NZ Nominal GDP')
plt.autoscale(enable=True, axis='x', tight=True)
ax2.set_title('GDP, New Zealand')
ax2.legend(loc='upper left')
ax2.set_ylabel('Biilions of NZ dollars')
ax2.set_xlim([gdpNZ['NZ Nominal GDP'].index[0], gdpNZ['NZ Nominal GDP'].index[-1]])
fig.tight_layout()
plt.savefig('NZgdp.png',dpi=72, format='png')
plt.show()

##-----Unemployment rate
unempSeries={}
unempSeries['USunempRate']=fred.get_series('LRUNTTTTUSQ156S')
unempSeries['NZunempRate']=fred.get_series('LRUNTTTTNZQ156S')

unempRates = pd.DataFrame(unempSeries)
unempRates = unempRates['1980-01-01':'2019-01-01']

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(9,5), sharey='all')
ax2.plot(unempRates['USunempRate'],color="green", linewidth=1.0, linestyle="--",label='Unemployment rate, U.S.')
ax2.plot(unempRates["NZunempRate"], color="navy", linewidth=1.0, linestyle="-",label='Unemployment rate, NZ')
plt.autoscale(enable=True, axis='x', tight=True)
ax2.set_title('Unemployment Rates, monthly')
ax2.legend(loc='upper right')
ax2.set_ylabel('Percent')
ax2.set_xlim(unempRates['USunempRate'].index[0],unempRates['USunempRate'].index[-1])
plt.xlabel("Date")
fig.tight_layout()
plt.savefig('unemploymentUSNZ.png')
plt.show()

# unemployment rate, New Zeland
unempAnnual = unempRates.resample('A').mean()
unempNZannual = unempAnnual[['NZunempRate']]

unempNZannual2 = unempNZannual ['1987-12-31':'2018-12-31']
width = 0.45
height2 = 1.0
y_pos = np.array(unempNZannual2.index.year)
unemp = np.array(unempNZannual2['NZunempRate'])

fig, ax = plt.subplots()
ax.bar(y_pos, unemp, width = width, color='green', align='center')
ax.set_ylabel('Unemp. Rate, NZ')
fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(9,5), sharey='all')
ax2.bar(y_pos, unemp, width = width, color='green', align='center')
plt.autoscale(enable=True, axis='x', tight=True)
ax2.set_ylabel('Unemp. Rate, NZ, annual Avg')
plt.xlabel("Year")
fig.tight_layout()
plt.savefig('unempNZannual.png')
