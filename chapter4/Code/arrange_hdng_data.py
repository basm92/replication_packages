#### Find HDNG Data and restructure it so that it be useful
### Also combine this with a key between different municipality names

## Import libraries
import pandas as pd
import urllib.request
import os

## First, download the data and read it into python's memory
url=" https://datasets.iisg.amsterdam/api/access/datafile/10264"
urllib.request.urlretrieve(url, './Data/hdng.txt')

variablenames = []
variableinformation = []

hdng = pd.read_csv("./Data/hdng.txt",  
  dtype={'amco':'Int32', 'naam':str, 'variable':str, 'description':str,'information':str, 'year':'Int32','values':str},
  delimiter = ",", 
  usecols=['amco','name', 'variable', 'description','information','year','value'])
  
hdng = hdng[(hdng['year'] < 1940) & (~pd.isna(hdng['amco']))]

#os.remove("./Data/hdng.txt")

## Now, clean the data

