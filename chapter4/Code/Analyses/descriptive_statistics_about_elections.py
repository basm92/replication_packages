import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import itertools
import geopandas as gpd
import difflib
import mapclassify

# Read the data sources
all_margins = pd.read_csv("./Data/elections/allmargins.csv").iloc[:,2:]
all_elected = all_margins[all_margins['margin']> 0 ].Naam.unique()
convert_muni_distr = pd.read_csv("./Data/district_data/Municipalities_and_districts.csv").iloc[:,1:].melt(
  id_vars=['gemeente','district'], 
  value_vars=['1848','1850','1888', '1897'])


# Set parameter margin:
closemargin = 0.2

## Close elections in general

### How many close elections? 
all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District', 'Verkiezingdatum']).size().reset_index().iloc[:,0:2].count()

### Per type
all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District', 'Verkiezingdatum','Type']).size().reset_index().iloc[:,0:3]

### Where are the close elections, in which districts?
districts_ce = all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District']).Verkiezingdatum.nunique().reset_index()

### In what years are the close elections? 
all_margins['election_year'] = all_margins['Verkiezingdatum'].str.split("/").apply(lambda x: x[2])
years_withpols = all_margins[abs(all_margins['margin']) < closemargin].groupby(['election_year']).District.nunique()

### In what district-years?
all_margins[abs(all_margins['margin']) < closemargin].groupby(['District','election_year']).Verkiezingdatum.nunique().reset_index()


## Close elections with non-politicians

#### Filter dataframe to negative margins (losers)
margins_without_politicians = all_margins[(abs(all_margins['margin']) < closemargin) & (all_margins['margin'] < 0)]
list_elected = all_elected

#### Check whether each of the names in list_elected matches a given name in margins_without_politicians, and filter
losers = margins_without_politicians[margins_without_politicians.apply(lambda row: all(a not in row['Naam'] for a in list_elected), axis = 1)]

### How many close electıons wıth only losers?
losers.groupby(['District', 'Verkiezingdatum']).size().reset_index().iloc[:,0:2].count()

### Per Type?
losers.groupby(['District', 'Verkiezingdatum','Type']).size().reset_index().iloc[:,0:3]

### Where are the close elections, in which districts?
districts_ce_withoutpol = losers.groupby(['District']).Verkiezingdatum.nunique().reset_index()

### In which years do close elections take place?
years_nopols = losers.groupby(['election_year']).District.nunique()

### Create a figure: Close elections - two district heatmaps

### One cumulative timemap 
#### Data for first subplot
years_nopols = years_nopols.reset_index()
years_nopols['District'] = years_nopols.District.cumsum()

years_withpols = years_withpols.reset_index()
years_withpols['District'] = years_withpols.District.cumsum()

data = pd.merge(years_nopols, years_withpols, how = "outer", left_on="election_year", right_on="election_year", sort = True)
data = data.rename(columns={'election_year':'Year', 'District_x':'Without Politicians', 'District_y':'With Politicians'})
data['Year'] = data['Year'].apply(lambda x: pd.Timestamp(x))
data.fillna(method='backfill', inplace=True)

#### Data for second subplot: new candidates per year
all_margins['election_year'] = all_margins['election_year'].apply(lambda x: pd.to_numeric(x))

newcandidates = pd.DataFrame({'election_year':range(1848, 1918), 'new_candidates':None})

out = []
old = []

for i in range(1848, 1918):
  new = []
  for j in all_margins[all_margins['election_year'] == i].Naam.unique():
    if j not in old:
      new.append(j)
    old.append(j)
  out.append(len(new))
newcandidates['new_candidates'] = out
newcandidates['cumulative_new_candidates'] = newcandidates['new_candidates'].cumsum()

del(out); del(new); del(old)

plt.figure()
plt.subplot(1,2,1)

#### First subplot
plt.plot(data['Year'], data['Without Politicians'], color = 'blue', label = 'Without Future Politicians')
plt.plot(data['Year'], data['With Politicians'], color = 'orange', label = 'With Future Politicians')
plt.xlabel("Year")
plt.ylabel("Politicians Participated in Close Elections")
plt.legend(loc='upper left')
plt.title("Cumulative Close Elections (20% Margin) \n With and Without Future Politicians")

plt.subplot(1,2,2)
#### Second subplot
plt.plot(newcandidates['election_year'], newcandidates['cumulative_new_candidates'], color = 'red')
plt.xlabel("Year")
plt.ylabel("Cumulative New Candidates")
plt.title("Cumulative New Candidates over Time")

plt.show()
plt.savefig("./Data/shapefiles/close_elections_over_time.pdf")
plt.clf()

## Two district heatmaps in one figure - where are the close elections?
### ### Merge attempt 1: Only use the first name of municipalities to match gemeentes:
nl_map = gpd.read_file("./Data/shapefiles/nl_1888.shp")
gemeentes_1888 = convert_muni_distr[(convert_muni_distr['variable'] == '1888') & (convert_muni_distr['value'] == 1)]
nl_map['GM_NAAM'] = nl_map['GM_NAAM'].str.replace(",(.+)", "", regex=True).str.replace(" en(.+)", "", regex=True)
nl_map = gpd.GeoDataFrame(pd.merge(nl_map, gemeentes_1888.iloc[:,0:2], how='left', left_on = 'GM_NAAM', right_on='gemeente'))

heatmap = nl_map.dissolve(by='district').reset_index()
heatmap.plot()
plt.show()
plt.clf()

### Merge attempt 2: merge the shapefile (with fuzzy merge) nl_map with the gemeentes_1888 and merge the boundaries
nl_map = gpd.read_file("./Data/shapefiles/nl_1888.shp")
nl_map['GM_NAAM'] = nl_map['GM_NAAM'].str.replace(",(.+)", "", regex=True).str.replace(" en(.+)", "", regex=True)
nl_map['merge_candidate'] = nl_map['GM_NAAM'].apply(lambda x: difflib.get_close_matches(x, gemeentes_1888['gemeente'])).apply(lambda x: x[0] if len(x) else None)
nl_map = gpd.GeoDataFrame(pd.merge(nl_map, gemeentes_1888.iloc[:,0:2], how='left', left_on = 'merge_candidate', right_on='gemeente'))

heatmap = nl_map.dissolve(by='district').reset_index()
heatmap.plot()
plt.show()
plt.clf()

### Now, we can continue with either one of these heatmaps. We get the matches for the districts
districts_ce['merge_candidate'] = districts_ce['District'].apply(lambda x: difflib.get_close_matches(x, heatmap['district'])).apply(lambda x: x[0] if len(x) else 0)

final1 = gpd.GeoDataFrame(pd.merge(heatmap, districts_ce, how='left', left_on='district', right_on='merge_candidate'))
final1['Verkiezingdatum'] = final1['Verkiezingdatum'].astype('Int64')

districts_ce_withoutpol['merge_candidate'] = districts_ce_withoutpol['District'].apply(lambda x: difflib.get_close_matches(x, heatmap['district'])).apply(lambda x: x[0] if len(x) else 0)

final2 = gpd.GeoDataFrame(pd.merge(heatmap, districts_ce_withoutpol, how='left', left_on='district', right_on='merge_candidate'))
final2['Verkiezingdatum'] = final2['Verkiezingdatum'].astype('Int64')

### Finally, create the plot

fig, (ax1, ax2) = plt.subplots(figsize=(15,20), ncols=2, sharex=True, sharey=True)
final1.plot(column='Verkiezingdatum', 
  cmap='OrRd', 
  scheme='quantiles', 
  missing_kwds={'color': 'lightgrey'},
  legend = True,
  ax = ax1)

ax1.set_title("Amount of Close Elections in Each District, 1848-1917 \n Including Future Politicians")

leg = ax1.get_legend()
leg.set_bbox_to_anchor((0.37,1)) 

final2.plot(column='Verkiezingdatum', 
  cmap='OrRd', 
  scheme='quantiles', 
  missing_kwds={'color': 'lightgrey'},
  legend = True,
  ax = ax2) 
  
ax2.set_title("Amount of Close Elections in Each District, 1848-1917 \n Excluding Future Politicians")
  
leg = ax2.get_legend()
leg.set_bbox_to_anchor((0.35,1)) 

# Adjust axis
frame = plt.gca()
frame.axes.get_xaxis().set_ticks([])
frame.axes.get_yaxis().set_ticks([])
  
plt.show()

plt.savefig("./Data/shapefiles/close_elections_geographic.pdf")
