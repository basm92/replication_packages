import pandas as pd
from tqdm import tqdm

def get_margin(dataframe):
    
    out = pd.DataFrame()
    
    for i in tqdm(range(len(dataframe))):
        
        try:
        
            distr = dataframe.iloc[i]['District']
            date = dataframe.iloc[i]['Verkiezingdatum']
        
            if dataframe.iloc[i]['gewonnen'] == 0:
            
                votes_marginal_winner = dataframe[(dataframe['District'] == distr) & 
                  (dataframe['Verkiezingdatum'] == date) &
                  (dataframe['marginal_winner'] == 1)]['Aantal stemmen'].values[0]
            
                margin = (dataframe.iloc[i]['Aantal stemmen'] - votes_marginal_winner)/dataframe.iloc[i]['totaal aantal stemmen']
        
            if dataframe.iloc[i]['gewonnen'] == 1:
        
                votes_marginal_loser = dataframe[(dataframe['District'] == distr) & 
                  (dataframe['Verkiezingdatum'] == date) &
                  (dataframe['marginal_loser'] == 1)]['Aantal stemmen'].values[0]
            
                margin = (dataframe.iloc[i]['Aantal stemmen'] - votes_marginal_loser)/dataframe.iloc[i]['totaal aantal stemmen']
    
        except:
            
            margin = None
            
        interim = dataframe.iloc[i:i+1]
        interim = interim.assign(margin = margin)
            
        out = out.append(interim)
        
    return out
    
def get_match(df_ep):
    
    matches = pd.DataFrame()
    
    #loop through each unique candidates
    for i in tqdm(df_ep['naam'].unique()):
        
        try: 
            
            interim = pd.DataFrame()

            distr_name = df_ep[df_ep['naam'] == i].iloc[0]['districtsnaam']
            verk_dat = df_ep[df_ep['naam'] == i].iloc[0]['verkiezingdatum']

            closest_verk_dat = all_candidates[(all_candidates['District'] == distr_name) & (all_candidates['Verkiezingdatum'] <= verk_dat)]['Verkiezingdatum'].max()
            candidate_matches = all_candidates[(all_candidates['District'] == distr_name) & (all_candidates['Verkiezingdatum'] == closest_verk_dat)]["Naam"].tolist()

            found_match = process.extractOne(i, candidate_matches)[0]

                # find name in all_candidates on basis of winning election
            interim = interim.assign(name_in_elected_people = [i],
                                     name_in_all_elections = [found_match])

            matches = matches.append(interim)

        except:
            next
            
    return(matches)
    

