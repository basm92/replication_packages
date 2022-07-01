## Replication Package for Chapter 2: "The Wealth of the Dutch Political Elite (1870-1922)"

- What is in this replication package?
  - The Code used to create the Figures (output in folder `/Figures/`) and Tables (output in folder `/Tables/`) in the text
    - The Code contains several files. Below is a description: 
      - Code files starting with `Tab` and `Fig` contain the Code used to create the correspondingly numbered Table or Graph in the text. 
      - All other files in the code are auxilary files used to create the necessary data for the graphs. 


  - The Data on the basis of which these Figures and Tables were created. 
    - The Data contains several files. Below is a description:
    	- `AnalysisFile.xlsx` - Raw file containing the Probate Inventories Data.
    	- `ek_1815tot1950_uu.xlsx`, `tk_1815tot1950_uu.xlsx`, `bewindslieden_1815tot1950_uu.xlsx` - Data about Upper House, Lower House members and Provincial Executives: demographics and career.
    	- `comp_with_pop_x.csv` - Summary statistics about wealth relative to the general population. Each $x$ corresponds to a different representative body.
    	- `election_dates.csv` - Dates (Calendar time) when new parliaments were instigated.
    	- `gov_orientation.csv` - Orientation of successive Dutch governments.
    	- `kabinetten.csv` - Name and period of governance of successive Dutch governments.
    	- `key_politicalparty_category.csv` - Mapping from heterogeneous party classification to $\{ \text{Confessional, Liberal, Socialist} \}$.
    	- `lh_parliaments.csv`, `uh_parliaments.csv` - Lists of names of MPs (Lower House and Upper House respectively) and periods of activity.
    	- `lowerhouse.csv`, `lowerhouse_clean.csv`, `upperhouse.csv`, `upperhouse_clean.csv` - Raw and cleaned lists of names of MPs. 
    	- `Wealth Tax (in Numbers) 20200606.xlsx` - Data about wealth distribution in the population from De Vicq et al. (2020).
