## Replication Package for Chapter 3: "Democratization, Personal Wealth of Politicians and Voting Behavior"

- What is in this replication package?
  - The Code used to create the Figures (output in folder `/figures/`) and Tables (output in folder `/tables/`) in the text
    - The Code contains several files. Below is a description: 
      - Code files starting with `Tab` and `Fig` contain the Code used to create the correspondingly numbered Table or Graph in the text. 
      - The Code for the Figures is in the folder `/chapter3/code/code_for_figures/`. 
      - `code/analysis/results/new_descr_stats.R` contains the code for Tables 3.2 and 3.3. This is also indicated by the notes inside the R-file. 
      - `code/analysis/results/new_results.R` contains the code for most of the tables starting from the data. Inside the file, I have indicate the number of the corresponding table in the text. 
      - Various other robustness checks present in the Chapter are contained in `code/analysis/results/robustness_checks.R`. I similarly indicated the correspondence between the table and the text by means of notes inside the code file. 
      - All other files in the code folder are auxilary files and scripts used to create the necessary data for the graphs and tables. 


  - The Data on the basis of which these Figures and Tables were created. 
    - The Data contains several files. 
    - The most important folder, on the basis of which the results are obtained are in the folder `chapter3/data/datasets`. 
    - Below is a rough description of these files:
      - `chapter3/data/datasets/fiscal_lowerandupper.RDS` - An .RDS file (to be loaded into R, also easily converted to .csv or .xlsx through `readr::write_csv`) containing voting behavior on fiscal legislation for the Lower and Upper Houses, and all of the variables used in the analysis. Also, the law is indicated, the time of voting, the house (upper or lower) and all control variables. 
      - `chapter3/data/datasets/fiscal_ivdata.RDS` - A similar .RDS file, containing now only Lower House observations, including the instruments used in the analysis: several profdummies and deflated parental wealth. 
      - `chapter3/data/datasets/electoral_lower.RDS` - An .RDS file for the Lower House voting outcomes on votes regarding Suffrage extension. Contains the same information as the `chapter3/data/datasets/fiscal_lowerandupper.RDS` file. 
      - `chapter3/data/datasets/social_lower.RDS` - Contains the same data as the preceding file for the Lower House voting outcomes, but for Government Intervention laws. 
      - On the basis of these files, the analyses in`chapter3/code/analysis/new_results.R` and `chapter3/code/analysis/robustness_checks.R` are conducted. Sometimes, there are some slight modifications, which will be made clear in those files. 
    - All the other files in the `/data/`folder contain raw data on the basis of which these four central datasets have been constructed, e.g. electoral data, wealth data, voting behavior data, municipality data and asset return data from Jorda et al. (2019). 
