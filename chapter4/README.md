## Replication Package for Chapter 4: "Returns to Politics Under A Changing Political System"

- What is in this replication package?

  ### Code

  - The Code used to create the Figures (output in folder `/figures_main/` and `figures/appendix`) and Tables (output in folder `/tables_main/` and `/tables_appendix/`) in the text
  - In the folder `/Code`, there are various notebooks illustrating how I proceeded from raw data to a dataset. This is extensive and not particularly accessible code. It is better to investigate the end product, which I detail under the subheader Data. 
    - In the subfolder `/Analysis/code_for_tables`, the actual analyses are effectuated. 
    - In the folder parent folder `/Analysis/`, all the other files are auxiliary files that help construct the dataset from raw data and to calculate some of the estimators that I have implemented, including the one from Cellini et al. 
  - I still have to synthesize the names of the table with the numeration in the dissertation. 

  ### Data

  - There are various datasets, themselves the product of aggregations of raw data, in the folder `/Data/analysis/`. The dataset used on the basis of which the results are obtained is called `full_sample_analysis_allvars.csv`. This dataset is itself being cleaned in the file `/Code/Analysis/new_data_analysis.R`. This file creates various R objects on the basis of which the analyses are conducted. These datasets are saved as .RDS files as:
    - 1
    - 2
    - 3
    - 4
    - 5
    - 6 (Still do this - clean these datasets a little bit by putting them in the right order)
  - And can be easily converted to e.g. .csv, Pickle, or anything else. 

