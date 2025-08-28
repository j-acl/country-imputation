This project takes a 'birthdata' file including missing records, uses external sources and internal logic to impute missing values, demonstrates the process in an interactive file, and summarizes the work in a report.

The knitted report and interface are included, and can be opened and read as they are to understand the results.

TO BUILD THE PROJECT:
- TODO creates the dashboard of imputation results
- 'report.rmd' creates the summary report
- for convenience, a preprocessed rds of all external data used is included
- additionally, helper 'fuzzy picks' and 'ostgebiete' files are pre-compiled

TO BUILD THE PROJECT FROM SCRATCH:
- download the raw data files from the links given and run preprocess_data on the alternate kwargs
- run the basic code without fuzzy matching
- manually mark 'fuzzy matches' from the produced 'fuzzy candidates' file
