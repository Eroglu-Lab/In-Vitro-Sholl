# Sholl Analysis in R

R analysis code for analyzing fluroescence microscopy images to determine the complexity of cultured astrocytes. 

Required programs:
1. ImageJ/FIJI https://imagej.net/software/fiji/ We have tested the Sholl analysis with ImageJ v1.53c and Java 1.8.0_172
2. R and R Studio: https://posit.co/download/rstudio-desktop/. Note this script was tested with R 4.0.0 and R studio 2022.07.1 Build 554. Updates to R may deprecate parts of the code. Please report an issue if so.

Required packages for ImageJ/FIJI:
Sholl Analysis 
Note: ImageJ/FIJI updates to the Sholl Analysis function may change in different versions. A workaround is to install the older version of the Sholl Analysis plugin by putting the jar file from this repo into your plugins folder within FIJI.

Required packages for R:
ggplot2 [v3.4.0], 
reshape [v0.8.8], 
car [v3.0-8]
nlme [v3.1-148]
multcomp [v1.4-13]
rstudioapi [v0.11]

Input data:
1. Merged Sholl Analysis files saved as .csv files. See example files named Cond0[]_Rep[].csv. All files must have some common string such as 'Cond' etc. <b> Critical step!: </b> ALL images names must be unique across the entire data set. Your data set and key should appear in a single experiment computer directory. 
2. Key file saved as a csv See example file.  L1 is just the list number as it appears in order on your computer. <b> Critical step!:</b> the key file must list your files in the SAME ORDER as they appear in your computer folder. Typically this is alphanumerically. See  comment on code line 64 of R script.

Output:
1. Sholl analysis graphs for each replicate and the average across replicates
2. All merged data
3. ANOVA model output and Tukey post-hoc output

Citations:
Wilson, M. D., Sethi, S., Lein, P. J., & Keil, K. P. (2017). Valid statistical approaches for analyzing sholl data: Mixed effects versus simple linear models. Journal of neuroscience methods, 279, 33-43. PMID: 28104486
https://github.com/adrigabzu/sholl_analysis_in_R
