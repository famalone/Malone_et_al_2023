
# Master R Script (Author: Fin Malone)


# Run scripts to produce data transformation, analysis, and figures
work.dir = 'D:/Documents/1_MASTERS/Malone_et_al_2023'

source(paste0(work.dir,'/1_Fit_GrowthCurves.R'), echo = TRUE)
rm(list = ls())

work.dir = 'D:/Documents/1_MASTERS/Malone_et_al_2023'

source(paste0(work.dir,'/2_Figures.R'), echo = TRUE)
rm(list = ls())
