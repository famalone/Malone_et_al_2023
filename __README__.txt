Author: Fin Malone

This folder contains all that is needed to reproduce my results from the collected data
and R scripts.

Here are the steps:

1. Open all three R scripts and set the work.dir variables to the directory of this folder
	on your computer. An example work.dir is already supplied in each script.
	Replace these with your own. Here are the locations of each work.dir variable:
	
	0_MasterScript.R: lines 6 and 11
	1_Fit_GrowthCurves.R: line 20
	2_Figures.R: line 38

2. Run 0_MasterScript.R line by line. This will run the other scripts and write a
	new excel file with processed data. This will also fill "Figures" and "Stat_Output"
	folders with images and results relating to the respective analysis. It should
	take 1 minute or less to run everything, depending on whether the relavant packages
	are installed or not.

3. That should be it! Metadata for all raw and processed datasets are in the
	"_1_metadata_thesis.xlsx" file. Raw field data, SNOTEL precipitation data, and
	geospatial data can be found in the "_2_lubrecht_data_thesis.xlsx" file. Check out
	script documentation for additional details regarding the analysis.

Contact finmalone@gmail.com for questions.