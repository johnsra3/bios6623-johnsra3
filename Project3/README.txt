#====================================================================#
# Project 3
# Rachel Johnson
#====================================================================#

#Overview and analysis plan for Project 3:

-Purpose of analysis is to identify trajectories of onsent of memory/other cognitive loss in people at risk of developing mild cognitive impairment (MCI) or dementia
-Population is healthy, community dwelling, cognitively intact elders 
-Developing MCI def. by 2+ consec. Clinical Dementia Ratings (CDRs) >= 0.5
-Want to quantify annual rates of change on four cognitive tests [(1) the Wechsler Memory Scale Logical Memory I Story A, 
	(2) Wechsler Memory Scale Logical Memory II Story A; (3) category fluency for animals; and, (4) the Wechsler Adult Intelligence Scale-Revised Block Design]
-Subj. must be followed for at least 3 time points to be included in an analysis for a certain outcome


#Steps to recreate analysis (all code files in bios6623-johnsra3/Project2/Code):
#Follow in this order:

1. Project 3 Data Cleaning.R
	-Input file: Project3Data.csv
	-Output file: BlockROutcome.csv, AnimalsOutcome.csv, LogMem1Outcome.csv, LogMem2Outcome.csv
	-Both files saved in AdvancedData (personal folder, since no data on GitHub)
	-Removes rows that are completely missing outcomes, then for each outcome removes individuals who don't have at least 3 time points 
	-Simple exploratory data and basic spaghetti plots
