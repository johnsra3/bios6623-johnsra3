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
	-Output files: BlockROutcome.csv, AnimalsOutcome.csv, LogMem1Outcome.csv, LogMem2Outcome.csv
	-Files saved in AdvancedData (personal folder, since no data on GitHub)
	-Removes individuals who don't have at least 3 time points for ANIMALS OUTCOME ONLY (as of 11/20/2017 email)

2. Project 3 Demographics Table.R
	-Input file: Project3Data.csv
	-Output file: Table1Demographics.csv
	-Uses first observation for all individuals w/ at least 3 animals outcomes to create demographics table at baseline, 
		stratified b/t those who do and don't develop dementia/MCI during course of the study	

3. Project 3 Spaghetti Plots.R
	-Input files: BlockROutcome.csv, AnimalsOutcome.csv, LogMem1Outcome.csv, LogMem2Outcome.csv
	-Output files: C:\Repositories\bios6623-johnsra3\Project3\Reports\Spaghetti Plots- Overall Trajectories.png,
			C:\Repositories\bios6623-johnsra3\Project3\Reports\Spaghetti Plots- Time before dementia diagnosis.png
	-Spaghetti plots (1-4): trajectory for all indivs over course of study, colored by dementia status 
	-Spaghetti plots (5-8): trajectory for individuals with dementia leading up to diagnosis

4. Functions- Change Points and Bootstrap.R
	-Contains functions to find change points and bootstraps for those changepoints
	-Sourced in Project 3 Find Change Point.R and Project 3 Bootstrap Change Point.R

5. Project 3 Find Change Point.R
	-Input files: BlockROutcome.csv, AnimalsOutcome.csv, LogMem1Outcome.csv, LogMem2Outcome.csv
	-Output files: BlockRModeling.csv, AnimalsModeling.csv, LogMem1Modeling.csv, LogMem2Modeling.csv
	-Adapts Camille Moore's code to find change point for each outcome model

6. Project 3 Bootstrap Change Point.R
	-Input files: BlockROutcome.csv, AnimalsOutcome.csv, LogMem1Outcome.csv, LogMem2Outcome.csv
	-Output files: Bootstraps_blockr.sv, Bootstraps_animals.csv, Bootstraps_logmem1.csv, Bootstraps_logmem2.csv

7. Project 3 Summarize CP Bootstrap.R
	-Input files: Bootstraps_blockr.sv, Bootstraps_animals.csv, Bootstraps_logmem1.csv, Bootstraps_logmem2.csv
	-Output files: ...................

8. Mixed Models- BlockR (SAS File)
	-Input file: BlockRModeling.csv
	
9. Mixed Models- Animals (SAS File)
	-Input file: AnimalsModeling.csv

10. Mixed Models- LogMem1 (SAS File)
	-Input file: LogMem1Modeling.csv

11. Mixed Models- LogMem2 (SAS File)
	-Input file: LogMem2Modeling.csv


