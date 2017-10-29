#====================================================================#
# Project 2
# Rachel Johnson
#====================================================================#

#Overview and analysis plan for Project 2:

-Purpose of analysis is to examine 30 day mortality data at 44 VA hospitals in period 39
-Want to compare the observed rate in the last 6 months to the expected rate for period 39 (which is population-adjusted 
and includes data over the last 6 periods)
-Run a logistic regression for death30 	~ proced + bmi + asa with and without albumin as a covariate
-Use fitted values from regression (without albumin) to get a predicted probability of death for each individual in period 39
-Average these probabilities by hospital--these are the expected probabilities of death
-Investigator stated that if observed/expected > 1.2 then they would want to check on hospital (added this column to hosp table)
-Bootstrap around individuals' probability in pd 39, then use to get 95% CI with 2.5% and 97.5% from each hospital distribution


#Steps to recreate analysis (all code files in bios6623-johnsra3/Project2/Code):
#Follow in this order:

1. Project 2 Data Cleaning.R
	-Input file: vadata2.sas7bdat
	-Output file: VadataCleaned.csv 
	-Both files saved in AdvancedData (personal folder, since no data on GitHub)
	-Fixes BMI issues, removes incorrect procedures, categorizes ASA differently acc. to discussion w/ investigator

2. Project 2 Missing Data.R
	-Input file: VadataCleaned.csv
	-Output file: NA
	-Explores missing data related to the variable albumin
	
3. Project 2 Table 1.R
	-Input file: VadataCleaned.csv
	-Output file: bios6623-johnsra3/Project2/Reports/TableOverallCharacteristics.csv
	-Creates simple demographics table (with no stratification)

4. Project 2 Logistic Regressions.R
	-Input file: VadataCleaned.csv
	-Output files: bios6623-johnsra3/Project2/Reports/ModelWithAlbuminResults.csv;
			bios6623-johnsra3/Project2/Reports/ModelWithoutAlbuminResults.csv;
			bios6623-johnsra3/Project2/Reports/CompleteCasesPd39Predicted.csv;
	-Runs logistic regression model with and without albumin for comparison, creates results tables, finds indivs' predicted values

5. Project 2 Boostrapping.R
	-Input file: VadataCleaned.csv
	-Output file: bios6623-johnsra3/Project2/Reports/BootstrapResults_raw.csv

6. Project 2 Overall Results Table.R
	-Input files: VadataCleaned.csv; bios6623-johnsra3/Project2/Reports/BootstrapResults_raw.csv;
			bios6623-johnsra3/Project2/Reports/CompleteCasesPd39Predicted.csv
	-Output files: bios6623-johnsra3/Project2/Reports/TableDeathsByHospital.csv (note: only in interim presentation, not final report);
			bios6623-johnsra3/Project2/Reports/TableExpectedPropsBootstrap.csv
			


