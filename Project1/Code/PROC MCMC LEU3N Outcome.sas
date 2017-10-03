*-----------------------------------------------------------;
* Project 1
* Rachel Johnson
* date started: 10/02/2017
* last updated: 10/02/2017
*-----------------------------------------------------------;

*-----------------------------------------------------------;
* Import indicator data frame from R code
*-----------------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\IndicatorHIVdata10022017_dots.csv" 
	dbms = csv 
	out = hiv replace;
	getnames = yes;
RUN;

*-----------------------------------------------------------;
* Full model for LEU3N
*-----------------------------------------------------------;
*DIC: 5879.669;

PROC MCMC data = hiv nbi = 7500 nmc = 150000 thin = 5 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count";
RUN; title;


*keep running models!!; 
