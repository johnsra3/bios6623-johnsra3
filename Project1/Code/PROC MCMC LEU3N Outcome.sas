*-----------------------------------------------------------;
* Project 1
* Rachel Johnson
* date started: 09/27/2017
* last updated: 09/27/2017
*-----------------------------------------------------------;

*-----------------------------------------------------------;
* Import indicator data frame from R code
*-----------------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\IndicatorHIVdata09252017_dots.csv" 
	dbms = csv 
	out = hiv replace;
	getnames = yes;
RUN;

*-----------------------------------------------------------;
*-----------------------------------------------------------;
* Model for LEU3N with hard_drugs and baseline only
*-----------------------------------------------------------;
*-----------------------------------------------------------;
*Huge autocorrelation problem w/ all betas in one PARMS statement,
	so moved them to separate statements;
*Still need to increase nbi and nmc to improve trace plots;
*DIC (5000 nbi 50000 nmc): 6603.531;

PROC MCMC data = hiv nbi = 1000 nmc = 10000 plots = all DIC;
	PARMS betaint 0;
	PARMS betaLEU3N 0 ;
	PARMS betadrugs 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "CD4+ count ~ drugs baseline";
RUN; title;

*-----------------------------------------------------------;
* Full model for LEU3N
*-----------------------------------------------------------;
*W/ 5000 nbi and 50000 nmc, having issues w/ betaint and betaage mixing;

PROC MCMC data = hiv nbi = 5000 nmc = 50000 plots = all DIC;
	PARMS betaint 0;
	PARMS betaLEU3N 0;
	PARMS betadrugs 0;
	PARMS betaage 0;
	PARMS betabmi 0;
	PARMS betaadh 0;
	PARMS betarace 0;
	PARMS betadrink 0;
	PARMS betasmoke 0;
	PARMS betaincmed 0;
	PARMS betainchigh 0;
	PARMS betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count";
RUN; title;
