*-----------------------------------------------------------;
* Project 1: Aggregate Physical Score Outcome
* Rachel Johnson
* date started: 10/02/2017
* last updated: 10/02/2017
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
* Model w/ only drugs (and baseline)
*-----------------------------------------------------------;

PROC MCMC data = hiv nbi = 1000 nmc = 10000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Simple Model of Aggregate Physical Score";
RUN; title;


*-----------------------------------------------------------;
* Full model for agg_phys
*-----------------------------------------------------------;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (One Parm)";
RUN; title;

*-----------------------------------------------------------;
* Test w/ separate parms statements to see if diff results;
*-----------------------------------------------------------;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0;
	PARMS betaAP 0 ;
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
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Multiple Parms)";
RUN; title;
