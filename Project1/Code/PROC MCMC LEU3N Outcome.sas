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
* Crude model for LEU3N for drugs only (and base)
*-----------------------------------------------------------;
*DIC: 6299.101, DIC is better w/ other things in model;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Crude Model of CD4+ count (drugs)";
RUN; title;


*-----------------------------------------------------------;
* Full model for LEU3N
*-----------------------------------------------------------;
*DIC: 5879.523;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
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

*-----------------------------------------------------------;
* Full model for LEU3N (remove drink)
*-----------------------------------------------------------;
*DIC: 5878.247, lower w/o drink so remove at next step; 
 
PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + 
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (Remove drink)";
RUN; title;

*-----------------------------------------------------------;
* Full model for LEU3N (remove smoke)
*-----------------------------------------------------------;
*DIC: 5878.376, lower w/o smoke so can remove in next step; 
PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betamarij 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (Remove smoke)";
RUN; title;


*-----------------------------------------------------------;
* Full model for LEU3N (remove marij)
*-----------------------------------------------------------;
*DIC: 5882.963, is worse w/o marijuana, so keep it in at next step;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (Remove marijuana)";
RUN; title;

*-----------------------------------------------------------;
* Full model for LEU3N (remove income)
*-----------------------------------------------------------;
*DIC: 6122.414, definitely need to keep income;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (Remove income)";
RUN; title;

*-----------------------------------------------------------;
* Full model for LEU3N (remove educ)
*-----------------------------------------------------------;
*DIC: 6122.414, need to keep education at next step;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (Remove educ)";
RUN; title;


*-----------------------------------------------------------;
* Next model: remove drink and smoke together
*-----------------------------------------------------------;
*DIC: 5877.083 (better w/o drink/smoke, okay to remove both);

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betamarij 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (rem. drink/smoke)";
RUN; title;

*-----------------------------------------------------------;
* Next model: remove drink and smoke together
* See if income can be removed
*-----------------------------------------------------------;
*Autocorrelation is bad w/o income and DIC increases a lot, keep income in;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betamarij 0 betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betamarij betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betamarij*marijY + betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (rem. drink/smoke + inc.)";
RUN; title;


*-----------------------------------------------------------;
* Next model: remove drink and smoke together (and educ)
*-----------------------------------------------------------;
*DIC: 5877.100, v. slightly above, take out of model for parsimony;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betamarij 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betamarij betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (rem. drink/smoke + educ.)";
RUN; title;


*-----------------------------------------------------------;
* Next model: remove drink and smoke together (and marij)
*-----------------------------------------------------------;
*DIC: 5880.650, can remove marijuana;

PROC MCMC data = hiv nbi = 7500 nmc = 250000 thin = 10 plots = all DIC seed = 204;
	PARMS betaint 0 betaLEU3N 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betaincmed 0 betainchigh 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaLEU3N betadrugs betaage betabmi betaadh betarace
		betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaLEU3N*LEU3N + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_LEU3N ~ normal(mu, var = sigma2);
	title "Full Model of CD4+ count (rem. drink/smoke + marij)";
RUN; title;

*final model: remove drink, smoke, educ, marij;
