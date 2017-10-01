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

PROC MCMC data = hiv nbi = 5000 nmc = 50000 plots = all DIC;
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

*-----------------------------------------------------------;
*-----------------------------------------------------------;
* Model for AGG_MENT with only hard_drugs as predictor 
* (baseline also included for sake of interpretation)
*-----------------------------------------------------------;
*-----------------------------------------------------------;

* First run: mixing is somewhat poor w/ 10000 nmc;
* Second run: slightly better w/ 20000 for betas mixing;
* Third run: discard more burn-in and increase nmc to 30,000;
* After third run, mixing + autocorr improve; 
* DIC: 3760.370; 

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0; 
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "AGG_MENT ~ hard_drugs";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT
*-----------------------------------------------------------;
*First run w/ 10000 nmc had fairly poor mixing in betas, need to run chain longer;
*Second run w/ 20000 nmc had fairly poor mixing, try even higher?;
*Third run w/ 30000 nmc was slightly better 2500 nbi, but still poor overall;
	*Try something to take care of autocorrelation?;


PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score";
RUN; title;

*-----------------------------------------------------------;
*-----------------------------------------------------------;
* Model for log10VLOAD w/ only hard_drugs + baseline;
*-----------------------------------------------------------;
*-----------------------------------------------------------;
*1st: sep parms statements, nbi 5000, nmc 50000-- mixing issues in int & logvload;
*2nd: try same #s in 1 parm statement for betas--good!;

PROC MCMC data = hiv nbi = 5000 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0;
	PARMS sigma2 1;
	PRIOR betaint betalogvload betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10vload count";
RUN; title;
