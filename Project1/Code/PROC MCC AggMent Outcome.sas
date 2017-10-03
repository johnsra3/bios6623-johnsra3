*-----------------------------------------------------------;
* Project 1
* Rachel Johnson
* date started: 09/27/2017
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
* Full model for AGG_MENT
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc;
*DIC: 3606.595;

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
* Full model for AGG_MENT
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc;
*DIC: 3606.595;

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
* Full model for AGG_MENT
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc;
*DIC: 3606.595;

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
* Full model for AGG_MENT (remove drink)
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc, DIC = 3603.993;
*DIC improved w/o drink, remove it at next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Drink)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove smoke)
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc, DIC: 3605.959;
*DIC slightly improved w/o smoke (try to remove for parsimony + DIC);

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betaincmed 0 betainchigh 0 betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Smoke)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove income)
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc, DIC: 3726.897;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Income)";
RUN; title;
