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
*First run: 10,000 iterations are fine for this, only increase if you need to
	*based on other models;
*DIC: 3539.865;
*Full model needed 2500 nbi, 30000 nmc, so increase here;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
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
*First run: 2500 burning, 30000 its, DIC: 3377.138;

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
* Full model for agg_phys-- remove drink
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3375.059;
*DIC improved without drink (can remove at next step);

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Drink)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys- remove smoke
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3376.596;
*DIC is slightly lower w/o smoke, can remove it in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Smoke)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys-- remove income
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3501.208;
*DIC gets much worse w/o income--need to keep it in model;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Income)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys-- remove education
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3376.777;
*DIC improved w/o education, can remove it at next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Education)";
RUN; title;

*-----------------------------------------------------------;
* See if all DIC-incr. vars can be removed from full 
* (drink, smoke, education) 
*-----------------------------------------------------------;
*When all 3 are removed, DIC: 3374.392, t/f remove all 3;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (3 Removed)";
RUN; title;

*At this point, all covariates must stay in model, stop here;
