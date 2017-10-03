*-----------------------------------------------------------;
* Project 1: Aggregate Physical Score Outcome
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
* Full model for agg_phys
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3094.218;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys (remove drink)
*-----------------------------------------------------------;
*First run: 2500 burning, 30000 its, DIC: 3092.302;
*DIC improved w/o drink, remove it in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + 
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Drink)";
RUN; title;


*-----------------------------------------------------------;
* Full model for agg_phys (remove smoke)
*-----------------------------------------------------------;
*DIC: 3092.216, DIC improved w/o smoke, remove in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Smoke)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys (remove marijuana)
*-----------------------------------------------------------;
*DIC: 3092.388, DIC slightly better w/o marijuana, remove in next step;

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
	title "Full Model of Aggregate Physical Score (Remove Marijuana)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys (remove income)
*-----------------------------------------------------------;
*DIC: 3220.329, way worse w/o income, need to keep it;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + 
		betaeduc*educHSmore;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Income)";
RUN; title;

*-----------------------------------------------------------;
* Full model for agg_phys (remove education)
*-----------------------------------------------------------;
*DIC: 3093.707, slightly lower w/o education, remove it;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC seed = 204;
	PARMS betaint 0 betaAP 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAP betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAP*AGG_PHYS + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggphys ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Physical Score (Remove Education)";
RUN; title;


*-----------------------------------------------------------;
* Model for agg_phys (remove drink, smoke, marijuana, educ)
*-----------------------------------------------------------;
*DIC: 3089.996, dic is better now, remove all 4 of these;

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
	title "Full Model of Aggregate Physical Score (Remove 4 vars)";
RUN; title;

*All vars must stay in model now, keep it here;
