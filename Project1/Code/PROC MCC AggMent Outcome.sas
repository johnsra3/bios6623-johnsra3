*-----------------------------------------------------------;
* Project 1
* Rachel Johnson
* date started: 09/27/2017
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
* Crude model for AGG_MENT (drugs and base only)
*-----------------------------------------------------------;
*DIC: 3547.100, better in big model, wh/ is good;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Crude Model of Aggregate Mental Score";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc;
*DIC: 3330.507;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove drink)
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc, DIC = 3328.351;
*DIC improved w/o drink, remove it in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Drink)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove smoke)
*-----------------------------------------------------------;
*DIC: 3331.001, DIC slightly better w/ smoke, can keep;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Smoke)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove marij)
*-----------------------------------------------------------;
*First run: 2500 nbi, 30000 nmc, DIC: 3330.325; 
*DIC slightly worse w/ marij, remove in next step for parsimony;

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
	title "Full Model of Aggregate Mental Score (Remove Marijuana)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove income)
*-----------------------------------------------------------;
*DIC: 3449.451, much worse w/o income, keep it;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaeduc 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaeduc*educHSmore;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Income)";
RUN; title;

*-----------------------------------------------------------;
* Full model for AGG_MENT (remove educ)
*-----------------------------------------------------------;
*DIC: 3329.512, better w/o education, try to remove in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove Education)";
RUN; title;

*-----------------------------------------------------------;
* Next step: try to remove drink, marijuana, education
*-----------------------------------------------------------;
*DIC: 3327.321, better w/o these vars, can remove them;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		 betasmoke betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + 
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove 3 vars)";
RUN; title;

*-----------------------------------------------------------;
* Next step: see if smoke should be removed or not
*-----------------------------------------------------------;
*DIC: 3327.481;
*Approximately the same, can remove for parsimony; 

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betaAM 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betaint betaAM betadrugs betaage betabmi betaadh betarace
		 betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaAM*AGG_MENT + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + 
		betaincmed*incomemed + betainchigh*incomehigh;
	model diff_aggment ~ normal(mu, var = sigma2);
	title "Full Model of Aggregate Mental Score (Remove 3 vars + Smoke)";
RUN; title;
