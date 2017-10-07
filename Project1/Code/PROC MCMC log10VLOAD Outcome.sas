*-----------------------------------------------------------;
* Project 1
* Rachel Johnson
* date started: 09/27/2017
* last updated: 10/03/2017
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
* Crude model of just drugs (and baseline)
*-----------------------------------------------------------;
*DIC: 390.449 (better for full model);

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Crude Model of log10 VLOAD (drugs only)";
RUN; title;


*-----------------------------------------------------------;
* Full model of log10vload
*-----------------------------------------------------------;
*DIC: 372.812;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD";
RUN; title;

*-----------------------------------------------------------;
* Full model of log10vload (remove drink)
*-----------------------------------------------------------;
*DIC: 370.643, is better w/o drink, so remove drink;

PROC MCMC data = hiv nbi = 2500 nmc = 30000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betasmoke betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (remove drink)";
RUN; title;

*-----------------------------------------------------------;
* Full model of log10vload (remove smoke)
*-----------------------------------------------------------;
*DIC: 370.716, better w/o smoke, so remove smoke in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betamarij 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betadrink betamarij betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (remove smoke)";
RUN; title;

*-----------------------------------------------------------;
* Full model of log10vload (remove marij)
*-----------------------------------------------------------;
*DIC: 371.730, is better w/o marij, so remove from next step;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betaincmed 0 betainchigh 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betaincmed betainchigh betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betaincmed*incomemed + betainchigh*incomehigh +
		betaeduc*educHSmore;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (remove marij)";
RUN; title;

*-----------------------------------------------------------;
* Full model of log10vload (remove income)
*-----------------------------------------------------------;
*DIC: 384.030, worse w/o income, so keep income;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0
		betaeduc 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij  betaeduc ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + 
		betaeduc*educHSmore;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (remove income)";
RUN; title;

*-----------------------------------------------------------;
* Full model of log10vload (remove educ)
*-----------------------------------------------------------;
*DIC: 370.557, better w/o educ, can remove in next step;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betadrink 0 betasmoke 0 betamarij 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betadrink betasmoke betamarij betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betadrink*drink13plus +
		betasmoke*smokecurrent + betamarij*marijY + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (remove educ)";
RUN; title;


*-----------------------------------------------------------;
* Next model: remove drink, smoke, marij, educ in 1 step
*-----------------------------------------------------------;
*DIC: 368.099;

PROC MCMC data = hiv nbi = 2500 nmc = 50000 plots = all DIC;
	PARMS betaint 0 betalogvload 0 betadrugs 0 betaage 0 betabmi 0 betaadh 0
		betarace 0 betaincmed 0 betainchigh 0;
	PARMS sigma2 1;
	PRIOR betalogvload betadrugs betaage betabmi betaadh betarace
		betaincmed betainchigh ~ normal(mean = 0, var = 1000);
	PRIOR betaint ~ normal(mean = 0, var = 10000);
	PRIOR sigma2 ~ igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betalogvload*logvload + betadrugs*harddrugsY + betaage*age +
		betabmi*BMI + betaadh*adhhigh + betarace*raceNHW + betaincmed*incomemed + betainchigh*incomehigh;
	model diff_logvload ~ normal(mu, var = sigma2);
	title "Full Model of log10 VLOAD (rem. 4 vars)";
RUN; title;

*DIC is best w/o them, remove all 4 vars;
