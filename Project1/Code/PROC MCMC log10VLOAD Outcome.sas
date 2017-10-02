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
