*-----------------------------------------------------;
* Project 3
* Mixed Models for Outcome LogMem2
* Rachel Johnson
*-----------------------------------------------------;

*-----------------------------------------------------;
* Import data
*-----------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\LogMem2Modeling.csv"
	out = logmem2 replace;
	getnames = yes;
RUN;

*-----------------------------------------------------;
* Plan: (1) model w/ random intercept
		(2) model w/ random intercept + AR(1)
		(3) model w/ random intercept + SP(POW)(age)
*-----------------------------------------------------;

*-----------------------------------------------------;
* (1) model w/ random intercept-- AIC 8049.9
*-----------------------------------------------------;

PROC MIXED data = logmem2;
	CLASS id gender demind;
	MODEL logmemII = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
RUN;

*-----------------------------------------------------;
* (2) model w/ random intercept + AR(1)- AIC 7892.5
*-----------------------------------------------------;

PROC MIXED data = logmem2;
	CLASS id gender demind;
	MODEL logmemII = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = AR(1) subject = id;
RUN;

*-----------------------------------------------------;
*(3) model w/ RI + SP(POW)(age)- AIC 7904.1
*-----------------------------------------------------;

PROC MIXED data = logmem2;
	CLASS id gender demind;
	MODEL logmemII = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = SP(POW)(age) subject = id;
RUN;

*AR(1) + RI is best for AIC
