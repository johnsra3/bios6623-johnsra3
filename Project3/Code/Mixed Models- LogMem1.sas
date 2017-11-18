*-----------------------------------------------------;
* Project 3
* Mixed Models for Outcome LogMem1
* Rachel Johnson
*-----------------------------------------------------;

*-----------------------------------------------------;
* Import data
*-----------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\LogMem1Modeling.csv"
	out = logmem1 replace;
	getnames = yes;
RUN;

*-----------------------------------------------------;
* Plan: (1) model w/ random intercept
		(2) model w/ random intercept + AR(1)
		(3) model w/ random intercept + SP(POW)(age)
*-----------------------------------------------------;

*-----------------------------------------------------;
* (1) model w/ random intercept-- AIC 7806.7
*-----------------------------------------------------;

PROC MIXED data = logmem1;
	CLASS id gender demind;
	MODEL logmemI = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
RUN;

*-----------------------------------------------------;
* (2) model w/ random intercept + AR(1)- AIC 7698.6
*-----------------------------------------------------;

PROC MIXED data = logmem1;
	CLASS id gender demind;
	MODEL logmemI = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = AR(1) subject = id;
RUN;

*-----------------------------------------------------;
*(3) model w/ RI + SP(POW)(age)- AIC 7699.8
*-----------------------------------------------------;

PROC MIXED data = logmem1;
	CLASS id gender demind;
	MODEL logmemI = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = SP(POW)(age) subject = id;
RUN;

*RI + AR(1) is best, just slightly better than RI + SP(POW)(age)
