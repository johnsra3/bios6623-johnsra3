*-----------------------------------------------------;
* Project 3
* Mixed Models for Outcome Animals
* Rachel Johnson
*-----------------------------------------------------;

*-----------------------------------------------------;
* Import data
*-----------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\AnimalsModeling.csv"
	out = animals replace;
	getnames = yes;
RUN;

*-----------------------------------------------------;
* Plan: (1) model w/ random intercept
		(2) model w/ random intercept + AR(1)
		(3) model w/ random intercept + SP(POW)(age)
*-----------------------------------------------------;

*-----------------------------------------------------;
* (1) model w/ random intercept-- AIC 7908.5
*-----------------------------------------------------;

PROC MIXED data = animals;
	CLASS id gender demind;
	MODEL animals = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
RUN;

PROC FREQ data = animals;
	TABLE demind;
RUN;

*-----------------------------------------------------;
* (2) model w/ random intercept + AR(1)- AIC 7882.0
*-----------------------------------------------------;

PROC MIXED data = animals;
	CLASS id gender demind;
	MODEL animals = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = AR(1) subject = id;
RUN;

*-----------------------------------------------------;
*(3) model w/ RI + SP(POW)(age)- AIC 7881.3
*-----------------------------------------------------;

PROC MIXED data = animals;
	CLASS id gender demind;
	MODEL animals = age_59 demind age_59*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = SP(POW)(age) subject = id;
RUN;

*Best AIC is from RI + SP(POW)(age), but just barely better than AR(1);
