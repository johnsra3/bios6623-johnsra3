*-----------------------------------------------------;
* Project 3
* Mixed Models for Outcome BlockR
* Rachel Johnson
*-----------------------------------------------------;

*-----------------------------------------------------;
* Import data
*-----------------------------------------------------;

PROC IMPORT datafile = "C:\Users\johnsra3\Documents\School\AdvancedData\BlockRModeling.csv"
	out = blockr replace;
	getnames = yes;
RUN;

*-----------------------------------------------------;
* Plan: (1) model w/ random intercept
		(2) model w/ random intercept + AR(1)
		(3) model w/ random intercept + SP(POW)(age)
		(4) model w/ random intercept + UN;
*-----------------------------------------------------;

*-----------------------------------------------------;
* (1) model w/ random intercept-- AIC 9048.7
*-----------------------------------------------------;

PROC MIXED data = blockr;
	CLASS id gender demind;
	MODEL blockr = age demind age*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
RUN;

*-----------------------------------------------------;
* (2) model w/ random intercept + AR(1)- AIC 9026.7
*-----------------------------------------------------;

PROC MIXED data = blockr;
	CLASS id gender demind;
	MODEL blockr = age demind age*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = AR(1) subject = id;
RUN;

*-----------------------------------------------------;
*(3) model w/ RI + SP(POW)(age)- AIC 9023.9
*-----------------------------------------------------;

PROC MIXED data = blockr;
	CLASS id gender demind;
	MODEL blockr = age demind age*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = SP(POW)(age) subject = id;
RUN;

*-----------------------------------------------------;
*(4) model w/ random intercept + UN
*-----------------------------------------------------;

PROC MIXED data = blockr;
	CLASS id gender demind;
	MODEL blockr = age demind age*demind timecp ses gender / solution;
	RANDOM intercept / subject = id;
	REPEATED / type = UN subject = id;
RUN;
*Unable to make Hessian positive definite (not surprising);

*Best AIC: random intercept + SP(POW)(age)
