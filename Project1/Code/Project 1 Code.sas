*---------------------------------------------------------------------------;
* Advanced Data: Project 0
* Rachel Johnson
* Last updated: 09/18/2017
*---------------------------------------------------------------------------;

PROC IMPORT filename = "C:\Users\johnsra3\Documents\School\AdvancedData\hiv_6623_final.csv" 
	out = hiv dbms = csv replace;
	getnames = yes;
RUN;
