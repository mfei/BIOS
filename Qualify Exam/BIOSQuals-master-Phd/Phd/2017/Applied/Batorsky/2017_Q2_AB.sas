/*
Year 2017 Problem 2
Input: CHILD.dat 
Variables: 	RX: group to which child was randomized (Control, Intervention)
			Y0:	Indicator of child obesity at baseline (0=not obese, 1=obese)
			Y1: Indicator of child obesity at follow-up (0=not obese, 1=obese, NA=missing)
			AGE: child's age in years at baseline
			SEX: child's sex (Male, Female)
			RACE: child's race (African American, Caucasian)
			PARENT_WT: Indicator of parent obesity (0=not obese, 1=obese)
Derived variables: M1: Indicator of missing data at follow-up visit (0=not missing, 1=missing)

Instructions: 	Investigate patterns of missing at follow-up. Perform complete-case analysis.
				Then perform MI on outcome at follow-up visit and re-do analysis. 
*/

* read in data;
data child;
	infile "C:\Users\abatorsk\Documents\My SAS Files\Comp\A2017\CHILD.dat" firstobs=2;
	input RX $ Y0 Y1 AGE SEX $ RACE $ PARENT_WT;
	if Y1=. then M1=1; else M1=0;
	if Y0=1 and Y1=0 then change="obese became non-obese";
	if Y0=1 and Y1=1 then change="stayed obese";
	if Y0=0 and Y1=0 then change="stayed non-obese";
	if Y0=0 and Y1=1 then change="became obese";
	if Y1=. then change="lost to follow up";
	label	rx="Treatment"
			y0="Obesity status at baseline"
			y1="Obesity status at follow-up"
			age="Age"
			sex="Sex"
			race="Race"
			parent_wt="Parent obesity status"
			m1="Missing data at follow-up";
run;
*How many children changed status? How does this look across treatment groups?;
title1 "How many children changed status? How does this look across treatment groups?";
proc freq data=child;
	table change change*rx/missing;
run;
title1;
* Part A: 	Tabulate descriptive statistics for baseline variables - overall and by intervention group
			Provide descriptive stats for Y1 by intervention group. Comment;

ods pdf file="C:\Users\abatorsk\Documents\My SAS Files\Practice\2017 Output\Q2PartA.pdf";
proc tabulate data=child missing;
	class rx sex race parent_wt y0 y1;
	var age;
	table (rx all), (age)*(n mean median std)  (sex race parent_wt y0 y1)*(n colpctn);
run;

proc format;
	value Yzero	0="Not obese at baseline"
				1="Obese at baseline";
	value Yone	0="Not obese at follow-up"
				1="Obese at follow-up";
	value $rx	"Control"="Control"
				"Interven"="Intervention";
	value $race	"Caucasia"="Caucasian"
				"African-"="African-American";
run;	

title1 "Descriptive Statistics";
proc report data=child nowd;
	column rx sex race (N Mean Std), age parent_wt y0 y1 m1;
	define rx / group format=$rx.;
	define race /group format=$race.;
	define sex / group;
	define parent_wt / analysis n mean median std format=percent5.2 style=[cellwidth=1in];
	define age / analysis n mean median std format=5.2;
	define y0 / analysis n mean median std format=percent5.2 style=[cellwidth=1in];
	define y1 / analysis n mean median std format=percent5.2 style=[cellwidth=1in];
	define m1 / analysis n mean median std format=percent5.2 style=[cellwidth=1in];
	rbreak after / summarize style=[backgroundcolor=ltgray];
run;
title1;

* Part B: Examine and describe extent of missing data for outcome - does it vary by RX and how does this affect outcome?;

proc tabulate data=child missing;
	class rx sex race parent_wt y0 y1 m1;
	var age;
	table (m1), (age)*(n mean median std)  (rx sex race parent_wt y0)*(n colpctn);
run;
ods pdf close;

* Part C: 	Considering only complete cases assess effect of intervention on risk of obesity at follow-up.
			Adjust for baseline obesity status, age, sex, race, and parents' obesity status. 
			Summarize findings, interpret relevant parameter estimates and corresponding 95% confidence intervals.;
/*
proc genmod descending data=child;
  class rx sex race parent_wt (ref="0") y0 (ref="0")/ param=ref;
  model y1 = rx age sex race parent_wt y0 / dist=binomial link=logit noscale type3 wald;
run;
*/
ods pdf file="C:\Users\abatorsk\Documents\My SAS Files\Practice\2017 Output\Q2PartC.pdf";
title1 "Use main effects only";
proc logistic descending data=child;
  class rx (ref="Control") sex race parent_wt (ref="0") y0 (ref="0")/ param=ref;
  model y1 = rx age sex race parent_wt y0 /scale=none waldcl;
  oddsratio rx;
run;
title1 "Use interaction of baseline status and treatment";
proc logistic descending data=child;
  class rx (ref="Control") sex race parent_wt (ref="0") y0 (ref="0")/ param=ref;
  model y1 = rx age sex race parent_wt y0 y0*rx /scale=none waldcl;
  oddsratio rx;
run;
title1;
ods pdf close;
* Part D:	Perform MI on outcome variable;
ods pdf file="C:\Users\abatorsk\Documents\My SAS Files\Practice\2017 Output\Q2PartD.pdf";
proc MI data=child seed=0705 nimpute=25 out=michild;
	var age rx sex race parent_wt y0 y1;
	class rx sex race parent_wt y0 y1;
	monotone logistic(y1=age rx sex race parent_wt y0);
run;

proc sort data=michild;
	by _imputation_;
run;
/*  for some reason I can't do param=ref and therefore there are missing values for std errors
proc mixed data=michild noclprint;
  class rx (ref="Control") sex (ref="Male") race (ref="Caucasia") parent_wt (ref="0") y0 (ref="0");
  model y1 = rx sex race parent_wt y0/s covb;
  by _imputation_;
  ods output solutionf=beta;
run;
*/

proc genmod descending data=michild;
  class rx (ref="Control") sex race parent_wt (ref="0") y0 (ref="0")/ param=ref;
  model y1 = age rx sex race parent_wt y0 / dist=binomial link=logit noscale type3 wald;
  by _imputation_;
  ods output ParameterEstimates=beta;
run;

proc mianalyze parms=beta;
	modeleffects intercept rx age sex race parent_wt y0;
run;
ods pdf close;

* Part E:	Discuss possible missing data mechanisms for which analyses in parts C and D are valid.
			Provide mathematical statement of missing data mechanisms discussed.
			Explain to what extent the observed data provide evidence of missing data mechanism.;

ods pdf file="C:\Users\abatorsk\Documents\My SAS Files\Practice\2017 Output\Q2PartE.pdf";
* if missing completely at random (MCAR), M1 will not be associated with Y0 - see page 493 of textbook;
title1 "Test for MCAR";
proc freq data=child;
	table Y0*M1/ chisq;
	exact chisq;
run;
title1;
ods pdf close;

* DON'T DO THIS - I just wanted to see what would happen
	Used imputed data to test associations between outcomes from imputed data and 
	missingness status from original data set;
data mi1;
	set michild;
	where _imputation_=1;
	drop _imputation_;
run;

* is there some way to save output across 25 imputed data sets and see if all p values are >0.05?;
title1 "Test for MAR";
proc freq data=mi1;
	table Y0*M1*Y1/ chisq cmh;
	exact chisq;
run;
title1;

