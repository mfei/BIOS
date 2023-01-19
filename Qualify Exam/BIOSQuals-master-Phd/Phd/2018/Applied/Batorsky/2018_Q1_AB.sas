/*
2018 Qualfying Exam Question 1
Disease: Disease severity/status (AD = Alzheimer's disease, MCI = Mild cognitive impairment, NL = Normal)
Columns 3-110: 108 image ROI biomarkers
Columns 111-120: 10 SNPs - genotype category 1 or 2
Age: continuous age
Gender: 1=female, 2=male
Columns 123-127: gPC: Genetic principle components
Phase: phase of the study in which the subject is enrolled
*/

proc import datafile="C:\Users\abatorsk\Documents\My SAS Files\Comp\A2018\ad.csv"
	out=ad
	dbms=CSV replace;
	getnames=yes;
run;

* Raname variables according to type to easily list in regression model;

proc sql noprint;
	*rename variable to ROInumber;
	select strip(name)||'=ROI'||strip(put(varnum,8.)) into :renamelist separated by ' '
	from dictionary.columns
	where libname='WORK' and memname='AD' and 3<=varnum<=110;
	%put renamelist=&renamelist;

	*add label from variable name;
	select 'ROI'||strip(put(varnum,8.))||"='"||strip(name)||"'" into :labellist separated by ' '
	from dictionary.columns
	where libname='WORK' and memname='AD' and 3<=varnum<=110;
	%put labellist=&labellist;
quit;

data ad;
	set ad;
	rename &renamelist;
	label &labellist;
run;

* Part A;
* report how many subjects were enrolled in each phase of the study and their corresponding disease status;
title1 "Disease status of subject by Study Phase";
proc freq data=ad; table phase*disease/missing plots=mosaic plot chisq cmh;run;
title1;
* assess graphically if there is evidence of strong batch effects in any image ROI biomarkers caused by enrolling in different phases;

* Part B;

proc logistic data=ad order=data plots=effect(polybar x=disease);
	class gender SNP1 Phase / param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 Phase / scale=none aggregate;  * can use technique=newton to use newton-raphson optimization instead of fisher scoring;
	oddsratio SNP1;
run;

proc logistic data=ad order=data plots=effect(polybar x=disease);
	class gender SNP1 Phase / param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 Phase / scale=none aggregate unequalslopes=SNP1;  * proportional odds assumption does not hold;
	oddsratio SNP1;
run;

%macro propodds;
proc sort data=ad;
	by disease;
run;
%do snpnum=1 %to 10;
title "Part B. Proportional Odds Model for SNP&snpnum";
proc logistic data=ad order=data;
	class gender phase SNP&snpnum/param=ref;
	model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase /scale=none aggregate;
	oddsratio SNP&snpnum;
run;
title;
%end;
%mend;
%propodds;

* use proc genmod to get ORs output
* NOTE: proc genmod does not output test of proportional odds assumption; 
%macro propodds;
proc sort data=ad;
	by disease;
run;
%do snpnum=1 %to 10;
title "Part B. Proportional Odds Model for SNP&snpnum";
proc genmod data=ad descending;
	class gender phase SNP&snpnum/param=ref;
	model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase /dist=multinomial link=cumlogit aggregate;
	estimate 'LOG OR' SNP&snpnum 1/exp;
run;
title;
%end;
%mend;
%propodds;


* Part C: Use probit and complementary log-log links and report "goodness of fit" log likelihood;

proc logistic data=ad order=data plots=effect(polybar x=disease);
	class gender SNP1 Phase / param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 Phase / scale=none aggregate link=probit;
	oddsratio SNP1;
run;

proc logistic data=ad order=data plots=effect(polybar x=disease);
	class gender SNP1 Phase / param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 Phase / scale=none aggregate link=cloglog;
	oddsratio SNP1;
run;

/* other ways to run the models in proc genmod 
proc genmod data=SNP10 order=data;
	title2 'Proportional Odds Logistic Regression model';
	class gender (ref='2') phase (ref='1')/param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=cumlogit aggregate;
	estimate 'LOGOR' snp1 1/exp;
run;

proc genmod data=SNP10 order=data;
	title2 'Probit Ordinal Regression model';
	class gender (ref='2') phase (ref='1')/param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=cumprobit aggregate;
run;

proc genmod data=SNP10 order=data;
	title2 'Cumulative Log Log Ordinal Regression model';
	class gender (ref='2') phase (ref='1')/param=ref;
	model disease = SNP1 age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=ccll aggregate;
run;
*/

%macro linkmodelfit;
proc sort data=ad;
	by disease;
run;
%do snpnum=1 %to 10;
title "Part C.";
	title2 "Logit Ordinal Regression Model for SNP&snpnum";
	ods output ModelFit=logit_fit_snp&snpnum;
	proc genmod data=ad order=data;
		class gender (ref='2') phase/param=ref;
		model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=cumlogit aggregate;
	run;
	title2;

	title2 "Probit Ordinal Regression Model for SNP&snpnum";
	ods output ModelFit=probit_fit_snp&snpnum;
	proc genmod data=ad order=data;
		class gender (ref='2') phase/param=ref;
		model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=cumprobit aggregate;
	run;
	title2;

	title2 "Complementary Log Log Ordinal Regression Model for SNP&snpnum";
	ods output ModelFit=cll_fit_snp&snpnum;
	proc genmod data=ad order=data;
		class gender (ref='2') phase/param=ref;
		model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase/dist=multinomial link=ccll aggregate;
	run;
	title2;
title;

proc sort data=logit_fit_snp&snpnum; by criterion; run;
proc sort data=probit_fit_snp&snpnum; by criterion; run;
proc sort data=cll_fit_snp&snpnum; by criterion; run;

data fit_snp&snpnum;
	merge logit_fit_snp&snpnum (in=inlog rename=(df=logit_df value=logit_value valuedf=logit_valuedf))
			probit_fit_snp&snpnum (in=inprobit rename=(df=probit_df value=probit_value valuedf=probit_valuedf))
			cll_fit_snp&snpnum (in=incll rename=(df=cll_df value=cll_value valuedf=cll_valuedf));
	by criterion;
run;
%end;
%mend;
%linkmodelfit;


data fit_all;
	set fit_snp1 (in=snp1)
		fit_snp2 (in=snp2)
		fit_snp3 (in=snp3)
		fit_snp4 (in=snp4)
		fit_snp5 (in=snp5)
		fit_snp6 (in=snp6)
		fit_snp7 (in=snp7)
		fit_snp8 (in=snp8)
		fit_snp9 (in=snp9)
		fit_snp10 (in=snp10);
	if snp1 then snp=1;
	if snp2 then snp=2;
	if snp3 then snp=3;
	if snp4 then snp=4;
	if snp5 then snp=5;
	if snp6 then snp=6;
	if snp7 then snp=7;
	if snp8 then snp=8;
	if snp9 then snp=9;
	if snp10 then snp=10;
	label snp='SNP'
			logit_value='Logit'
			probit_value='Probit'
			cll_value='Complementary Log-Log';
run;

proc sql;
	title 'Model Fit by Log Likelihood';
	select snp, logit_value, probit_value, cll_value
	from fit_all
	where criterion='Log Likelihood';
	title;

	title 'Model Fit by AIC';
	select snp, logit_value, probit_value, cll_value
	from fit_all
	where criterion='AIC (smaller is better)';
	title;

	title 'Model Fit by Deviance';
	select snp, logit_value, probit_value, cll_value
	from fit_all
	where criterion='Deviance';
	title;
quit;

* Part D: assess goodness of fit using psuedo R squared statistics - use Cox Snell, Nagelkerke, McFadden;

%macro propodds_R2;
proc sort data=ad;
	by disease;
run;
%do snpnum=1 %to 10;
title "Part D. Pseudo R-squred for Proportional Odds Model for SNP&snpnum";
ods output RSquare=rsquare_snp&snpnum;
proc logistic data=ad order=data;
	class gender phase SNP&snpnum/param=ref;
	model disease = SNP&snpnum age gender gPC1 gPC2 gPC3 gPC4 gPC5 phase /rsq scale=none aggregate;
run;
title;
%end;
%mend;
%propodds_R2


data rsquare_all;
	set rsquare_snp1 (in=snp1)
		rsquare_snp2 (in=snp2)
		rsquare_snp3 (in=snp3)
		rsquare_snp4 (in=snp4)
		rsquare_snp5 (in=snp5)
		rsquare_snp6 (in=snp6)
		rsquare_snp7 (in=snp7)
		rsquare_snp8 (in=snp8)
		rsquare_snp9 (in=snp9)
		rsquare_snp10 (in=snp10);
	if snp1 then snp=1;
	if snp2 then snp=2;
	if snp3 then snp=3;
	if snp4 then snp=4;
	if snp5 then snp=5;
	if snp6 then snp=6;
	if snp7 then snp=7;
	if snp8 then snp=8;
	if snp9 then snp=9;
	if snp10 then snp=10;
	label snp='SNP'
			nvalue1="R-squared"
			nvalue2="Max-rescaled R-Square";
run;

proc sql;
	title "Pseudo R-squared for Proportional Odds Models";
	select snp, nvalue1, nvalue2
	from rsquare_all;
	title;
quit;

* Part E: Perform model selection on all variables using disease as multi-categorical outcome;

* use lasso in hpgenselect;
proc hpgenselect data=ad lassosteps=50;
	class gender phase snp1 snp2 snp3 snp4 snp5 snp6 snp7 snp8 snp9 snp10/param=ref;
	model disease(ref='AD') = roi3-roi110 snp1-snp10 age gender gPC1-gPC5 phase/distribution=multinomial link=glogit ;
	*partition fraction(validate=0.33 seed=1234); 
	selection method=lasso(choose=AIC) details=all;
run;

* Part F: Get estimated vector of regression parameters for variables selected in part E by running model on phase 1 subjects. Then predict disease status of phase 2/3 subjects and compare to actual values;

title1 "Part F. i. Run model selected from Part E on Phase 1 subjects";
proc logistic data=ad outmodel=phase1_model;
	where phase=1;
	class gender SNP1 SNP2 SNP3 SNP4 SNP5 SNP6 /*SNP7 SNP8*/ SNP9 /param=ref;
	model disease(ref='NL') = SNP1 SNP2 SNP3 SNP4 SNP5 SNP6 /*SNP7 SNP8*/ SNP9 Age Gender/link=glogit;
	score out=score1;
run;
title1;

* use proc freq to see to check prediction accuracy;
proc freq data=Score1; 
	table F_disease*I_disease / nocol nocum nopercent; 
run;

* create data set with only phase 2/3 subjects;
data phase23;
	set ad;
	where phase in(2,3);
run;

title1 "Use Model from Phase 1 to Predict Phase 2/3";
proc logistic noprint inmodel=phase1_model;
	score data=phase23 out=ScoredPhase23;
run;
title1;

* use proc freq to see to check prediction accuracy;
proc freq data=ScoredPhase23; 
	table F_disease*I_disease / nocol nocum nopercent; 
run;

** Part F ii. Remove subjects with Disease=MCI and repeat part i using logistic regression;

data nomci;
	set ad;
	where disease ne 'MCI';
run;

title1 "Part F. ii. Run model from Part E on Phase 1 subjects, no MCI";
proc logistic data=nomci outmodel=phase1_logisticmodel;
	where phase=1;
	class gender SNP1 SNP2 SNP3 SNP4 SNP5 SNP6 SNP7 SNP8 SNP9 /param=ref;
	model disease(ref='NL') = SNP1 SNP2 SNP3 SNP4 SNP5 SNP6 SNP7 SNP8 SNP9 Age Gender/link=glogit;
	score out=scorelogistic;
run;
title1;

* create phase 2/3 data with no mci;
data nomci23;
	set nomci;
	where disease ne 'MCI';
run;

title1 "Part F. ii. Use Model from Phase 1 to Predict Phase 2/3 Disease State";
proc logistic noprint inmodel=phase1_logisticmodel;
	score data=nomci23 out=ScoredPhase23logistic;
run;
title1;

* check prediction accuracy; 
proc freq data=ScoredPhase23logistic; 
	table F_disease*I_disease / nocol nocum nopercent; 
run;

** Part G: Build a model using another machine learning method (random forest) and repeat Part F.i.
	Treat diseae status as polytomous {NL, MCI, AD};

proc hpforest data=ad;
	input roi3-roi110 snp1-snp10 age gender gPC1-gPC5;
	target disease;
	ods output fitstatistics=fitstats;
run;

* select top 7 variables and re-do part F.i.;

title1 "Part G. Run model selected from Random Forest on Phase 1 subjects";
proc logistic data=ad outmodel=phase1_forest;
	where phase=1;
	class SNP4 SNP9/param=ref;
	model disease(ref='NL') = Age ROI99 ROI30 ROI41 ROI44 SNP9 SNP4/link=glogit;
	score out=score1forest;
run;
title1;

* use proc freq to see to check prediction accuracy;
proc freq data=Score1forest; 
	table F_disease*I_disease / nocol nocum nopercent; 
run;

title1 "Part G. Use Random Forest Model from Phase 1 to Predict Phase 2/3";
proc logistic noprint inmodel=phase1_forest;
	score data=phase23 out=ScoredPhase23forest;
run;
title1;

* use proc freq to see to check prediction accuracy;
proc freq data=ScoredPhase23forest; 
	table F_disease*I_disease / nocol nocum nopercent; 
run;
