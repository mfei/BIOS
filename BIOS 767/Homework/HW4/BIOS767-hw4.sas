/*******************************************************************
*
* Project            : BIOS 767 hw4
*
* Program name       : BIOS767_hw4.sas
*
* Author             : 
*
* Date created       : 2023-03-27
*
* Modification log   : 2023-03-28 (Modified for submission)
*
********************************************************************/
options pagesize = 10000;

libname data '/home/u63075885/BIOS767/HW4';

proc import datafile="/home/u63075885/BIOS767/HW4/monkey.dat" dbms=tab out=monkey;
	getnames=yes;
run;

proc print data=monkey;
run;

PROC MEANS DATA = cebu;
VAR momht;
OUTPUT out=out p25 = p25 p50=p50 p75=p75;
RUN;

DATA _null_;
SET out;
call symput('q2',trim(left(put(p50,8.2))));
call symput('q1',trim(left(put(p25,8.2))));
call symput('q3',trim(left(put(p75,8.2))));
RUN;
%put med = &q1;

DATA analysis;
SET cebu;
IF momht < &q1.  THEN MHC = 1;
ELSE IF &q1. <= momht < &q2.  THEN MHC = 2;
ELSE IF &q2. <= momht < &q3.  THEN MHC = 3;
ELSE MHC = 4;

time_ls_2m = max(time-1, 0);
time_ls_6m = max(time-3, 0);
RUN;

***1(b) the differences in total carnitine between the high and low fat diets after adjusting for the
baseline carnitine level ****

%macro cov(typeCov);
		ods output FitStatistics = %sysfunc(compress(&typeCov,()));
		proc mixed data = analysis method=REML PLOTS=none NOCLPRINT NOITPRINT;
			class mhc(ref="1") time(ref="0") id;
			model weight_kg = mhc time mhc*time;
			repeated time / subject=id type=&typeCov.; 
		run;
		quit;

		data %sysfunc(compress(&typeCov,()));
			set %sysfunc(compress(&typeCov,()));
			rename value = %sysfunc(compress(&typeCov,()));
				label value = "%upcase(&typeCov)";			
		run;
%mend;

%cov(typeCov=un);
%cov(typeCov=cs);
%cov(typeCov=ar(1));
%cov(typeCov=toep);


/* Merge outputs */
data FitStatistics_cov;
	merge un
		  cs
		  ar1
		  toep;
run;
/* AIC */
title "Overall Covariance Model Fit Statistics";
title2 "in Profile analysis";
proc print data=FitStatistics_cov noobs label; run;

/* Correlation matrix for UN */
		ods output FitStatistics = %sysfunc(compress(un,()));
		proc mixed data = analysis method=REML PLOTS=none NOCLPRINT NOITPRINT;
			class mhc(ref="1") time(ref="0") id;
			model weight_kg = mhc time mhc*time  ;
			repeated time / subject=id type=un R RCORR; 
		run;

*** Question 3 ***;
ods output FitStatistics = fit_lp;
		proc mixed data = analysis method=ML PLOTS=none NOCLPRINT NOITPRINT;
			class mhc(ref="1") id;
			model weight_kg = mhc time mhc*time  ;
			repeated  / subject=id type=un; 
		run;

		
/* quadratic polynomial */
ods output FitStatistics = fit_qp;
proc mixed data=analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time*time mhc*time*time / solution;
	repeated / subject=id type=un;
run;
/* cubic polynomial */
ods output FitStatistics = fit_cp;
proc mixed data=analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time*time mhc*time*time 
						  time*time*time mhc*time*time*time/ solution;
	repeated / subject=id type=un;
run;
/* linear spline with the knot at 2 month */
ods output FitStatistics = fit_lsp1;
proc mixed data=analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time_ls_2m mhc*time_ls_2m / solution;
	repeated / subject=id type=un;
run;
/* linear spline with the knots at 2 month and 6 month */
ods output FitStatistics = fit_lsp2;
proc mixed data=analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time_ls_2m mhc*time_ls_2m
						  time_ls_6m mhc*time_ls_6m / solution;
	repeated / subject=id type=un;
run;
/* Merge outputs */
data FitStatistics_mean;
	merge fit_lp(rename=(value=lp))
		  fit_qp(rename=(value=qp))
		  fit_cp(rename=(value=cp))
		  fit_lsp1(rename=(value=lsp1))
		  fit_lsp2(rename=(value=lsp2));
	by descr;
run;

/* Print results */
title "Overall Mean Model Fit Statistics";
proc print data=FitStatistics_mean noobs label; run;

**** Different covariance structures for linear spline model ***;
/* Build macro for cov model */
%macro cov2(typeCov);
		ods output FitStatistics = %sysfunc(compress(&typeCov,()))_lsp2;
proc mixed data=analysis method=reml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time_ls_2m mhc*time_ls_2m
						  time_ls_6m mhc*time_ls_6m / solution;
	repeated / subject=id type=&typeCov;
run;
		quit;
%mend;

/* Execute macro */
%cov2(typeCov=un);
%cov2(typeCov=cs);
%cov2(typeCov=ar(1));
%cov2(typeCov=toep);

/* Merge outputs */
data FitStatistics_lsp2_cov;
	merge un_lsp2(rename=(value=un))
		  cs_lsp2(rename=(value=cs))
		  ar1_lsp2(rename=(value=ar1))
		  toep_lsp2(rename=(value=toep));
	by descr;
run;

/* Print results */
title "Overall Covariance Model Fit Statistics in LSP2";
proc print data=FitStatistics_lsp2_cov noobs label; run;


/** (c) **/
/* Test Parallelism */
/* Calculate LL for full model */
ods output FitStatistics = FullModel;
proc mixed data= analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time mhc*time
						  time_ls_2m mhc*time_ls_2m
						  time_ls_6m mhc*time_ls_6m / solution;
	repeated / subject=id type=un;
run;

/* Calculate LL for reduced model */
ods output FitStatistics = ReducedModel;
proc mixed data=analysis method=ml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc(ref='1') id;
	model weight_kg = mhc time
						  time_ls_2m
						  time_ls_6m / solution;
	repeated / subject=id type=un;
run;


/* Merge outputs */
data BothModels;
	merge Fullmodel(rename=(value=full))
		  ReducedModel(rename=(value=reduced));
	by descr;
	where upcase(descr) = "-2 LOG LIKELIHOOD";
	
	LRT_stat = reduced - full;
	pValue = sdf("chisquare", LRT_stat, 9);
	full = -0.5*full;
	reduced = -0.5*reduced;
	drop descr;
run;

/* Print results */
title "Testing Parallelism";
proc print data=BothModels split="^" noobs;
	label LRT_stat = "Likelihood^Ratio^Statistic"
		  pValue = "LRT P-value"
		  full = "Full^Model^Log-Likelihood"
		  reduced = "Reduced^Model^Log-Likelihood";
	format LRT_stat full reduced 6.4 pValue pvalue6.4;
run;


/** (d) **/
/* Estimates on several time points*/
/* Note that estimated weights are equal to that in the model includes time effect.
because time effect is absorbed into mhc*time term */
proc mixed data=analysis method=reml PLOTS=none NOCLPRINT NOITPRINT;
	class mhc id;
	model weight_kg = mhc time mhc*time
						  time_ls_2m mhc*time_ls_2m
						  time_ls_6m mhc*time_ls_6m /noint solution;
	repeated / subject=id type=un;
	
	estimate "Mean Difference between MHC4 and MHC1 at 0 month" 	mhc -1 0 0 1 mhc*time 0 0 0 0 			mhc*time_ls_2m 0 0 0 0 		mhc*time_ls_6m 0 0 0 0 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 3 month" 	mhc -1 0 0 1 mhc*time -1.5 0 0 1.5 		mhc*time_ls_2m -0.5 0 0 0.5 mhc*time_ls_6m 0 0 0 0 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 6 month" 	mhc -1 0 0 1 mhc*time -3 0 0 3 			mhc*time_ls_2m -2 0 0 2 	mhc*time_ls_6m 0 0 0 0 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 9 month" 	mhc -1 0 0 1 mhc*time -4.5 0 0 4.5 		mhc*time_ls_2m -3.5 0 0 3.5 mhc*time_ls_6m -1.5 0 0 1.5 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 12 month" 	mhc -1 0 0 1 mhc*time -6 0 0 6 			mhc*time_ls_2m -5 0 0 5 	mhc*time_ls_6m -3 0 0 3 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 15 month" 	mhc -1 0 0 1 mhc*time -7.5 0 0 7.5 		mhc*time_ls_2m -6.5 0 0 6.5 mhc*time_ls_6m -4.5 0 0 4.5 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 18 month" 	mhc -1 0 0 1 mhc*time -9 0 0 9 			mhc*time_ls_2m -8 0 0 8 	mhc*time_ls_6m -6 0 0 6 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 21 month" 	mhc -1 0 0 1 mhc*time -10.5 0 0 10.5 	mhc*time_ls_2m -9.5 0 0 9.5 mhc*time_ls_6m -7.5 0 0 7.5 / cl;
	estimate "Mean Difference between MHC4 and MHC1 at 24 month" 	mhc -1 0 0 1 mhc*time -12 0 0 12 		mhc*time_ls_2m -11 0 0 11 	mhc*time_ls_6m -9 0 0 9 / cl;
run;
