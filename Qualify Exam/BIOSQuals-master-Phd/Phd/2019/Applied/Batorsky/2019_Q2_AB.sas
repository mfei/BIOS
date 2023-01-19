/*
Year 2019 Problem 2
Input: pilot.csv
Variables: weeks: 0 (baseline), 2, 4, 6, 8; totchol = continuous variable total cholesterol
	id: patient ID; group: E = experimental, C = usual care control
*/

* read in data - this is in long format;
proc import datafile="C:\Users\abatorsk\Documents\My SAS Files\Comp\A2019\pilot.csv"
	out=pilot
	dbms=CSV;
run;

* put in wide format;

proc transpose data=pilot out=pilotwide1 prefix=totchol;
    by id;
    id weeks;
    var totchol;
run;

proc transpose data=pilot out=pilotwide2 prefix=group;
    by id;
    id weeks;
    var group;
run;

data pilotw;
	merge pilotwide1(drop=_name_)  pilotwide2(drop=_name_);
	by id;
	drop group2 group4 group6 group8;
	label totchol0="Week 0" totchol2="Week 2" totchol4="Week 4" totchol6="Week6" totchol8="Week 8";
	length group $15;
	if group0 = "C" then group = "Control";
	if group0 = "E" then group = "Experimental";
	drop group0;
	* create dichotomous variable for whether totchol is equal to or greater than 200 at each visit;
	if totchol0 => 200 then elevated0=1;
		else elevated0 =0;
	if totchol2 => 200 then elevated2=1;
		else elevated2 =0;
	if totchol4 => 200 then elevated4=1;
		else elevated4 =0;
	if totchol6 => 200 then elevated6=1;
		else elevated6 =0;
	if totchol8 => 200 then elevated8=1;
		else elevated8 =0;
run;

* create long data set which includes dichotomous variable for elevated cholesterol;
data elong;
	set pilotw;
	y=elevated0; weeks=0; output;
	y=elevated2; weeks=2; output;
	y=elevated4; weeks=4; output;
	y=elevated6; weeks=6; output;
	y=elevated8; weeks=8; output;
	drop elevated0 elevated2 elevated4 elevated6 elevated8 totchol0 totchol2 totchol4 totchol6 totchol8;
  label y      = "Elevated cholesterol 0=no 1=yes";
run;

*** Part A: Descriptive statistics and summary of estimates needed for sample size calculations;
ods noproctitle;
ods rtf file="C:\Users\abatorsk\Documents\My SAS Files\Practice\2019 Output\Q2PartA.rtf";
title1 "Descriptive Statistics for AIM 1";
proc tabulate data=pilotw;
   class group;
   var totchol0 totchol2 totchol4 totchol6 totchol8;
   table group, (totchol0 totchol2 totchol4 totchol6 totchol8) * (n mean median var) ;
   label totchol0="Week 0 Cholesterol" totchol2="Week 2 Cholesterol" totchol4="Week 4 Cholesterol" totchol6="Week 6 Cholesterol" totchol8="Week 8 Cholesterol"
	group="Treatment";
run;
title1;

* time plot;
proc means data=pilot n mean var nway noprint;
	var totchol;
	class weeks group;
	output out=pilotp mean=mean;
run;

goptions reset=all;
	symbol1 value=circle color=black interpol=join;
	symbol2 value=square color=red interpol=join;

title1 "Time Plot of Mean Cholesterol(mg/dL) for Subjects Across 8 Weeks by Group";
proc gplot data=pilotp;
	plot mean*weeks=group;
run;
title1;

title "Plot of subject-specific levels of total cholesterol over 8 weeks";
proc sgplot data=pilot;
   series x=weeks y=totchol / group=id groupLC=group curvelabel;
run;
title;

proc sort data=pilot out=pilot2; by group; run;
proc corr data=pilot2 out=covmxs(type=cov) nocorr cov noprint;
	var totchol;
	by group;
run;
proc print data=covmxs;
 run;

* Descriptive Stats for AIM2;
title1 "Descriptive Statistics for AIM2";
proc means data=pilotw maxdec=2 n mean var;
	var elevated0 elevated2 elevated4 elevated6 elevated8;
	class group;
	label elevated0="High Cholesterol Week 0" elevated2="High Cholesterol Week 2" elevated4="High Cholesterol Week 4" 
		elevated6="High Cholesterol Week 6" elevated8="High Cholesterol Week 8";
run;
title1;

ods rtf close;

*** Part B - Determine sample size needed for AIM 1
				H0: beta3 = 0 vs H1: beta3 = -3
			power = 0.85
			alpha = 1-sided 0.025
;

* use residual variance, not g11;
title1 "Part B: LMM: weeks + week*group, random intercept, type=un";
proc mixed data=pilot method=reml covtest cl noclprint=10;
	class id group(ref="C");
	model totchol = weeks weeks*group / s chisq cl;
	repeated / subject = id;
	random intercept / subject=id type=un g gcorr;
run;
title1;

title1 "Part B: LMM: weeks + weeks*group, random intercept and slope, type=un";
proc mixed data=pilot method=reml covtest cl noclprint=10;
	class id group(ref="C");
	model totchol = weeks weeks*group / s chisq cl;
	repeated / subject = id;
	random intercept weeks/ subject=id type=un g gcorr;
run;
title1;

%let alpha = 0.025; /* Set alpha value */
%let mu = 0;	   /* Set mean value */
%let sigma = 1;	   /* Set st. dev value */
%let power = 0.85; /* Set power level where power = 1 - gamma (type II error) */
* determine Z values for power; 
data z;
   	criticalz = quantile('normal', 1-&alpha, &mu, &sigma);
	powerz = quantile('normal', &power, &mu, &sigma);	
run;
proc print data=z;run;

*** Part C - Determine sample size needed for AIM 2 - use ss06.sas code from 767
				H0: gamma3 = 0 vs H1: gamma3 = -0.15
			power = 0.85
			alpha = 1-sided 0.025
;
/* got warning: iteration limit exceeded
title1 "Part C: GEE weeks + weeks*group, unstructured correlation structure ";
proc genmod descending data=elong;
  class id group/ param=ref;
  model y = group weeks weeks*group / dist=binomial link=logit scale=1 noscale type3 wald;
  repeated subject=id / corrw type=un;
  run;
title1;
*/
title1 "Part C: GEE weeks + weeks*group, exchangeable correlation structure ";
proc genmod descending data=elong;
  class id group (ref="Control")/ param=ref;
  model y = group weeks weeks*group / dist=binomial link=logit scale=1 noscale type3 wald;
  repeated subject=id / corrw type=exch;
  run;
title1;

* does  - Sample size calculations
        - Marginal models for dependent binary responses
        - Using proc genmod as an asymptotic variance calculator
***************************************************;
title1 "Sample Size for 2019 Qual Exam Q2 Part D: genmod as an asymptotic variance calculator";
***************************************************;
data A;
  one   = 1;
  p0    = 0.87;     * E[Y] at time t=0;
  p1    = 0.33;    * E[Y] at time t=5 in the experimental treatment group;
  p2	= 0.60;		* E[Y] at time t=5 in the control treatment group;
  beta1 = log(p0/(1-p0));
  beta2 = ((log(p2/(1-p2)))-(log(p0/(1-p0))))/8;    * time trend in the control (standard of care) treatment group;
  beta3 = -0.15; *(log(p1/(1-p1)) - log(p0/(1-p0))) / 8;  * H1;
  put p0= p1= beta1= beta2= beta3= ;

  do id = 1 to 2;
    trt = id - 1;  
    do t = 0 to 8 by 2;
      eta_0 = beta1 + beta2 * t                  ;  * H0: beta3 = 0;
      mu_0  = 1 / (1 + exp(-eta_0));                * H0;
      eta_1 = beta1 + beta2 * t + beta3 * trt * t;  * H1: beta3=-0.15;
      mu_1  = 1 / (1 + exp(-eta_1));                * H1;
      output;
    end;
  end;

  label t     = "Time (weeks)";
  label trt   = "Treatment (0=control, 1=experimental)";
  label beta3 = "Difference in slopes (experimental treatment - control treatment)";
  label mu_0  = "The mean under H0";
  label mu_1  = "The mean under H1";

  run;

proc print data = A;
  var id trt t mu_0 mu_1 beta1 beta2 beta3;
  run;

***************************************************;

%let rho = 0.46;          * The correlation;
%let lp  = t  t*trt;     * The linear predictor in genmod;

title2 "H0: parameter for change in log odds over time for experimental group is zero";
proc genmod data=A;
  class id;
  model mu_0 / one  = &lp  / d=b scale=1 noscale;
  repeated subject = id / modelse
    type=user(
              1    &rho &rho &rho &rho,
              &rho 1    &rho &rho &rho,
              &rho &rho 1    &rho &rho,
              &rho &rho &rho 1    &rho,
              &rho &rho &rho &rho 1
            );
  run;

title2 "H1: parameter for change in log odds over time for experimental group is -0.15";
proc genmod data=A;
  class id;
  model mu_1 / one  = &lp  / d=b scale=1 noscale;
  repeated subject = id / modelse
    type=user(
              1    &rho &rho &rho &rho,
              &rho 1    &rho &rho &rho,
              &rho &rho 1    &rho &rho,
              &rho &rho &rho 1    &rho,
              &rho &rho &rho &rho 1
            );
  run;
title1;
title2;
