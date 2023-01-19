/*
Year 2019 Problem 3
Input: illness1.dat (long format), illness2.dat (wide format)
Variables: Week: 0 (baseline), 1, 3, 6; Outcome: 1 = moderately or severely ill, 0 = normal to mildly ill
	treatment: 0 = placebo, 1 = drug
Derived variables: t_ij is the square root of week where t = 0, 1, sqrt(3), sqrt(6)

Instructions: 	use likelihood ratio tests for hypothesis testing, otherwise Wald type tests
				use quadrature of 25
*/

* read in data;
data ilong;
	infile "C:\Users\abatorsk\Documents\My SAS Files\Comp\A2019\illness1.dat" firstobs=2;
	input id week treatment illness;
	t = sqrt(week);
run;

data iwide;
	infile "C:\Users\abatorsk\Documents\My SAS Files\Comp\A2019\illness2.dat" firstobs=2;
	input id treatment illness0 illness1 illness3 illness6;
run;

*** Part A: Present relevant summaries, tabular and/or graphical and some brief comments;
* descriptive statistics using long data;
proc means data=iwide n nmiss mean var nway maxdec=2;
	var illness0 illness1 illness3 illness6;
	by treatment;
run;

* example code for descriptive statistics using proc tabulate;
proc tabulate data=iwide;
   class treatment;
   var illness0 illness1 illness3 illness6;
   table (illness0 illness1 illness3 illness6), treatment * (n mean median qrange std) ;
   label illness0="Baseline" illness1="Week 1" illness3="Week 3" illness6="Week 6";
run;

* create time plot using wide data;
proc means data=ilong n mean var nway;
	var illness;
	class treatment week;
	output out=timeplotdata mean=mean;
run;

goptions reset=all;
	symbol1 value=circle color=black interpol=join;
	symbol2 value=square color=red interpol=join;

title1 Time Plot of Probability of Moderate/Severe Illness vs Mild/None;
title2 Placebo vs Drug Treatment;
proc gplot data=timeplotdata;
	plot mean*week=treatment;
run;
title1;
title2;

*** Part B: Without explicitly using regression methods carry out asymptotic 2-sample test of null hypothesis
	of no effect of drug compared to placebo. Do not assume common cov matrix between groups; 
*code uses wide data set;

* get covariance matrix for outcomes by treatment group;
proc sort data = iwide; by treatment; run;
proc corr data = iwide  cov;
  var illness0 illness1 illness3 illness6;
  by treatment;
run;

/* Hotelling's is not an appropriate method to use for this question because:
	1. data need to be multivariate normal
	2. you need to assume common cov matrices

NOTES ON HOW TO SOLVE (incomplete)

estimate cov matrix of difference between X_bar and Y_bar (these are the vectors of outcome means of 2 groups?)
use formula S1/N1 - S2/N2
distribution follows chi square with p degrees of freedom (is p=number of repeated outcomes?)

data A;
	set iwide;
	dif1=illness1-illness0;
	dif3=illness3-illness0;
	dif6=illness6-illness0;
run;
proc sort data=A; by treatment;run;

proc iml ;

***************************************************;
start hot2(y1, y2, theta);
 * Hotelling's T^2 two-sample multivariate test;
 * y1, y2 = data matrices, theta = column vector;

  n1 = nrow(y1);
  n2 = nrow(y2);
  d  = ncol(y1);
  ybar1 = t(mean(y1));
  ybar2 = t(mean(y2));
  r  = ybar1 - ybar2 - theta;
  s1 = cov(y1);
  s2 = cov(y2);
  sp = (n1 - 1) * s1 + (n2 - 1) * s2;  
  sp = sp / (n1 + n2 - 2);            * Pooled;
  a  = solve(sp, r);
  n  = 1 / (1 / n1 + 1 / n2);
  t2 = n * (t(r) * a);                * ~ T^2(d, n1+n2-2) under H0;
  t2_df1 = d;
  t2_df2 = n1 + n2 - 2;
  f      = t2 * (n1 + n2 - d - 1) / (d * (n1 + n2 - 2)); * ~ F(d, n1+n2-d-1) under H0;
  f_df1  = d;
  f_df2  = n1 + n2 - d - 1;
  pval   = 1 - probf(f, f_df1, f_df2);
  maxroot = t2 / (n1 + n2 - 2);

  print "Hotelling's T^2 two-sample test";
  print n1 n2 d;
  print ybar1  s1;
  print ybar2  s2;
  print "T^2:" ,  t2  t2_df1 t2_df2;
  print "F:"   ,  f   f_df1  f_df2;
  print pval;
  print "The max eigenvalue of E^{-1} H: "  maxroot;

  print "Coefficients:", a  [format=16.10];

finish;

use A;

print "Group: 1=drug, 0=placebo";
read all var  {dif1 dif3 dif6} into y1 where (treatment = 1);
read all var  {dif1 dif3 dif6} into y2 where (treatment = 0);
run hot2(y1, y2, j(3, 1, 0));
quit;
*/

*** Part C: Fit the model M1:

logit E[Y_ij | b_1i] = beta1 + b_1i + beta2*t_ij + beta3*t_ij*x_i
i = 1...K		j = 1...4		b_1i ... b_1k are unobserved random variables distributed as iid N(0,sigma^2)
assume Y_i1...Y_i4 are conditionally independent given b_1i
Present parameter and empirical (robust, sandwich) SE estimates and 95% confidence intervals

note: t = square root of week
;
title1 "Part C: GLMM: sqrt(week) + sqrt(week)*treatment, random intercept, type=un";
proc glimmix data=ilong method=quad(qpoints=25) noclprint=10;
	class id treatment(ref="0");
	model illness = t t*treatment / dist=binomial link=logit s cl;
	random intercept / subject=id type=un;
run;
title1;

* get confidence intervals for sigma squared (g11) - estimate of b1i;
data c;
	set ilong;
	t=sqrt(week);
	txtrt=t*treatment;
run;

proc nlmixed data=c qpoints=25 emperical;
	parms beta1 = 4 beta_t = -2 beta_txtrt = -2 sigma2 = 1;
	eta  = beta1 + beta_t*t + beta_txtrt*txtrt + u;
	p = 1/(1+exp(-eta));
	model illness ~ binary(p);
	random u ~ normal(0, sigma2) subject=id;
run;

*** Part E: Fit the model M2:

logit E[Y_ij | b_1i, b_2i] = beta1 + b_1i + beta2*t_ij + beta3*t_ij*x_i + b_2i*t_ij

where b_i=(b_1i, b_2i)' i = 1... K are are unobserved random variables distributed as iid N(0,G)
;
title1 "Part E: GLMM: sqrt(week) + sqrt(week)*treatment, random intercept and slope, type=un";
proc glimmix data=ilong method=quad(qpoints=25) noclprint=10;
	class id treatment(ref="0");
	model illness = t t*treatment / dist=binomial link=logit s cl;
	random intercept t/ subject=id type=un;
run;
title1;

proc nlmixed data=c qpoints=25 emperical;
	parms beta1 = 4 beta_t = -2 beta_txtrt = -2 sigma11 = 2 sigma21 = 0.1 sigma22 = 0.2;
	eta  = beta1 + beta_t*t + beta_txtrt*txtrt + u1 + u2*t;
	p = 1/(1+exp(-eta));
	model illness ~ binary(p);
	random u1 u2 ~ normal([0,0], [sigma11, sigma21, sigma22]) subject=id;
run;

*** Part H: Test null hypothesis of g22=0 vs g22!=0;

proc iml;
pvalue1 = 1 - cdf("ChiSq", 3.63, 1);
pvalue2 = 1 - cdf("ChiSq", 3.63, 2);
pvalue = (pvalue1+pvalue2)/2;
print pvalue1 pvalue2 pvalue;
quit;

*** Part I: Fit the model M3:

logit E[Y_ij] = gamma1 + gamma2*t_ij + gamma3*t_ij*x_i

Model the correlation among Y_ij's within a subject via the pairwise log odds ratios, alpha1-alpha6. 
Allow alpha1-alpha6 to differ between each other but be the same regardless of treatment group. 

Test the null hypothesis of no treatment effect. 
;
title1 "Part I: GLM: sqrt(week) + sqrt(week)*treatment, logor=fullclust";
proc genmod descending data=ilong;
  class id treatment(ref="0");
  model illness = t t*treatment / dist=binomial link=logit type3 wald;
  repeated subject=id / logor=fullclust;
run;
title1;


*** Part J: Test the null hypothesis that alpha1=...=alpha6 vs the alternative;

title1 "Part I: GLM: sqrt(week) + sqrt(week)*treatment, logor=exch";
proc genmod descending data=ilong;
  class id treatment(ref="0");
  model illness = t t*treatment / dist=binomial link=logit type3 wald;
  repeated subject=id / logor=exch;
run;
title1;
