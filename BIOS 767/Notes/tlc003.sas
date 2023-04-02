%let progname = tlc003.sas;
* input: tlc.dat
* output:
* xref: tlc001.sas
* does: Large-sample
        Profile analysis
        Treatment of Lead Exposed Children Trial (TLC)
        One-sample asymptotic test
       H0: constant mean over time    
   
*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF   "tlc.dat";
ods pdf file = "tlc003.pdf";

*******************************************************************;

data A;
  infile INF  firstobs=2;
  input id group $ lead0 lead1 lead4 lead6;
  dif1 = lead1 - lead0;
  dif4 = lead4 - lead0;
  dif6 = lead6 - lead0;

  run;

***********************************************;

title3 "proc IML";

proc iml ;

***************************************************;
start ls1(y, mu0);
 * Large-sample one-sample multivariate test;
 * y = data matrix, mu0 = column vector;

  n = nrow(y);
  d = ncol(y);
  ybar = t(mean(y));
  r = ybar - mu0;
  s = cov(y);
  a = solve(s, r);
  x2 = n * (t(r) * a);          * ~ X^2(d) under H0, large n;
  x2_df = d;
  pval = 1 - probchi(x2, x2_df);

  print "Large sample-based one-sample test";
  print n d;
  print ybar s;
  print "X^2:" ,  x2  x2_df ;
  print pval;

finish;
******************************************************;

* IML execution starts here;

use A;

print "Group: A";
read all var  {dif1 dif4 dif6} into y where (group = "A");
run ls1(y, j(3, 1, 0));

print "Group: P";
read all var  {dif1 dif4 dif6} into y where (group = "P");
run ls1(y, j(3, 1, 0));

******************************************************;
