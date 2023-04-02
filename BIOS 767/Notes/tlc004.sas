%let progname = tlc004.sas;
* input: tlc.dat
* output:
* xref: 
* does: Large-sample, unequal variance,  two-sample test
        Profile analysis
        Treatment of Lead Exposed Children Trial (TLC)
        Asymptotic two-sample test using proc iml
       
*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF "tlc.dat";
ods pdf file = "tlc004.pdf";

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
start ls2(y1, y2, theta);
 * Large-sample two-sample multivariate test;
 * y1, y2 = data matrices, theta = column vector;

  n1 = nrow(y1);
  n2 = nrow(y2);
  d = ncol(y1);
  ybar1 = t(mean(y1));
  ybar2 = t(mean(y2));
  r  = ybar1 - ybar2 - theta;
  s1 = cov(y1);
  s2 = cov(y2);
  v = s1 / n1 + s2 / n2;
  x2 =  t(r) * solve(v, r);       * ~ X^2(d) under H0, large samples;
  x2_df = d;
  pval   = 1 - probchi(x2, x2_df);

  print "Large-sample two-sample test";
  print n1 n2 d;
  print ybar1  s1;
  print ybar2  s2;
  print "X^2:" ,  x2  x2_df;
  print pval;

finish;
******************************************************;

* IML execution starts here;

use A;

print "Group: 1=A, 2=P";
print "Change from baseline at 1, 4, 6 weeks";
read all var  {dif1 dif4 dif6} into y1 where (group = "A");
read all var  {dif1 dif4 dif6} into y2 where (group = "P");
run ls2(y1, y2, j(3, 1, 0));

print "Group: 1=A, 2=P";
print "Lead levels at 1, 4, 6 weeks";
read all var  {lead1 lead4 lead6} into y1 where (group = "A");
read all var  {lead1 lead4 lead6} into y2 where (group = "P");
run ls2(y1, y2, j(3, 1, 0));

print "Group: 1=A, 2=P";
print "Lead levels at 0, 1, 4, 6 weeks";
read all var  {lead0 lead1 lead4 lead6} into y1 where (group = "A");
read all var  {lead0 lead1 lead4 lead6} into y2 where (group = "P");
run ls2(y1, y2, j(4, 1, 0));

******************************************************;

