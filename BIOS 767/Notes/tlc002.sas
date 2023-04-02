%let progname = tlc002.sas;
* input: tlc.dat
* output:
* xref: tlc001.sas
* does: Hotelling's T^2 - two-sample test
        Profile analysis
        Treatment of Lead Exposed Children Trial (TLC)
       
*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF "tlc.dat";
%let pdfout  = tlc002.pdf;
ods pdf file = "&pdfout";

*******************************************************************;

data A;
  infile INF  firstobs=2;
  input id group $ lead0 lead1 lead4 lead6;
  dif1 = lead1 - lead0;
  dif4 = lead4 - lead0;
  dif6 = lead6 - lead0;

  ay =
    0.3468270071 * dif1
   +0.0622951353 * dif4
   -0.0615772669 * dif6;

  run;

***********************************************;

title3 "proc IML";

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
******************************************************;

* IML execution starts here;

use A;

print "Group: 1=A, 2=P";
print "Change from baseline at 1, 4, 6 weeks";
read all var  {dif1 dif4 dif6} into y1 where (group = "A");
read all var  {dif1 dif4 dif6} into y2 where (group = "P");
run hot2(y1, y2, j(3, 1, 0));

print "Group: 1=A, 2=P";
print "Lead levels at 1, 4, 6 weeks";
read all var  {lead1 lead4 lead6} into y1 where (group = "A");
read all var  {lead1 lead4 lead6} into y2 where (group = "P");
run hot2(y1, y2, j(3, 1, 0));

print "Group: 1=A, 2=P";
print "Lead levels at 0, 1, 4, 6 weeks";
read all var  {lead0 lead1 lead4 lead6} into y1 where (group = "A");
read all var  {lead0 lead1 lead4 lead6} into y2 where (group = "P");
run hot2(y1, y2, j(4, 1, 0));

******************************************************;

proc sort data = A; by group; run;
* manova in glm;
proc glm data = A;
  class group;
  model dif1 dif4 dif6 = group;
  manova h = group ;
  title2 "proc glm";
  run;

******************************************************;

title2 "Hotelling T^2 = t^2 based on the optimal linear combination";
proc ttest data = A;
  class group;
  var   ay;
  run;

