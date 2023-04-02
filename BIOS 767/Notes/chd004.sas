%let progname = chd004.sas;
ods pdf file = "chd004.pdf";
* input:  chd.dat
* xref:
     Chapter 11, Section 11.3.2
     Study of Risk Factors for Coronary Heart Disease (CHD)
     Loglinear (Poisson)  Regression Model
     Logistic  (Binomial) Regression Model
***************************************************;
filename INF "chd.dat";
title1 "&progname: Risk Factors for Coronary Heart Disease (CHD)";
***************************************************;
data A;
  infile INF firstobs=2;
  retain id 0;  * create a "subject id";
  id + 1;
  input smoking bp personality  cases py;
  lpy  = log(py) ; * log denominator;

  label
  smoking     = "Smoking (0/10/20/30 0/1-10/11-20/30+ cig/day)"
  bp          = "Blood pressure (0:<140, 1:>=140 mm Hg systolic)"
  personality = "Behavior (0=Type B, 1=Type A)"
  cases       = "Number of cases of CHD"
  py          = "Person-years (denominator)"
  lpy         = "Log person-years"
  ;
  run;

***************************************************;

proc genmod data = A;
  model cases = smoking personality bp / d = poisson offset = lpy;
  title2 "1. Poisson loglinear (Table 11.5)";
  run;

proc genmod data = A;
  model cases / py = smoking personality bp / d = binomial;
  title2 "2. Binomial logistic";
  run;

***************************************************;

ods _all_ close;
