%let progname = chd001.sas;
ods pdf file = "chd001.pdf";
* input:  chd.dat
* xref:
     Study of Risk Factors for Coronary Heart Disease (CHD)
     Loglinear (Poisson) Regression Model
     Plots
***************************************************;
filename INF "chd.dat";
title1 "&progname: Risk Factors for Coronary Heart Disease (CHD)";
ods graphics on;
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

title2 "1.a Poisson model, Smoking + Personality + BP";

proc genmod data = A;
  model cases = smoking personality bp 
    / d = poisson offset = lpy covb;

  contrast 'Wald test of beta_3 = 0'
     personality 1
    / wald 
  ;
  contrast 'Likelihood ratio test of beta_3 = 0'
     personality 1
  ;
  contrast 'Wald test of (beta_2, beta_4) = (0, 0)'
    smoking     1,
    bp          1
    / wald 
  ;
  contrast 'Likelihood ratio test of (beta_2, beta_4) = (0, 0)'
     smoking     1,
     bp          1
  ;
  run;

***************************************************;
title2 "1.b Poisson model, Smoking + BP ";
proc genmod data = A   plots = all;
  model cases = smoking bp / d = poisson offset = lpy;
  run;

***************************************************;
title2 "1.c Poisson model, Personality";
proc genmod data = A   plots = all;
  model cases = personality / d = poisson offset = lpy;
  run;

***************************************************;

title2 "2. Poisson model, Smoking + Personality + BP";
proc genmod data = A   plots = all;
  model cases = smoking personality bp 
    / d = poisson offset = lpy;
  output out = B115  resraw=resraw reschi=reschi stdreschi=stdreschi
         pred=fitted xbeta=xbeta stdxbeta=stdxbeta
    ;
  run;
***************************************************;

data B115;
  set  B115;
  xbeta = xbeta - lpy;       * SAS includes the offset in XBETA;
  rate = exp(xbeta);         * or: rate = fitted / py;
  lo95 = exp(xbeta - 1.96 * stdxbeta);
  up95 = exp(xbeta + 1.96 * stdxbeta);
  p0   = exp(-rate);         * Poisson probability of 0;
  p1   = exp(-rate) * rate;  * Poisson probability of 1;
  pa1  = 1 - (p0 + p1);      * Poisson probability of >1;
  label p0 = "P(0  events in 1 person-year";
  label p1 = "P(1  event  in 1 person-year";
  label pa1= "P(>1 event  in 1 person-year";

run;
***************************************************;

proc print data = B115;
  var smoking bp personality py cases fitted resraw reschi stdreschi;
  sum py cases fitted resraw;
  run;
***************************************************;

proc print data = B115;
  var smoking bp personality rate lo95 up95 p0 p1 pa1;
  run;
***************************************************;

ods graphics off;
ods _all_ close;
