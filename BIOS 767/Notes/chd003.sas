%let progname = chd003.sas;
ods pdf file = "chd003.pdf";
* input:  chd.dat
* xref:   chd001.sas
     Chapter 11, Section 11.3.2
     Study of Risk Factors for Coronary Heart Disease (CHD)
     Loglinear Regression Model
     Over-dispersion relative to Poisson
     The "repeated" trick to obtain the robust variance estimator (RVE)
***************************************************;
filename INF "chd.dat";
title1 "&progname: Risk Factors for Coronary Heart Disease (CHD)";
title2 "'repeated' statement to obtain the robust variance estimator";
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
                  *** Overdispersion ***           
***************************************************;

* Keep the same covariates;

***************************************************;
title2 "1. V(mu) = mu";
proc genmod data = A;
  model cases = smoking  personality bp 
    / d = poisson offset = lpy;
  run;
***************************************************;

title2 "2. V(mu) = phi mu";
proc genmod data = A;
  model cases = smoking  personality bp 
    / d = poisson offset = lpy pscale;
  run;
***************************************************;

title2 "3. Negative-Binomial  ";
proc genmod data = A;
  model cases = smoking  personality bp 
    / d = negbin offset = lpy;
  run;
***************************************************;

title2 "4. Conditionally Poisson, normal intercept";
proc nlmixed data = A;
      parms 
        b_1          -5.42
        b_smoking     0.027
        b_personality 0.75 
        b_bp          0.75
        logsigma      0
      ;
      eta = lpy + b_1 + b_smoking * smoking + b_personality * personality
               + b_bp * bp + e; * linear predictor;
      lambda = exp(eta);        * conditional mean;
      model cases ~ poisson(lambda);  * conditional distribution;
      random e ~ normal(0,exp(2*logsigma)) subject = id;
      run;
***************************************************;

title3 "5. Poisson with RVE";
proc genmod data = A;
  class id;
  model cases = smoking  personality bp  
    / d = poisson offset = lpy;
  repeated subject = id;
  run;

