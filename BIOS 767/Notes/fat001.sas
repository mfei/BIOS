%let progname = fat001.sas;
%let pdfout   = fat001.pdf;

* input: fat.dat
* output:
* xref:
* does:
     *  MIT Growth and Development Study, mixed models
     *  "covtest" produces asymptotic standard errors and
        Wald tests for the covariance parameters.
     *  "local" adds a term to the variance

*******************************************************************;
title "&progname: MIT Growth and Development Study, mixed models";

filename INF  "fat.dat";
ods pdf file = "&pdfout";
*******************************************************************;

data A;
  infile INF firstobs = 2;
  input id age agemen time pbf;
  time_0 = max(time, 0);

  label
    id     = "Subject ID"
    age    = "Current Age (years)"
    agemen = "Age at menarche (years)"
    time   = "Time relative to Menarche (years)"
    pbf    = "Percent Body Fat"
  ;
  run;

*******************************************************************;

title2 "1. Random Intercept and Slopes before and after Menarche";
title3 "Tables 8.6 and 8.7";

proc mixed data = A method = reml noclprint = 10 covtest;
     class id;
     model pbf = time time_0 / s chisq;
     random intercept time time_0 / subject = id type = un g gcorr;
     run;

*******************************************************************;

title2 "2. R  = Exponential Correlation Structure + a diagonal";
title3 "Tables 8.9 and 8.10";

proc mixed data = A method = reml noclprint = 10 covtest asycov asycorr;

     class id;
     model pbf = time time_0 / s chisq;
     repeated / subject = id type = sp(exp)(time) local;
     random intercept / subject = id type = un g;
     run;

*******************************************************************;

title2 "3. R has Gaussian Correlation Structure + a diagonal";

proc mixed data = A method = reml noclprint = 10 covtest;
     class id;
     model pbf = time time_0 / s chisq;
     repeated / subject = id type = sp(gau)(time)  local;
     random intercept  / subject = id type = un g;
     run;

