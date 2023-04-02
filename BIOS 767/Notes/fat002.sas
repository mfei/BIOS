%let progname = fat002.sas;
%let pdfout   = fat002.pdf;

* input: fat.dat
* output:
* xref:
* does:
        MIT Growth and Development Study, mixed models
        Hypothesis testing for G

*******************************************************************;
title "&progname: MIT Growth and Development Study, mixed models";

filename INF  "fat.dat";
ods pdf file = "&pdfout";
*******************************************************************;

data A;
  infile INF firstobs = 2;
  input id age agemen time pbf;
  time_0 = max(time,0);

  label
    id     = "Subject ID"
    age    = "Current Age (years)"
    agemen = "Age at menarche (years)"
    time   = "Time relative to Menarche (years)"
    pbf    = "Percent Body Fat"
  ;
  run;

*******************************************************************;

title2 "0. Random Intercept";

proc mixed data = A method = reml noclprint = 10;
     class id;
     model pbf = time  time * time / s ;
     random intercept 
          / subject = id type = un g v = 20, 30 vcorr = 20, 30;
     run;

*******************************************************************;

title2 "1. Random Intercept and Slope";

proc mixed data = A method = reml noclprint = 10;
     class id;
     model pbf = time  time * time / s ;
     random intercept time 
          / subject = id type = un g v = 20, 30 vcorr = 20, 30;
     run;

*******************************************************************;

title2 "2. Random quadratic ";

proc mixed data = A method = reml noclprint = 10;
     class id;
     model pbf = time time * time / s;
     random intercept time time * time 
          / subject = id type = un g v = 20, 30 vcorr = 20, 30;
     run;

